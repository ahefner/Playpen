;;;; Main event loop, application skeleton.

;;;; Copyright (c) 2012, Andy Hefner <ahefner@gmail.com>

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :playpen)

;;;; ---------------------------------------------------------------
;;;; Application host
;;;; ---------------------------------------------------------------

;;;; Default event handlers:

(defgeneric handle-event (receiver event)
  (:documentation "Handle.. an event."))

(defmethod handle-event ((the window) event)
  (declare (ignore the event)))

(defmethod handle-event ((a window) (event close-request))
  (destroy-window a))

(defmethod handle-event ((a window) (event expose))
  (with-graphics-context (a)
    (clear-screen #(0.2 0.2 0.2 1.0))))

(defgeneric animating (object)
  (:documentation "Returns true if object is in the process of an animation.")
  (:method (object) nil))

(defgeneric wakeup-time (object)
  (:documentation "Microsecond time at which a window wants to be woken up, or NIL.")
  (:method (object) nil))

(defgeneric event-loop-hook (window)
  (:documentation "Called on each window, after each pass through the event loop.")
  (:method (object) nil))

;;;; Event loop:

(defparameter *event-loop-break* nil
  "Escape hatch to interrupt a running event loop.")

(defun call-with-*window* (window continuation)
  (let ((*window* window))
    (restart-case (funcall continuation)
      (destroy-window ()
        :report "Destroy this window."
        (destroy-window window)))))

(defun dispatch-event-by-type (event)
  (etypecase event
    ;; Ignore timeout events.
    (timeout)
    ;; Window events go through HANDLE-EVENT.
    (window-event
     (call-with-*window*
      (event-window event)
      (lambda () (handle-event *window* event))))
    ;; IO events are handled separately.
    (io-ready
     (dispatch-io-event event))))

(defun dispatch-event (event)
  (tagbody top
   (restart-case (dispatch-event-by-type event)
     (ignore-event ()
       :report "Ignore this event."
       (return-from dispatch-event))
     (retry-event-handler ()
       :report "Retry event handler"
       (go top)))))

(defvar *event-loop-pipe-output* nil)
(defvar *event-loop-pipe-input* nil)

(defun wake-event-loop ()
  (assert (not (null *event-loop-pipe-input*)))
  (fd-write-byte *event-loop-pipe-input* 33))

;;; The purpose of this to support launching applications from outside
;;; the main thread (since the application init needs to happen in the
;;; UI thread). It's also a handy place to hang any events originating
;;; from outside the windowing layer's get-event function which must
;;; be propagated through the event loop. Implementation is a little
;;; hacky.

;;; Don't enqueue things on this directly! Do this:
;;;   (send :event <message>)

;;; This ensures the event loop is woken up, by writing to a pipe
;;; which the IO multiplexer monitors.

;;; Somewhat arbitrarily use SBCL's queues for this, because they're
;;; more efficient than mine.

(defvar *event-loop-queue*
  (sb-concurrency:make-queue :name "Synthetic event queue"))

(defun process-event-loop-queue ()
  (dolist (value (drain :event))
    (etypecase value
      (null (return))
      (function (funcall value))
      (event (dispatch-event value)))))

(defun force-repaint (window)
  (when (window-alive-p window)
   (dispatch-event
    (make-instance 'expose
                   :window window
                   :x 0
                   :y 0
                   :width (width window)
                   :height (height window)))))

(defun continue-event-loop-p (persist-p)
  (and (not *event-loop-break*)
       (or persist-p
           (not (zerop (number-of-windows))))))

(defun check-for-event-loop-exit (persist-p)
  (when (not (continue-event-loop-p persist-p))
    (throw 'event-loop-exit t)))

;;; Delete redundant exposure events. A normal toolkit would have to
;;; grow bounding rectangles, but we always redraw the entire window,
;;; so we can simply discard all but the most recent exposure.
(defun condense-events (events)
  (let ((exposed '()))
    (nreverse
     (delete-if
      (lambda (event)
        (typecase event
          (expose
           (cond
             ((member (event-window event) exposed) t)
             (t (push (event-window event) exposed)
                nil)))
          (event nil)))
      (nreverse events)))))

(defun next-event (&optional deadline)
  (prepare-watched-descriptors)
  (get-event deadline))

(defun pending-events ()
  (loop as event = (next-event 0)
        until (typep event 'timeout)
        collect event))

(defun dispatch-pending-events ()
  (map nil 'dispatch-event
       (condense-events (pending-events))))

(defun event-loop-step (persist-p)
  (let* ((frame-start-time (usectime))
         (frame-interval 16666)
         (next-frame-start (+ frame-start-time frame-interval)))
    ;; Handle events.
    (dispatch-pending-events)
    ;; If the last window just closed, we'll block forever in
    ;; GET-EVENT unless we exit the event loop now.
    (check-for-event-loop-exit persist-p)
    ;; Update animating windows. Not ideal, because unrelated events
    ;; will force animating windows to redraw. Not a problem yet in
    ;; practice, unless you have an app with a really slow expose
    ;; handler (which sounds like a bigger problem).
    (dolist (window (all-windows))
      (when (animating window)
        (force-repaint window)))
    ;; Wait for events, deadlines, or the animating interval.
    (let* ((deadline (reduce (lambda (deadline window)
                               (min-or deadline
                                       (min-or (and (animating window)
                                                    next-frame-start)
                                               (wakeup-time window))))
                             (all-windows)
                             :initial-value nil))
           ;; FIXME: A persist-p event loop will hang here when
           ;; deadline is nil.
           (next-event (next-event deadline)))
      ;; Give each window a chance to check for deadlines, etc.  Run
      ;; it before dispatching the next event, for the sake of
      ;; deadlines being processed before the next event.
      (dolist (window (all-windows))
        (call-with-*window*
         window
         (lambda () (event-loop-hook *window*))))
      (dispatch-event next-event))))

(defvar *event-loop-running* nil)

(defun inner-event-loop (&key persist)
  (assert (eql *event-loop-running* :this-thread))
  (loop while (continue-event-loop-p persist)
        do
        (process-event-loop-queue)      ; Start new apps, etc.
        (force-output *standard-output*)
        (force-output *trace-output*)
        (event-loop-step persist)))

(defun event-loop-init ()

  #+NIL
  (push (lambda ()
          (print (list :dynamic-usage (round (sb-kernel:dynamic-usage) 1024)
                       :bytes-consed-between-gcs (round (sb-ext:bytes-consed-between-gcs) 1024)
                       :gc-run-time sb-ext:*gc-run-time*)))
        sb-ext:*after-gc-hooks*)

  (assert (not *event-loop-running*))
  (initialize-display)
  (unless *event-loop-pipe-output*
    (multiple-value-setq (*event-loop-pipe-output*
                          *event-loop-pipe-input*)
      (osicat-posix:pipe))))

(defun run-event-loop (&key persist)
  (event-loop-init)
  (setf *event-loop-running* :running)
  (unwind-protect
       (let ((*event-loop-running* :this-thread))
         (catch 'event-loop-exit
           (inner-event-loop :persist persist)))
    ;; Cleanup.
    (setf *event-loop-running* nil
          *event-loop-break* nil)))

;;;; IO Dispatch

(defun dispatch-io-event (io)
  (declare (ignore io))
  ;;; This is just a joke of a test:
  ;;(print (list :dispatch-io (check-fd :read 0)))
  #+NIL
  (when (check-fd :read 0)
    (print (list :stdin (fd-read-byte 0))))
  ;; Read wakeup notify
  (when (check-fd :read *event-loop-pipe-output*)
    (fd-read-byte *event-loop-pipe-output*)
    #+NIL
    (print (list :event (fd-read-byte *event-loop-pipe-output*)))))

(defun prepare-watched-descriptors ()
  ;;(watch-fd :read 0)                    ; Silly test code.
  (watch-fd :read *event-loop-pipe-output*))

;;;; FIXME: Need to make sure event loop always runs in the same thread.

(defun run-app (class &rest initargs)
  "Start an application, by instantiating 'class' with the given initargs."
  (ecase *event-loop-running*
    ;; Case 1: Event loop not running. Initialize and run until app(s) exit.
    ((nil)
     (event-loop-init)
     (pwin:create-window :class class :window-initargs initargs)
     (run-event-loop :persist nil))

    ;; Case 2: UI running in another thread. Queue the application start.
    (:running
     (find-class class)
     (send :event (lambda () (apply 'run-app class initargs))))

    ;; Case 3: In the UI thread, can start app immediately.
    (:this-thread
     (pwin:create-window :class class :window-initargs initargs))))

(defun wait-for-event-loop ()
  "Wait until event loop is initialized and running."
  ;; Kludge:
  (loop repeat 60 until *event-loop-running* do (sleep 0.05)
        finally (unless *event-loop-running*
                  (error "Where's the event loop?"))))

(defun start-event-loop ()
  "Start a persistent event loop in a background thread"
  (assert (not *event-loop-running*))
  (bordeaux-threads:make-thread
   (lambda () (run-event-loop :persist t)))
  (wait-for-event-loop))

;;;; A little glue between events and spatial protocol functions:

(defmethod coordinate ((event pwin::event-coordinate-trait))
  (complex
   (event-x event)
   (event-y event)))

(defmethod dimensions ((event pwin::event-dimension-trait))
  (complex
   (event-width event)
   (event-height event)))

(defmethod width ((event pwin::event-dimension-trait))
  (realpart (dimensions event)))

(defmethod height ((event pwin::event-dimension-trait))
  (imagpart (dimensions event)))

;;;; ...

;(defgeneric pressed  (what which))

;(defgeneric released (what which))

;(defgeneric held     (what which))


;;;; Caching of image assets. Assets are automatically reloaded when
;;;; they are modified on disk.

;;;; I imagine using inotify for this eventually, but for now it just
;;;; checks the file-write-date.

(defstruct cached-asset
  truename
  write-date
  computed-value
  (missing-warned nil))

(defun builtin-asset-path (&optional (path ""))
  (asdf:system-relative-pathname
   :playpen
   (merge-pathnames
    path
    (make-pathname :directory '(:relative "assets")))))

(defvar *image-assets* (make-hash-table :test 'equal :synchronized t))

(defun image-asset (name)
  (tagbody
   retry
   (let* ((path (builtin-asset-path name))
          (truename (probe-file path))
          (cached (gethash path *image-assets*))
          (write-date (and truename (file-write-date path))))

     (cond
       ;; File exists:
       (truename
        (cond
          ;; Cache miss:
          ((not cached)
           (let ((value (read-image-file path)))
             (setf (gethash path *image-assets*)
                   (make-cached-asset :truename truename
                                      :write-date write-date
                                      :computed-value value))
             (return-from image-asset value)))

          ;; Cache hit:
          ((and (eql write-date (cached-asset-write-date cached))
                (equal truename (cached-asset-truename cached)))
           (return-from image-asset
             (cached-asset-computed-value cached)))

          ;; Out of date. There's a not small chance that the file
          ;; could be in the process of being re-written, so fail
          ;; gracefully.
          (t
           (let ((new-value (read-image-file path :error nil)))
             (cond
               (new-value
                (setf (gethash path *image-assets*)
                      (make-cached-asset :truename truename
                                         :write-date write-date
                                         :computed-value new-value))
                (return-from image-asset new-value))
               ((not new-value)
                (return-from image-asset
                  (cached-asset-computed-value cached))))))))

       ;; File doesn't exist, but we've cached an old version. Return
       ;; the cached value and warn the user: presumably they're
       ;; shuffling files around, changing symlinks, whatever, and we
       ;; should leave them to it.
       (cached
        (unless (cached-asset-missing-warned cached)
          (warn "Asset ~A disappeared! Using copy cached from ~A"
                name
                (cached-asset-truename cached))
          (setf (cached-asset-missing-warned cached) t))
        (return-from image-asset
          (cached-asset-computed-value cached)))

       ;; File doesn't exist, and we don't have a cached version.
       ;; There's various things we could do, but for now just CERROR.
       ((not cached)
        (cerror "Try again" "Image asset ~A not found" name)
        (go retry))))))

;;;; -------------------------------------------------------------------
;;;; Implement channels protocol for objects managed by the event loop.

;;;; The event loop is a channel named by the symbol :EVENT

;;;; FIXME: Don't use SBCL's queue for this. Use a bounded queue.

(defmethod transact ((type (eql :write)) (channel (eql :event)) proceed fail)
  (declare (ignore fail))
  (funcall
   proceed
   (lambda (message)
     (sb-concurrency:enqueue message *event-loop-queue*)
     (wake-event-loop))))

(defmethod channel-open-p ((channel (eql :event)))
  t)                                    ; Close enough for now.

;;; FIXME: While you're replacing the use of SBCL's queue, do this
;;; right too, so someone flooding the queue doesn't starve the
;;; program. Read the amount in the queue once, return at most the
;;; number of items at that instant (ignoring additional items added
;;; while you were unloading the queue).
(defmethod drain ((channel (eql :event)))
  (loop with result = '()
        do (multiple-value-bind (value nonempty)
               (sb-concurrency:dequeue *event-loop-queue*)
             (if nonempty
                 (push value result)
                 (loop-finish)))
        finally (return (nreverse result))))


;;;; Windows are channels accepting window-event objects only. These
;;;; get delivered by the event loop.
(defmethod transact ((type (eql :write)) (window window) proceed fail)
  (bordeaux-threads:with-lock-held ((window-lock window))
    (if (channel-open-p window)
        (funcall proceed
         (lambda (message) (send :event message)))
        (funcall fail (make-instance 'channel-closed :channel window)))))

(defmethod channel-open-p ((window window))
   (window-alive-p window))

;;;; Object stream for delivering data to windows as events.

(defstruct window-event-channel window constructor lock)

(defmethod channel-open-p ((channel window-event-channel))
  (window-alive-p (window-event-channel-window channel)))

