;;;; Lisp interface to portable windowing layer.

;;;; Copyright (c) 2008-2012, Andy Hefner <ahefner@gmail.com>

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

(in-package :pwin)

;;;; OpenGL contexts

;;; GLX allows using one global OpenGL context across all windows, for
;;; the lifetime of the app. Native Mac and Windows are more picky - I
;;; think you need one context per window, and contexts are
;;; invalidated when a window is resized. Since my "portable" layer
;;; only has an implementation for X, I'll just make one global
;;; context. Having an explicit context object gets us most of the way
;;; toward making this work, when it's finally needed.

(defvar *global-opengl-context* nil)

(defclass opengl-context ()
  ((alive-p :initform t
            :reader context-alive-p)
   (resource-map :initform (make-hash-table)
                 :reader resource-map)
   (attributes :reader context-attributes
               :initform (make-hash-table :test 'equal))))

;;; This one is bound dynamically by WITH-GRAPHICS-CONTEXT. Don't use
;;; the other one.
(defvar *gl-context*)

(defun assert-gl-context ()
  (assert (not (null *gl-context*))))

(defun asserted-gl-context ()
  (assert-gl-context)
  *gl-context*)

(defun context-attribute (key &optional (context *gl-context*))
  (assert (not (null context)))
  (gethash key (context-attributes context)))


(defclass opengl-resource ()
  ((context :initform nil
            :initarg :context
            :reader opengl-resource-context)))

(defun assert-ownership (resource)
  (eql (asserted-gl-context)
       (opengl-resource-context resource)))

(defun attach-resource (context resource &optional (key resource))
  (cond
    ;; Already attached to context?
    ((eql context (opengl-resource-context resource))
     (values))
    ;; Not attached to a context?
    ((null (opengl-resource-context resource))
     (with-slots (resource-map) context
       (setf (gethash key resource-map) resource)))
    ;; Must not be attached to a different context.
    (t (error "Resource ~A is already attached to context ~A"
              resource
              context))))

(defun remove-resource (context resource &optional (key resource))
  (assert (eql context (slot-value resource 'context)))
  (with-slots (resource-map) context
    (remhash key resource-map)
    (setf (slot-value resource 'context) nil)))

;;;; Display/init

(cffi:defcstruct display
  (width  :int)
  (height :int)
  (alphap :int))

(cffi:defcvar ("dpy_info" *display*) display)

(defun display-width (&optional (display-ptr *display*))
  (cffi:foreign-slot-value display-ptr 'display 'width))

(defun display-height (&optional (display-ptr *display*))
  (cffi:foreign-slot-value display-ptr 'display 'height))

(defun display-supports-compositing-p (&optional (display-ptr *display*))
  (not (zerop (cffi:foreign-slot-value display-ptr 'display 'alphap))))

(cffi:defcfun (%initialize-display "pwin_init")
    (:wrapper :int)
  (name :string))

(defvar *display-initialized* nil)

(defun initialize-display (&optional (display-spec (uiop/os:getenv "DISPLAY")))
  (unless *display-initialized*
    (unless (zerop (%initialize-display display-spec))
      (error "Unable to initialize display ~A" display-spec))
    (unless *global-opengl-context*
      (setf *global-opengl-context* (make-instance 'opengl-context)))
    (setf *display-initialized* t)))

;;;; Windowing

(cffi:defcstruct window
  (width  :int)
  (height :int)
  (user   :pointer)
  #| Past this point appears data private to window system interface.
  Lisp only deals in pointers to these structures, so it's okay to
  simply omit it. |#)

(defvar *window-map*
  (make-hash-table :test 'equal
                   :synchronized t))

(defun all-windows ()
  (loop for window being the hash-values of *window-map* collect window))

(defun pointer->window (window-pointer)
  (gethash (cffi:pointer-address window-pointer)
           *window-map*))

(defgeneric window->pointer (windoid))

(defun %window-width  (window)
  (cffi:foreign-slot-value window 'window 'width))

(defun %window-height (window)
  (cffi:foreign-slot-value window 'window 'height))

(defun %window-user-pointer  (window)
  (cffi:foreign-slot-value window 'window 'user))

(cffi:defcenum window-type
  (:frame 1))

(cffi:defcfun (%create-window "create_window") :pointer
  (type window-type)
  (application-name :string)
  (width  :int)
  (height :int))

(cffi:defcfun (%destroy-window "destroy_window") :void
  (window :pointer))

(cffi:defcfun (%begin-paint "window_begin_paint") :void
  (window :pointer))

(cffi:defcfun (%end-paint "window_end_paint") :void
  (window :pointer))

(cffi:defcfun (%scribble-window "scribble_window") :void
  (window :pointer))

(cffi:defcfun (%set-title "window_set_title") :void
  (window :pointer)
  (title  :string))

(defclass window ()
  ((window-pointer
    :reader window->pointer
    :initarg :window-pointer)
   (alive-p
    :reader window-alive-p
    :initform t)
   (initial-width
    :reader initial-width
    :initarg :initial-width)
   (initial-height
    :reader initial-height
    :initarg :initial-height)
   (application-name
    :reader application-name
    :initarg :application-name)
   (title
    :reader title
    :initform nil
    :initarg :title)
   (window-type
    :reader window-type
    :initform nil
    :initarg :window-type)
   (pointer-position
    :reader window-pointer-position
    :initform nil)
   (button-state
    :reader window-button-state
    :initform 0)
   (lock
    :reader window-lock
    :initform (bordeaux-threads:make-lock "window lock"))
   (graphics-context
    :reader graphics-context
    :initarg :graphics-context
    :initform nil))

  (:default-initargs
   :initial-width 700
   :initial-height 600
   :application-name "Playpen Application"
   :window-type :frame))

(defgeneric window-width (window)
  (:method (window) (%window-width (window->pointer window))))

(defgeneric window-height (window)
  (:method (window) (%window-height (window->pointer window))))

(defun create-window (&key
                      width
                      height
                      type
                      application-name
                      (title application-name)
                      (class 'window)
                      (window-initargs nil))
  (let* ((window (apply #'make-instance
                        class
                        :graphics-context *global-opengl-context*
                        window-initargs))
         (%win (%create-window
                (or type (window-type window))
                (or application-name (application-name window))
                (round (or width (initial-width window)))
                (round (or height (initial-height window))))))
    (setf (slot-value window 'window-pointer) %win
          (gethash (cffi:pointer-address %win) *window-map*) window)
    (set-title window (or title
                          (title window)
                          (application-name window)))
    window))

(defun begin-paint (window)
  (%begin-paint (window->pointer window)))

(defun end-paint (window)
  (%end-paint   (window->pointer window)))

(defun %window-alive-p (window)
  (check-type window window)
  (if (gethash (cffi:pointer-address
                (window->pointer window))
               *window-map*)
      t
      nil))

(defun window-composited-p (window)
  (declare (ignore window))
  (display-supports-compositing-p *display*))

(defun set-title   (window title)
  (%set-title (window->pointer window) title))

(defun destroy-window (window)
  (check-type window window)
  (bordeaux-threads:with-lock-held ((window-lock window))
    (setf (slot-value window 'alive-p) nil)
    (when (%window-alive-p window)
      (remhash (cffi:pointer-address
                (window->pointer window))
               *window-map*)
      (%destroy-window (window->pointer window)))))

(defun destroy-all-windows ()
  (dolist (window (all-windows))
    (destroy-window window))
  (unless (zerop (hash-table-count *window-map*))
    (warn "*window-map* not empty after destroying all windows?")
    (clrhash *window-map*)))

(defun number-of-windows ()
  (hash-table-count *window-map*))

;;;; Events

(cffi:defcenum event-type
  (:motion 1)
  :pointer-enter
  :pointer-exit
  :button-press
  :button-release
  :key-press
  :key-release
  :mapped
  :unmapped
  :close-request
  :expose
  :resized
  :io-ready
  :timeout)

(cffi:defcstruct event
  (type event-type)
  (window        :pointer)
  (x             :unsigned-int)
  (y             :unsigned-int)

  (button        :unsigned-int)
  (button-state  :unsigned-int)
  (old-button-state :unsigned-int)

  (modifier-mask :unsigned-int)

  (native-code   :unsigned-int)
  (native-keysym :unsigned-int)
  (keysym        :unsigned-int)
  (unicode       :long)

  (width         :unsigned-int)
  (height        :unsigned-int)
  (old-width     :unsigned-int)
  (old-height    :unsigned-int))

;;; TODO: It would be nice to define which fields are set by which event type,
;;; and let the slot readers check this and signal an error otherwise.

(defgeneric initialize-event-from-c (lisp-event c-event)
  (:method (lisp-event c-event)
    (declare (ignore c-event))
    lisp-event))

(defmacro define-event-property
    (name (&rest slot-specs) )
  `(progn
     (defclass ,name ()
       (,@(mapcar
           (lambda (slot-spec)
             (destructuring-bind (slot-name &optional (translate-fn 'identity))
                 (if (symbolp slot-spec)
                     (list slot-spec)
                     slot-spec)
               (declare (ignore translate-fn))
                 `(,slot-name :reader ,(suffix 'event- slot-name)
                              :initarg ,(keywordify slot-name))))
           slot-specs)))
     (defmethod initialize-event-from-c :after ((lisp-event ,name) c-event)
       ,@(mapcar
           (lambda (slot-spec)
             (destructuring-bind (slot-name &optional (translate-fn 'identity))
                 (if (symbolp slot-spec) (list slot-spec) slot-spec)
                 `(setf (slot-value lisp-event ',slot-name)
                        (,translate-fn
                         (cffi:foreign-slot-value c-event
                                                  'event
                                                  ',slot-name)))))
           slot-specs))))

(define-event-property event-window-trait
    ((window pointer->window)))

(define-event-property event-coordinate-trait (x y))
(define-event-property event-modifier-trait (modifier-mask))

(define-event-property event-keysym-trait
    (native-code
     native-keysym
     (keysym  (lambda (code) (gethash code *keysym-map*)))
     (unicode (lambda (code) (and (>= code 0) (code-char code))))))

(define-event-property event-dimension-trait (width height))
(define-event-property event-old-dimension-trait (old-width old-height))
(define-event-property event-button-state-trait (old-button-state button-state))
(define-event-property event-button-trait (button))

(defclass event () ())

(defclass window-event (event event-window-trait)
  ())

(defclass key-event
    (window-event
     event-modifier-trait
     event-keysym-trait)
  ())

(defclass key-press   (key-event) ())
(defclass key-release (key-event) ())

(defclass pointer-event
    (window-event
     event-coordinate-trait
     event-modifier-trait
     event-button-state-trait)
  ())

(defclass pointer-enter  (pointer-event) ())
(defclass pointer-exit   (pointer-event) ())
(defclass motion         (pointer-event) ())

(defclass button-event
    (pointer-event
     event-button-trait)
  ())

(defclass button-press   (button-event)  ())
(defclass button-release (button-event)  ())

(defclass expose
    (window-event
     event-coordinate-trait
     event-dimension-trait)
  ())

(defclass mapped        (window-event) ())
(defclass unmapped      (window-event) ())
(defclass close-request (window-event) ())

(defclass resized
    (window-event
     event-dimension-trait
     event-old-dimension-trait)
  ())

(defclass io-ready (event) ())

(defclass timeout (event) ())

(defun event-keyword->class (type-keyword)
  ;; This is bullshit. If CFFI let us use arbitrary symbols for enums, it
  ;; wouldn't be necessary.
  (ecase type-keyword
    (:motion         'motion)
    (:pointer-enter  'pointer-enter)
    (:pointer-exit   'pointer-exit)
    (:button-press   'button-press)
    (:button-release 'button-release)
    (:key-press      'key-press)
    (:key-release    'key-release)
    (:mapped         'mapped)
    (:unmapped       'unmapped)
    (:close-request  'close-request)
    (:expose         'expose)
    (:resized        'resized)
    (:io-ready       'io-ready)
    (:timeout        'timeout)))

(defun translate-c-event (c-event-pointer)
  (initialize-event-from-c
   (make-instance (event-keyword->class (c-event-type c-event-pointer)))
   c-event-pointer))

(defun c-event-type (event) (cffi:foreign-slot-value event 'event 'type))

(defconstant +pointer-button-1+     1)
(defconstant +pointer-button-2+     2)
(defconstant +pointer-button-3+     4)
(defconstant +pointer-wheel-up+     8)
(defconstant +pointer-wheel-down+   16)
(defconstant +pointer-wheel-left+   32)
(defconstant +pointer-wheel-right+  64)

(cffi:defcfun (%%get-event-blocking "get_event_blocking") :void
  (event-out :pointer))

(cffi:defcfun (%%get-event-deadline "get_event_with_deadline") :void
  (event-out :pointer)
  (deadline :long-long))

(defun %get-event (event-out &optional deadline)
  (if deadline
      (%%get-event-deadline event-out deadline)
      (%%get-event-blocking event-out)))

(defun update-window-properties (event)
  (typecase event
    (pointer-event
     (with-slots (pointer-position button-state)
         (event-window event)
       (setf pointer-position (complex (event-x event)
                                       (event-y event))
             button-state (event-button-state event))))))

(defun get-event  (&optional deadline)
  ;; Use catch/throw here as a workaround for an SBCL quirk where
  ;; returning from with-alien causes us to leak stack space. Doing
  ;; the NLX appears to fix the stack pointer up.
  ;; [Fixed in SBCL years ago, but I'll leave it in anyway.]
  (catch 'return-event
    (loop
     (cffi:with-foreign-object (event 'pwin::event)
       (%get-event event deadline)
       (let ((ev (translate-c-event event)))
         (unless (and (typep ev 'event-window-trait)
                      (null (event-window ev)))

           ;; Update window properties before returning event.
           (update-window-properties ev)

           (throw 'return-event ev)))))))

(cffi:defcfun "usectime" :long-long)

;;;; Define and load the library

(cffi:define-foreign-library libpwin
  (t (:default "libpwin")))

(let ((cffi:*foreign-library-directories*
       (list
        (merge-pathnames
         (make-pathname :directory '(:relative :back))
         (make-pathname :name nil
                        :type nil
                        :version nil
                        :defaults #.(or *compile-file-truename*
                                        *load-truename*))))))
  (cffi:load-foreign-library 'libpwin))

;;;; Misc functions

;;;; I feel like I should make this work, but I'm skeptical there's
;;;; anything useful to do with it outside the world of iOS devices,
;;;; where this code will never run anyway.  Personally, if my
;;;; monitor's DPI happened to be 30% higher, I'd probably just move
;;;; it that much closer to me.

(defun display-dpi (&optional (display-ptr *display*))
  72)

;;; FIXME: Probably misguided.
#+unix (cffi:defcfun (getproc "glXGetProcAddress") :pointer (name :string))
(setf cl-opengl-bindings:*gl-get-proc-address* 'getproc)

(cffi:defcfun reset-watched-fds :void)

(cffi:defcenum watch-type
  (:read 0)
  (:write 1)
  (:exception 2))

(cffi:defcfun watch-fd :void
  (setidx watch-type)
  (fd :int))

(cffi:defcfun check-fd :boolean
  (setidx watch-type)
  (fd :int))
