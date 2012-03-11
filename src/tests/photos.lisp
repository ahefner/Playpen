
;;;; To run this app:
;;;;   (playpen:run-app 'playpen-tests:photos)

(in-package :playpen-tests)

(defclass photos (window time-consumer)
  (;;(filenames :initarg :filenames)
   (zoom :accessor zoom :initform 1.0)
   (zoom-target :accessor zoom :initform 1.0)
   (offset :accessor offset :initform #c(0 0))
   (offset-target :accessor offset-target :initform nil)
   (goto :accessor goto :initform nil)
   (objects :initform nil
            :allocation :class)
   (marker :initform (read-image-file
                      (builtin-asset-path "angry.gif"))
           :allocation :class))
  (:default-initargs
   :application-name "Photos?"))

(defclass photo ()
  ((coordinate :initarg :coordinate :accessor coordinate)
   (target :initform nil :accessor target)
   (image :initarg :image :accessor image)))

(defparameter *max-x* 2000)
(defparameter *max-y* 1000)

(defun shuffle-photos (w)
  (with-slots (objects) w
    (dolist (object objects)
      (setf (target object)
            (complex
             (birand *max-x*)
             (birand *max-y*))))
    (setf objects (alexandria:shuffle objects))))

(defmethod initialize-instance :after ((a photos) &rest initargs)
  (declare (ignore initargs))
  (setf *random-state* (make-random-state t))
  (with-slots (objects) a
    (unless objects
      (setf objects
            (mapcar
             (lambda (filename)
               (make-instance 'photo
                              :coordinate nil
                              :image (read-image-file filename)))
             (append
              #+NIL (directory #p"/home/hefner/ptest/*.png")
              (directory #p"/home/hefner/ptest2/*.*")
              #+NIL (directory #p"/home/hefner/ptest3/*.*")
              #+NIL (directory "/home/hefner/*.png"))))
      (shuffle-photos a))))

(defparameter *zoom-step* 1.2)

(defmethod handle-event ((window photos) (event button-press))
  (with-slots (zoom-target goto objects animating) window
    (case (event-button event)
      (1 (setf goto (coordinate event)))
      (3 (shuffle-photos window))
      (4 (setf zoom-target (* zoom-target *zoom-step*)))
      (5 (setf zoom-target (/ zoom-target *zoom-step*))))
    (setf zoom-target (clamp 0.1 zoom-target 10.0)))
  (animate))

(defmethod handle-event ((window photos) (event motion))
  (animate))

(defmethod handle-event ((window photos) (event expose))
  (with-graphics-context (window)
    (handler-case                      ; XXX
     (with-slots (objects zoom zoom-target offset offset-target goto marker) window
       ;; Setup for display.
       (clear-screen #(0.1 0.1 0.1 0.85))
       (use-centered-pixel-projection)
       (gl:scale zoom zoom 1.0)
       (gl:translate (- (realpart offset))
                     (- (imagpart offset))
                     0.0)
       ;;(gl:rotate 15.0 0.0 0.0 1.0)

       ;; Update zoom and offset. Need translations established above.
       (when goto
         (setf offset-target (transform-from-screen goto)
               goto nil))
       (setf zoom (exponential-approach zoom zoom-target)
             offset (exponential-approach offset
                                          (or offset-target offset)
                                          :threshold 1))
       ;; Draw objects:
       (let ((p nil))
        (dolist (object objects)
          ;; Move object.
          (when (target object)
            (setf (coordinate object)
                  (exponential-approach (or (coordinate object)
                                            (target object))
                                        (target object)
                                        :threshold 1)))
          ;; Draw object.
          (use-texture (image object))
          (gl:tex-env :texture-env :texture-env-mode :modulate)
          ;;(gl:tex-env :texture-env :texture-env-color #(0 0 0 0))
          ;;(gl:color 1.0 0.4 0.2 1.0)
          ;;(gl:color 1.0 1.0 1.0 1.0)
          (gl:color 0.6 0.6 0.6 0.6)

          (when (and
                 (window-pointer-position window)
                 (rectangle-contains-p
                  (transform-to-screen
                   (coordinate object))
                  (transform-to-screen
                   (+ (coordinate object)
                      (dimensions (image object))))
                  (window-pointer-position window)))
            (gl:color 1.0 1.0 1.0 1.0))
          (gl:begin :quads)
          (playpen::%draw-rect (coordinate object)
                               (+ (coordinate object)
                                  (dimensions (image object))))
          (gl:end)
          (gl:check-error)
          #+NIL
          (unless p
            (setf p (transform-to-screen (coordinate object)))))

        ;; Test - show pointer
        #+NIL
        (when (window-pointer-position window)
          (print (list :offset offset
                       :pointer-at (transform-from-screen
                                    (window-pointer-position window))))
          (use-texture marker)
          (gl:color 1 1 1 1)
          (let ((p (transform-from-screen
                    (window-pointer-position window))))
            (gl:begin :quads)
            (playpen::%draw-rect (- p (* 0.5 (dimensions marker)))
                                 (+ p (* 0.5 (dimensions marker))))
            (gl:end)))

        ;; Highlight selected object.
        #+NIL
        (when p
          ;;(setf p (or (window-pointer-position window) p))
          (use-pixel-projection)
          (use-texture marker)
          (gl:color 1.0 1.0 1.0 1.0)
          (gl:begin :quads)
          (playpen::%draw-rect p (+ p (dimensions marker)))
          (gl:end)
          (gl:check-error))))
      #+NIL
      (error (c)
        (print c)
        (sleep 1)))))





