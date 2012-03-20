
;;;; To run this app:
;;;;   (playpen:run-app 'playpen-tests:photos)

(in-package :playpen-tests)

(defclass photos (window time-consumer)
  (;;(filenames :initarg :filenames)
   (contents-area :initform 0 :accessor contents-area)
   (zoom :accessor zoom :initform 2.0)
   (zoom-target :accessor zoom :initform 0.2)
   (offset :accessor offset :initform #c(0 0))
   (offset-target :accessor offset-target :initform nil)
   (goto :accessor goto :initform nil)
   (objects :initform nil
            :accessor objects-of)
   (new-objects :initform nil           ; Something of a temporary hack.
                :accessor new-objects-of)
   (finished-loading :initform nil :accessor finished-loading))
  (:default-initargs
   :application-name "Photos?"
    :initial-width (print (* 0.9 (display-width)))
    :initial-height (* 0.9 (display-height))))

(defclass photo ()
  ((coordinate :initarg :coordinate :accessor coordinate)
   (target :initarg :target :initform nil :accessor target)
   (image :initarg :image :accessor image)))

(defun random-photo-coordinate (window)
  (scalec
   (complex (aspect-ratio window) 1.0)
   (* (cis (random (* 2 pi)))
      (random
       (+ 64.0
          (/ (sqrt (/ (contents-area window)
                      (aspect-ratio window)))
             pi))))))

(defun shuffle-photos (w)
  (with-slots (objects) w
    (dolist (object objects)
      (setf (target object) (random-photo-coordinate w)))
    (setf objects (alexandria:shuffle objects))))

(defclass new-photo-event (window-event)
  ((photo :reader photo-of :initarg :photo)))

(defclass finished-photoscan (window-event) ())

(defun load-photos (app)
  (let ((files (directory #p"/home/hefner/ptest2/*.*")))
    (dolist (filename files)
      (ignore-errors
        (send app
              (make-instance 'new-photo-event
                             :window app
                             :photo
                             (make-instance 'photo
                                            :coordinate 0
                                            :image
                                            (read-image-file filename)
                                            #+nil
                                            (read-jpeg-file
                                             filename
                                             :configurator
                                             (lambda (j)
                                               (jlo-set-downscale j 2))))))))
    (send app (make-instance 'finished-photoscan
                             :window app))))

(defmethod initialize-instance :after ((app photos) &rest initargs)
  (declare (ignore initargs))
  (setf *random-state* (make-random-state t))
  (bordeaux-threads:make-thread
   (lambda () (load-photos app))))

(defmethod handle-event ((app photos) (event finished-photoscan))
  (setf (finished-loading app) t))

(defmethod handle-event ((window photos) (event new-photo-event))
  (push (photo-of event) (new-objects-of window))
  (animate))

(defparameter *zoom-step* 1.2)

(defmethod handle-event ((window photos) (event button-press))
  (with-slots (zoom-target goto objects) window
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
       (clear-screen #(0.07 0.07 0.07 0.65))
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
       (setf zoom (expt-approach zoom zoom-target)
             offset (expt-approach offset
                                   (or offset-target offset)
                                   :threshold 1))

       ;; Experiment: rate limit the introduction of new objects:
       (loop repeat 1
             as object = (pop (new-objects-of window))
             while object do
             (incf (contents-area window) (area (image object)))
             (setf (target object) (random-photo-coordinate window))
             (animate)
             (use-texture (image object))
             (push object objects))

       ;; Print the time it took to load all the images from disk.
       (when (and (eql (finished-loading window) t)
                  (null (new-objects-of window)))
         (print (list :time (relative-time window)))
         (setf (finished-loading window) :reported))

       ;; Draw objects:
       (dolist (object objects)
         ;; Move object.
         (when (target object)
           (setf (coordinate object)
                 (expt-approach (or (coordinate object)
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
         (gl:check-error)))

      ;; Quick hack, remove.
      #+NIL
      (error (c)
        (print c)
        (sleep 1)))))





