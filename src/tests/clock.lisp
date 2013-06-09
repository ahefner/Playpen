(in-package :playpen-tests)


(defclass clock (window time-consumer)
  ((hour-hand   :initform nil)
   (minute-hand :initform nil)
   (second-hand :initform nil)
   (utime       :initform (get-universal-time) :accessor utime))
  (:default-initargs
   :application-name "Clock"))

(defun clock-angles ()
  (multiple-value-bind (seconds minutes hours) (get-decoded-time)
    (values (* seconds (/ 360 60))
            (* minutes (/ 360 60))
            (* hours   (/ 360 12)))))

;;; Wake up every second to move the clock hands.
(defmethod wakeup-time ((a clock))
  (* 1000000 (ceiling (pwin:usectime) 1000000)))

(defmethod event-loop-hook ((window clock))
  (let ((now (get-universal-time)))
    (unless (eql now (utime window))
      (animate))
    (setf (utime window) now)))

(defun draw-clock-hand (image angle)
  (use-texture image)
  (gl:push-matrix)
  (gl:rotate angle 0 0 -1)
  (gl:begin :quads)
  (playpen::%draw-rect@ 0 (complex (/ (width image) 1024.0 0.5) -2.0))
  (gl:end)
  (gl:pop-matrix))

(defun draw-clock-face (image)
  (use-texture image)
  (gl:begin :quads)
  (playpen::%draw-rect* -1 1 1 -1)
  (gl:end))

(defmethod handle-event ((this clock) (event expose))
  (with-graphics-context (this)
    (clear-screen (if (window-composited-p *window*)
                      #(0 0 0 0)
                      #(1 1 1 1)))
    (use-graphic-projection)
    (draw-clock-face (image-asset "clock-face.png"))
    (with-slots (second-hand minute-hand hour-hand) this
      (multiple-value-bind (ns nm nh) (clock-angles)
        (setf second-hand (expt-approach second-hand ns :rate 1e-6 :mod 360 :threshold 0.3)
              minute-hand (expt-approach minute-hand nm :rate 0.1  :mod 360 :threshold 0.3)
              hour-hand   (expt-approach hour-hand   nh :rate 0.1  :mod 360 :threshold 0.3)))
      (draw-clock-hand (image-asset "clock-hand-1.png") hour-hand)
      (draw-clock-hand (image-asset "clock-hand-2.png") minute-hand)
      (draw-clock-hand (image-asset "clock-hand-3.png") second-hand))
    (draw-clock-face (image-asset "clock-glass.png"))))
