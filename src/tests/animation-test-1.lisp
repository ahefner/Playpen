;;;; Animation test.

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

(in-package :playpen-tests)

;;;; To run this app:
;;;;   (playpen:run-app 'playpen-tests:animation-test-1)

(defclass time-consumer ()
  ((start-time)
   (current-time :initform nil)
   (last-time    :initform nil)
   (animating    :initform t
                 :accessor animating)))

(defmethod initialize-instance :after ((a time-consumer) &rest args)
  (with-slots (start-time current-time) a
    (setf start-time (pwin:usectime)
          current-time start-time)))

(defun animate (&optional (object *window*))
  (with-slots (animating last-time current-time) object
    (unless animating
      (shiftf last-time current-time (pwin:usectime))
      (setf animating t))))

(defun relative-time (&optional (object *window*))
  (with-slots (current-time start-time animating) object
    (setf animating t)
    (* 1f-6 (- current-time start-time))))

(defun delta-t (&optional (object *window*))
  (with-slots (current-time last-time animating) object
    (setf animating t)
    (* 1f-6 (- current-time last-time))))

(defmethod handle-event :around ((a time-consumer) (during expose))
  (with-slots (current-time last-time animating) a
    (shiftf last-time current-time (pwin:usectime))
    (setf animating nil)
    (call-next-method)))

(defun calculate-exponential-approach (current target rate dt)
  (+ target
     (* (- current target)
        (expt rate dt))))

(defun unwrap-mod (current target mod)
  (setf current (mod current mod)
        target  (mod target mod))
  (let ((d (mod (- target current) mod)))
    (when (> d (* mod 0.5))
      (setf d (- d mod)))
    (setf target (+ current d)))
  (values current target))

(defun expt-approach (current target
                             &key
                             mod
                             (rate 0.01)
                             (snap nil)
                             (threshold (abs (* target 0.01))))
  (setf current (or current target))
  (when mod
    (multiple-value-setq (current target)
      (unwrap-mod current target mod)))
  (if (<= (abs (- target current)) threshold)
      (if snap target current)
      (calculate-exponential-approach current target rate (delta-t))))

(defclass animation-test-1 (window time-consumer)
  ()
  (:default-initargs
   :application-name "Animation Test"))

(defmethod handle-event ((window animation-test-1) (event expose))
  (with-graphics-context (window)
    (with-slots (positions) window
      (clear-screen #(0.06 0.10 0.06 0.6))
      (use-graphic-projection)
      (use-texture (image-asset "pepper.png"))
      (gl:begin :quads)
      (loop with time = (relative-time)
            for i from 0 below 64
            with r = (complex 0.07 -0.07)
            with scale = 0.6
            as p = (+ (* 0.7 (* (+ 0.40 (expt (cos time) 2))
                                (cis (+ (* 0.4 time)
                                        (* i (/ pi 0.5 64))))))

                      (* 0.28 (sin (max 0 (cos (* time 0.73))))
                         (cis (* i (/ pi 0.5 64 1/3))))

                      (* 0.28 (sin (* -7.0 (expt (cos (* 0.31 time)) 3)))
                         (cis (* i (/ pi 0.5 64 1/2)))))
            do
            (playpen::%draw-rect (* scale (- p r))
                                 (* scale (+ p r))))
      (gl:end))))

