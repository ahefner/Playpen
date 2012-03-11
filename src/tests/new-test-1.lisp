;;;; Test 1 - basic test of OpenGL rendering and multiple windows.

;;;; Copyright (c) 2008, Andy Hefner <ahefner@gmail.com>

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

(defun draw-circle (x y radius &optional (segments 64))
  (gl:begin :triangle-fan)
  (loop with d-theta = (/ pi 0.5 segments)
        for theta from 0.0 by d-theta
        repeat segments
        do (gl:vertex (+ x (* radius (sin theta)))
                      (+ y (* radius (cos theta)))))
  (gl:end))

(defun run-new-test-1 ()
  (pwin:initialize-display)

  (let ((windows nil)
        (total-windows 0)
        (image (playpen::read-gif "/home/hefner/cl/playpen/src/tests/angry.gif"))
        (x 0.1)
        (y 0.1)
        (radius 0.2))

    (flet ((paint (window)
             (with-graphics-context (window)
               (clear-screen #(0.3 0.0 0.0 1.0))

               (use-pixel-projection)
               (gl:scale (width *window*)
                         (width *window*)
                         1.0)

               (gl:color 1.0 1.0 1.0)
               (draw-circle x y radius)
               (gl:color 1.0 0.3 0.1)
               (draw-circle x y (* 0.94 radius))

               ;; Test projection back to screen coordinates:
               (multiple-value-bind (x y) (transform-to-screen x y 0.0)
                 (use-pixel-projection)
                 (use-texture image)
                 (gl:color 1.0 1.0 1.0 1.0)
                 (gl:begin :quads)
                 (let ((r 100))
                   (playpen::%draw-rect (- x r) (- y r) (+ x r) (+ y r)))
                 (gl:end))))

           (create-test-window ()
             (incf total-windows)
             (car
              (push (pwin:create-window
                     :application-name "Multiwindow Test"
                     :width  (floor (pwin:display-width  pwin:*display*) 2)
                     :height (floor (pwin:display-height pwin:*display*) 2))
                    windows)))

           (delete-test-window (window)
             (pwin:destroy-window window)
             (setf windows (delete window windows))))

      (create-test-window)

      (unwind-protect
           (loop while windows
                 with needs-repaint = t
                 with wait-for-events = nil
                 as event = (pwin:get-event (if wait-for-events nil 0))
                 do
                 (setf wait-for-events nil)
                 (typecase event
                   (pwin:timeout
                    (setf wait-for-events t)
                    (when needs-repaint
                      (dolist (window windows) (paint window))
                      (setf needs-repaint nil)))
                   (pwin:expose (paint (pwin:event-window event)))
                   (pwin:motion
                    (setf x (/ (pwin:event-x event)
                               (width (pwin:event-window event)))
                          y (/ (pwin:event-y event)
                               (width (pwin:event-window event))))
                    (setf needs-repaint t))
                   (pwin:button-release
                    (cond
                      ((not (zerop (logand (pwin:event-button-state event)
                                           pwin:+pointer-button-1+)))
                       (pwin:set-title (create-test-window)
                                  (format nil "Window ~A" total-windows)))
                      ((not (zerop (logand (pwin:event-button-state event)
                                           pwin:+pointer-button-3+)))
                       (delete-test-window (pwin:event-window event)))))
                   (pwin:close-request
                    (delete-test-window (pwin:event-window event)))))
        (dolist (window windows)
          (pwin:destroy-window window))))))
