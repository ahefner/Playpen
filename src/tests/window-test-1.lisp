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

(in-package :pwin-tests)

(defun draw-circle (x y radius &optional (segments 64))
  (gl:begin :triangle-fan)
  (loop with d-theta = (/ pi 0.5 segments)
        for theta from 0.0 by d-theta
        repeat segments
        do (gl:vertex (+ -1.0 (*  2.0 x) (* radius (sin theta)))
                      (+  1.0 (* -2.0 y) (* radius (cos theta)))))
  (gl:end))

(defun run-test-1 ()
  (pwin:initialize-display)

  (let ((windows nil)
        (total-windows 0)
        (x 0.5)
        (y 0.5)
        (radius 0.2))
    (flet ((paint (window)
             (gl:matrix-mode :modelview)
             (gl:load-identity)
             (begin-paint window)
             (gl:clear-color 0.3 0.0 0.0 0.3)
             (gl:clear :color-buffer-bit)
             (gl:disable :texture-2d)
             (gl:disable :blend)
             (gl:color 1.0 1.0 1.0)
             (draw-circle x y radius)
             (gl:color 1.0 0.3 0.1)
             (draw-circle x y (* 0.94 radius))
             (end-paint window))
           (create-test-window ()
             (incf total-windows)
             (car
              (push (pwin:create-window :application-name "Multiwindow Test"
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
                   (timeout
                    (setf wait-for-events t)
                    (when needs-repaint
                      (dolist (window windows) (paint window))
                      (setf needs-repaint nil)))
                   (expose (paint (event-window event)))
                   (motion
                    (setf x (/ (event-x event) (window-width  (event-window event)))
                          y (/ (event-y event) (window-height (event-window event))))
                    (setf needs-repaint t))
                   (button-release
                    (cond
                      ((not (zerop (logand (event-button-state event)
                                           +pointer-button-1+)))
                       (set-title (create-test-window)
                                  (format nil "Window ~A" total-windows)))
                      ((not (zerop (logand (event-button-state event)
                                           +pointer-button-3+)))
                       (delete-test-window (event-window event)))))
                   (close-request
                    (delete-test-window (event-window event)))))
        (dolist (window windows)
          (pwin:destroy-window window))))))
