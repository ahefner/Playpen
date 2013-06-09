;;;; Test 1a - basic test of OpenGL rendering and multiple windows.
;;;; This version uses the Playpen event loop.

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

;;; To run this app:
;;;  (playpen:run-app 'playpen-tests:multiwindow-test)

(defclass multiwindow-test (window)
  ((coordinate :initform #c(0.5 0.5) :allocation :class)
   (my-coordinate :initform nil)
   (r :initform 0.4)
   (background :initarg :background :initform #(0.3 0.1 0.1 1.0))
   (image
    :allocation :class
    :initform (read-image-file (builtin-asset-path "pepper.png")))
   (background-image
    :allocation :class
    :initform (read-image-file (builtin-asset-path "test-bg.jpg"))))
  (:default-initargs
   :application-name "Playpen Multiwindow Test"))

(defmethod handle-event ((window multiwindow-test) (event expose))
  (with-slots (coordinate my-coordinate r image background background-image) window
    (setf my-coordinate coordinate)
    (with-graphics-context (window)
      ;;(clear-screen background)
      (clear-screen #(0 0 0 0))
      (use-pixel-projection)
      (gl:scale (width *window*)
                (width *window*)
                1.0)
      (gl:color 1.0 1.0 1.0 1.0)

      (use-texture background-image)
      (gl:begin :quads)
      (playpen::%draw-rect 0
                           (complex
                            (max 1.0 (/ (aspect-ratio window)))
                            (max 1.0 (/ (aspect-ratio window)))))
      (gl:end)

      (use-texture image)
      (gl:begin :quads)
      (playpen::%draw-rect (- coordinate (complex (* r (aspect-ratio image)) r))
                           (+ coordinate (complex (* r (aspect-ratio image)) r)))
      (gl:end))))

(defmethod handle-event ((window multiwindow-test) (event motion))
  (with-slots (coordinate) window
    (setf coordinate (* (coordinate event)
                        (/ (width window))))))

(defun random-bg-color ()
  (vector (random 0.6) (random 0.6) (random 0.6) 1.0))

;;; Respond to left button clicks by opening a new window.
;;; Respond to right button clicks by closing this window.
(defmethod handle-event ((a multiwindow-test) (event button-release))
  (case (event-button event)
    (1 (run-app 'multiwindow-test
                :background (random-bg-color)))
    (3 (destroy-window *window*))))

;;; If our last rendered coordinate differs from the global
;;; coordinate, ANIMATING returns true, causing the event loop to send
;;; us another expose event.
(defmethod animating ((a multiwindow-test))
  (with-slots (coordinate my-coordinate) a
    (not (eql coordinate my-coordinate))))
