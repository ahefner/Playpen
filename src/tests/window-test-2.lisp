;;;; Second test of windowing and graphics

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

(defparameter *texture-surface* nil)
(defparameter *glyph-c* nil)
(defparameter *glyph-l* nil)

(defun opengl-state-init ()
  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
  ;; Set the border color to black, else you get a fringe around images on some drivers.
  (gl:tex-parameter :texture-2d :texture-border-color #(0.0 0.0 0.0 0.0))
  (gl:tex-env :texture-env :texture-env-mode :modulate)
  (gl:check-error))

(defun draw-string (face string left-x baseline-y em-size)
  ;;; If the surface cache and drawing code were actually done, I'd do
  ;;; this differently. This approach has obvious horrible performance
  ;;; implications.
  (let ((glyphs (map 'vector
                     (lambda (char)
                       (let ((glyph (playpen::typeface-ensure-glyph face (char-code char))))
                         (unless (playpen::cached-glyph-leaf glyph)
                           (setf (playpen::cached-glyph-leaf glyph)
                                 (playpen:cache-surface-allocate
                                  *texture-surface*
                                  (playpen::cached-glyph-image glyph)
                                  0 0
                                  (playpen:image-width  (playpen::cached-glyph-image glyph))
                                  (playpen:image-height (playpen::cached-glyph-image glyph))
                                  glyph)))
                         (assert (not (zerop (playpen::sctree-requested-width
                                              (playpen::cached-glyph-leaf glyph)))))
                         glyph))
                     string)))
    (gl:begin :quads)
    (loop with x = left-x
          for glyph across glyphs
          as pixels->em = (float (playpen::cached-glyph-pixels->em glyph))
          as ascender = (playpen::cached-glyph-ascender glyph)
          as descender = (playpen::cached-glyph-descender glyph)
          as offset-x = (playpen::cached-glyph-offset-x glyph)
          as offset-y = (playpen::cached-glyph-offset-y glyph)
          as advance-width = (playpen::cached-glyph-advance-width glyph)
          as lsb = (playpen::cached-glyph-left-side-bearing glyph)
          as x0 = x ;(+ x (* lsb pixels->em em-size)) ; correct?
          as y0 = (- baseline-y (* offset-y pixels->em em-size))
          as char-width = (* (playpen::sctree-requested-width
                              (playpen::cached-glyph-leaf glyph))
                             pixels->em em-size)
          as char-height = (* (playpen::sctree-requested-height
                               (playpen::cached-glyph-leaf glyph))
                              pixels->em em-size)
          as x1 = (+ x0 char-width)
          as y1 = (+ y0 char-height)
          do
          ;;(hef:debugf char-width char-height left-x x (* lsb em-size) x0 x1 y0 y1)
          (playpen::%draw-leaf (playpen::cached-glyph-leaf glyph) x0 y0 x1 y1)
          (incf x (* advance-width pixels->em em-size)))
    (gl:end)))

(defun init-2 ()
  (playpen::realize-texture-surface *texture-surface*)
  (gl:enable :texture-2d)
  (gl:bind-texture :texture-2d (playpen::texture-id *texture-surface*))
  (gl:check-error)

  (opengl-state-init)

  (let* ((surface *texture-surface*)
         (image (playpen::mirrored-surface-image surface))
         (array (playpen:image-matrix-data image))
         (width (array-dimension array 1))
         (height (array-dimension array 0))
         (face (playpen:text-face :serif :italic))
         (glyph-1 (playpen::typeface-ensure-glyph face (char-code #\4)))
         (glyph-2 (playpen::typeface-ensure-glyph face (char-code #\2))))

    (assert (= width (playpen:image-width image)))
    (assert (= height (playpen:image-height image)))

    (playpen::array-fill-rectangle array 0 0 width height 0)

    ;;(playpen::copy-a8-rectangle-to-rgba32-image (array-dimension glyph 1) (array-dimension glyph 0) glyph 0 0 array 0 0)
    ;;(playpen::copy-a8-rectangle-to-rgba32-image (array-dimension glyph-2 1) (array-dimension glyph-2 0) glyph-2 0 0 array 64 20)
    ;;(playpen::mirror-push-rectangle surface 0 0 width height)

    ;;(gl:generate-mipmap-ext :texture-2d)
    ;;(playpen::update-mipmap-region *pattern-array* 0 0 128 128 0 0)
    #+NIL
    (cffi:with-pointer-to-vector-data (pointer (playpen::a8-to-rgba32 glyph #xFFFFFF))
      (gl:tex-sub-image-2d :texture-2d 0 target-x target-y (array-dimension glyph 1) (array-dimension glyph 0) :rgba :unsigned-byte pointer)))
  (gl:check-error))

(defun paint-2 (window offset)
  (begin-paint window)
  (unwind-protect
    (progn
      (gl:check-error)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:scale 1.0 -1.0 1.0)
      (gl:matrix-mode :projection)
      (gl:load-identity)

      (gl:clear-color 0.3 0.3 0.3 0.4)
      (gl:clear :color-buffer-bit)

      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:bind-texture :texture-2d (playpen::texture-id *texture-surface*))
      (gl:matrix-mode :texture)
      (gl:load-identity)
      (gl:scale (/ (playpen:width *texture-surface*)) (/ (playpen:height *texture-surface*)) 1)
      (gl:enable :texture-2d)
      (gl:color 1.0 1.0 1.0 1.0)
      (gl:check-error)

      ;; (gl:begin :quads)

      (loop for step from 140 downto 0
            as step* = (+ step (/ offset 200.0))
            as angle = (* step* (/ pi 0.5 15))
            as size = (* 0.1 (expt 0.97 step))
            as radius = (- 0.8 (/ step 200))
            with glyph = *glyph-c*
            for color in '#1=((1.0 0.5 0.5)
                              (1.0 1.0 0.5)
                              (0.5 0.5 1.0) . #1#)
            as x0 = (- (* radius (cos angle)) (* 0.5 size))
            as y0 = (+ (* radius (sin angle)) (* 0.5 size))
            do
            (loop for color in (list '(0.0 0.0 0.0) color)
                  for offset from 0 by 0.004 do
                  (apply #'gl:color color)
                  (draw-string (playpen:text-face :serif :roman)
                               "This is the text."
                               (+ x0 offset) (- y0 offset) size))

            ;;(playpen::%draw-leaf *glyph-l* x0 y0 (+ x0 size) (- y0 size))
            #+NIL
            (playpen::%draw-rect x0 y0 (+ x0 size) (- y0 size) 0.0
                                 (playpen::sctree-x0 glyph)
                                 (playpen::sctree-y0 glyph)
                                 (playpen::sctree-x1 glyph)
                                 (playpen::sctree-y1 glyph) ))

      ;;(gl:color 1 1 1 1)
      ;;(draw-rect -0.9 0.9 0.5 -0.5)
      ;;(draw-rect -0.6 0.6 0 0 0.0 0.0 0.0 1024/48 1024/48)
      ;; (gl:end)
      (gl:check-error))
    ;; Cleanup:
    (end-paint window)))

(defun run-test-2 ()
  (pwin:initialize-display)
  (let ((window (pwin:create-window
                 :application-name "Windowing test 2"
                 :width  (floor (pwin:display-width  pwin:*display*) 2)
                 :height (floor (pwin:display-height pwin:*display*) 2))))
    (unwind-protect
         (let ((playpen::*opengl-owner* 'funcall))
           (unless *texture-surface*
             (setf *texture-surface*
                   (make-instance 'playpen:cache-surface
                                  :image (playpen:image-matrix playpen:+rgba+ :width 1024 :height 1024))))
           (init-2)
           (loop with need-paint = t
                 with block = nil
                 with offset = 0
                 as event = (pwin:get-event (if block nil 0)) do
                 (setf block nil)
                 (typecase event
                   (expose
                    (paint-2 window offset)
                    (setf need-paint nil))
                   (close-request
                    (return-from run-test-2))
                   (motion
                    (setf offset (+ (event-x event) (event-y event))
                          need-paint t))
                   (timeout (setf block t)))
                 (when (and block need-paint)
                   (paint-2 window offset)
                   (setf need-paint nil))))
      ;; Cleanups:
      (pwin:destroy-window window))))

;; awesome pseudocode
#+NIL
(let ((x 0)
      (y 0)
      (toggle nil))
  (cell with (keyboard-event keyboard-event-p) from (default-keyboard-stream)
        with pointer-event from (latch (default-pointer))
        with channel to whatever-window
        compute
        (when keyboard-event-p (setf toggle (not toggle)))
        (setf x (pointer-event-x pointer-event)
              y (pointer-event-y pointer-event))))

#+NIL
(cell
 (:input keyboard-event (default-keyboard-event-stream))
 (:input pointer-state  (default-pointer-state))
 (:display ...))








