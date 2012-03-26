;;;; This should be a very short-lived test wherein I organically grow
;;;; some crude text rendering on top of the cache surface, then begin
;;;; migrating that back into Playpen proper.

(in-package :playpen-tests)

(defclass text-test-1 (window time-consumer)
  ((text-size :initform 30 :accessor text-size))
  (:default-initargs
   :application-name "Text test"
   :initial-width 1100 :initial-height 400))


(defvar *face* nil)

(defmethod handle-event ((window text-test-1) (event expose))
  (with-graphics-context (window)
    (clear-screen #( 0.82 0.82 0.82 1.0))
    ;;(clear-screen #( 0.62 0.62 0.62 0.7))
    (use-pixel-projection)
    (assert (playpen::text-init))
    (orf *face* (with-filename-pointer (ptr (playpen::test-face-filename))
                  (playpen::text-load-face ptr 0)))
    (let ((cs (playpen::get-cache-surface)))

      (use-texture cs)

      (gl:matrix-mode :texture)
      (gl:load-identity)
      (gl:scale (/ 1.0d0 (width cs))
                (/ 1.0d0 (height cs))
                1.0d0)

      (loop with size = (text-size window)
            with pad = 1
            with point = #c(25 200)
            for char across "Bigger slump and bigger wars and a smaller recovery."
            for show = t then nil
            as glyph = (playpen::text-render-glyph *face* size (char-code char) 1)
            do
            (unless (cffi:null-pointer-p glyph)
              (cffi:with-foreign-slots
                  ((playpen::f-advance-x
                    playpen::index
                    width height
                    playpen::buffer
                    playpen::bitmap-left
                    playpen::bitmap-top)
                   glyph playpen::rendered-glyph)
                ;;(print (list char playpen::index width height))
                (when show
                  (print (list char
                               playpen::bitmap-left
                               playpen::f-advance-x
                               (/ playpen::f-advance-x
                                  (text-size window)))))
                (unless (or (zerop width) (zerop height))
                  (let ((leaf (non-null (playpen::%cs-allocate-leaf
                                         cs
                                         (+ pad width pad)
                                         (+ pad height pad)))))
                    (setf (playpen::sctree-pad leaf) pad)
                    (playpen::%cs-upload-from-foreign leaf playpen::buffer width height)
                    (gl:color 0.32 0.32 0.32 1)
                    (gl:begin :quads)
                    (let* ((p1 (+ point (complex playpen::bitmap-left
                                                 (- playpen::bitmap-top))))
                           (p2 (+ p1 (complex width height))))
                      (playpen::%draw-rect p1 p2 0
                                           (complex (+ (playpen::sctree-x0 leaf) pad)
                                                    (+ (playpen::sctree-y0 leaf) pad))
                                           (complex (- (playpen::sctree-x1 leaf) pad)
                                                    (- (playpen::sctree-y1 leaf) pad))))
                    (gl:end)))
                (incf point (complex playpen::f-advance-x)))
              (free glyph))))))

(defmethod handle-event ((window text-test-1) (event motion))
  (setf (text-size window)
        (round
         (clamp
          6
          (/ (realpart (coordinate event)) 8.0)
          130)))
  (animate))