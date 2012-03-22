(in-package :playpen-tests)

(defun cst-load-test-images ()
  (alexandria:shuffle
   (append
    (directory "/home/hefner/ptest2/*.gif")
    (directory "/home/hefner/nerdly/game-screenshots/Good7800 - 0.999.3 - Atari 7800 - screen/snap/*.png")
    (directory "/home/hefner/graphics/clipping/icons/PNG/*.png")))
  #+NIL
  (mapcar 'builtin-asset-path
          (list
           "test-a.png"
           "test-b.png"
           "test-c.png"
           "pepper.png"
           "angry.gif")))

(defclass cache-surface-test (window time-consumer)
  (#+NIL (cache :initform nil)
   (images :initform (cst-load-test-images)))
  (:default-initargs
   :application-name "Cache surface test"
   :initial-width 1024
   :initial-height 1024))

;(defun draw (image))

(defmethod handle-event ((window cache-surface-test) (event expose))
  (with-slots () window
    (with-graphics-context (window)
      (clear-screen)
      (use-pixel-projection)
      ;;(orf cache (playpen::make-cache-surface *gl-context*))
      (let ((cache (playpen::get-cache-surface)))
        (assert (typep cache 'playpen::opengl-texture))
        (use-texture cache)
        (gl:begin :quads)
        (playpen::%draw-rect 0 (dimensions cache) 0 0 (dimensions cache))
        (gl:end)))))

(defmethod handle-event ((window cache-surface-test) (event button-press))
  (case (event-button event)
    (1 (with-graphics-context (window)
         (with-slots (images) window
           (let ((next (pop images))
                 #+NIL (cache (playpen::get-cache-surface))
                 (*print-circle* t))
             (when next
               (print next)
               (playpen::cached-image (read-image-file next))))))
       (animate))))
