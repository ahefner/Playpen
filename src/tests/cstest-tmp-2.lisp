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

(defvar *robo* nil)

(defmethod handle-event ((window cache-surface-test) (event expose))
  (with-slots () window
    (with-graphics-context (window)
      (clear-screen #(0.0 0.0 0.0 0.0))
      (use-pixel-projection)
      (let ((cache (playpen::get-cache-surface)))
        (assert (typep cache 'playpen::opengl-texture))
        (use-texture cache)
        (gl:color 1.0 1.0 1.0 0.5)
        (gl:begin :quads)
        (playpen::%draw-rect 0 (dimensions cache))
        (gl:end))

      (gl:color 1.0 1.0 1.0 1.0)
      (let ((img (image-asset "pepper.png")))
        (use-texture img)
        (gl:begin :quads)
        (playpen::%draw-rect #c(0 0) #c(64 64))
        (gl:end)))))

(defmethod handle-event ((window cache-surface-test) (event button-press))
  (case (event-button event)
    (1
     (assert (playpen::text-init))
     (orf *robo* (with-filename-pointer (ptr (playpen::test-face-filename))
                   (playpen::text-load-face ptr 0)))
     (loop repeat 30 do
           (let* ((code (+ 30 (random 300))#+NIL (+ (random 400) (char-code #\A)))
                  (size (+ 10 (random 80)))
                  (glyph (playpen::text-render-glyph *robo* size code 1)))

             (unless (cffi:null-pointer-p glyph)
               (with-graphics-context (window)
                 (macrolet ((slot (name)
                              `(cffi:foreign-slot-value glyph 'playpen::rendered-glyph ',name)))
                   (let* ((width  (slot width))
                          (height (slot height))
                          (buffer (slot playpen::buffer))
                          (cs (playpen::get-cache-surface)))
                     (use-texture cs)
                     #+NIL
                     (print (list :width width :height height
                                  :advance-x (slot playpen::advance-x)
                                  :advance-y (slot playpen::advance-y)
                                  :buffer-p (cffi:null-pointer-p buffer)))
                     (unless (cffi:null-pointer-p buffer)
                       (assert (not (or (zerop width)
                                        (zerop height))))
                       (playpen::%cs-upload-from-foreign
                        (non-null (playpen::%cs-allocate-leaf cs width height))
                        buffer width height)))))
               (animate)))))
    (3
     (with-graphics-context (window)
       (use-texture (playpen::get-cache-surface))
       (playpen::cache-surface-reset
        (playpen::get-cache-surface)
        :clear t)
       (animate)))))
