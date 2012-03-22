(in-package :playpen-tests)


(defun st-new-tree ()
  (playpen::make-sctree :x1 1024 :y1 512))

(defclass surface-test (window time-consumer)
  ((tree :accessor tree :initform (st-new-tree))
   (rects :accessor rects :initform nil))
  (:default-initargs
   :application-name "kd-tree"
   :initial-width 1200 :initial-height 600))

(defmethod handle-event ((window surface-test) (event expose))
  (with-graphics-context (window)
    (clear-screen)
    (use-pixel-projection)
    (gl:translate 30 30 0)
    (labels ((draw-tree (tree colors)
               (when (eql :full (playpen::sctree-state tree))
                 (apply 'gl:color (if (null (playpen::sctree-children tree))
                                      (pop colors)
                                      '(0.2 0.2 0.2 0.2)))
                 (playpen::%draw-rect (complex (playpen::sctree-x0 tree)
                                               (playpen::sctree-y0 tree))
                                      (complex (1- (playpen::sctree-x1 tree))
                                               (1- (playpen::sctree-y1 tree)))))
               (dolist (child (playpen::sctree-children tree))
                 (draw-tree child colors)
                 (pop colors))))
      (gl:begin :quads)
      (draw-tree (tree window)
                 '#1=((0.3 0.04 0.0 0.3)
                      (0.0 0.3 0.26 0.3)
                      . #1#))
      (gl:end))))

(defmethod handle-event ((window surface-test) (event button-press))
  (case (event-button event)
    ;; Insert
    (1
     (let* ((width  (* 32 (1+ (random 5))) #+NIL (+ 4 (* 7 (round (expt (random 3.0) 3)))))
            (height (* 32 (1+ (random 5))) #+NIL (+ 4 (* 4 (round (expt (random 3.0) 3)))))
            (leaf (playpen::sctree-allocate (tree window) width height)))
       (etypecase leaf
         (playpen::sctree
          (print (list :allocated width height))
          (push (complex width height) (rects window))
          (animate))
         (null
          (print (list :allocation-failure width height))))))
    ;; Sort and re-insert
    (2 (setf (tree window) (st-new-tree)
             (rects window) (sort (rects window)
                                  #'>
                                  :key (lambda (x)
                                         (max (realpart x)
                                              (imagpart x)))))
       (dolist (rect (rects window))
         (playpen::sctree-allocate (tree window) (realpart rect) (imagpart rect)))
       (animate))
    ;; Reset
    (3 (setf (tree window) (st-new-tree)
             (rects window) nil)
       (animate))))

