;;;; Test image allocation within a surface.

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

(in-package :playpen)                   ; MOVEME

(defun rect-intersect-p* (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)
  (and (< ax0 bx1)
       (> ax1 bx0)
       (< ay0 by1)
       (> ay1 by0)))

(defun rect-contained-p* (ax0 ay0 ax1 ay1 bx0 by0 bx1 by1)
  "Returns true if rectangle A is fully contained in rectangle B."
  (and (>= ax0 bx0)
       (<= ax1 bx1)
       (>= ay0 by0)
       (<= ay1 by1)))

(defun validate-sctree-nodes (nodes x0 y0 x1 y1)
  "Ensure that no sctree nodes overlap each other or the edges of the image."
  (let ((overlapping 0)
        (out-of-bounds 0))
    (dolist (n1 nodes)
      (unless (rect-contained-p*
               (sctree-x0 n1) (sctree-y0 n1)
               (sctree-x1 n1) (sctree-y1 n1)
               x0 y0 x1 y1)
        (incf out-of-bounds))
      (dolist (n2 nodes)        
        (when (and (not (eq n1 n2))
                   (rect-intersect-p*
                    (sctree-x0 n1) (sctree-y0 n1)
                    (sctree-x1 n1) (sctree-y1 n1)
                    (sctree-x0 n2) (sctree-y0 n2)
                    (sctree-x1 n2) (sctree-y1 n2)))
          (incf overlapping 1))))
    (assert (evenp overlapping))
    (setf overlapping (ash overlapping -1))
    (unless (and (zerop overlapping)
                 (zerop out-of-bounds))
      (error "Some rectangles were invalid: ~A out of bounds, ~A overlapping pairs"
             out-of-bounds overlapping))))

(defun sctree-recursive-validate (tree)
  (validate-sctree-nodes (sctree-children tree)
                         (sctree-x0 tree)
                         (sctree-y0 tree)
                         (sctree-x1 tree)
                         (sctree-y1 tree))
  (dolist (child (sctree-children tree))
    (sctree-recursive-validate child)))  

(defun sa-test ()
  (let* ((width 800)
         (height 600)
         (total-area (* width height))
         (remaining-area total-area)
         (tree (make-sctree :x0 0 :y0 0 :x1 width :y1 height))
         (children
          (loop as child-width  = (random 100)
                as child-height = (random 100)
                as child-area = (* child-width child-height)
                repeat total-area
                while (>= remaining-area child-area)
                as child = (sctree-allocate tree child-width child-height)
                when child collect child into children
                when (null child) do (return children)
                do (decf remaining-area child-area))))
    (assert (= 0 (sctree-x0 tree)))
    (assert (= 0 (sctree-y0 tree)))
    (assert (= width (sctree-x1 tree)))
    (assert (= height (sctree-y1 tree)))
    (sctree-recursive-validate tree)
    (format t "~&Allocated ~A children with ~A pixels wasted (~4F% utilization)~%"
            (length children)
            remaining-area
            (- 100.0 (/ remaining-area total-area 0.01)))
    (validate-sctree-nodes children 0 0 width height)
    (sctree-recursive-validate tree)
    (setf *print-circle* t)             ;)
    tree))
