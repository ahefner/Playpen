;;;; Surface manager - handles caching of image patterns on OpenGL textures.

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


;;;; *****************************************************************
;;;; *****************************************************************
;;;; *****                                                       *****
;;;; *****             THIS WHOLE FILE IS SHIT.                  *****
;;;; *****                                                         ||
;;;; *****************************************************************
;;;; *****************************************************************

(in-package :playpen)

;;; The interface might look something like this: A function/macro to ensure
;;; that textures are locked in within a certain dynamic extent, keyed by
;;; pointer equality of the image array and an optional cache value (thereby
;;; making the assumption that the contents of an image array will be immutable
;;; unless the programmer specifically provides new cache values indicating it
;;; has changed, or explicitly invalidates the cached image). It's reasonable
;;; to only allow invalidating a cached image when it is not locked in

;;; The obvious way to implement this mapping, an EQUAL hash table keyed on
;;; the tuple of object identity and cache value, is demonstrably insufficient
;;; for text rendering performance. A hash table lookup (particularly an EQUAL
;;; hash table lookup!) per character will destroy performance. We need an
;;; efficient mapping directly from character codes to texture coordinates.
;;; We also need the same sort of mapping to glyph properties, and they might
;;; as well be the same structure if it's possible (rather than maintain two
;;; caches which happen to cycle almost identical streams of data through
;;; them). It might really be easier to manage this manually at a lower level,
;;; but it's just stupid, because the font texture is managed in almost the
;;; same way as the general cache surface, just with a specialized interface
;;; to the cache.

;;; We could use a generic function which notes when an object has been
;;; placed and removed in the cache, and the optimized glyph cache could
;;; be updated at this time. The locking style still poses a problem - we
;;; must explicitly lock each glyph used in a string before drawing it,
;;; otherwise a nonpresent glyph might evict a previous glyph in the string
;;; which hasn't yet been drawn. Implicit in this is the assumption that we
;;; must have all glyph images present in order to draw them at once, which
;;; is not necessary. Relaxing this constraint, we can simply draw all the
;;; glyphs which are already cached before attempting to bring in subsequent
;;; glyphs and trying again. In this case, all that's necessary is a way to
;;; query whether a glyph is present or not.


;;; Okay, the bit about locking above is wrong. You don't lock within a
;;; dynamic scope, which causes all sorts of obvious problems unless you
;;; also disallow nested lock requests. Instead, locking only occurs with
;;; respect to the stateful drawing functions, such that it reflects points
;;; where buffered drawing must be flushed (e.g. before removing an image
;;; pending to be drawn to accomodate a new one). This implies that the
;;; representation of locks can itself be distinct from the objects in the
;;; tree, and even the cache-surface itself, and instead is entirely the
;;; domain of said drawing functions. It also implies that code drawing
;;; directly through OpenGL get to manage their own textures.

(deftype texcoord () '(unsigned-byte 16))
(deftype image-array () '(simple-array (unsigned-byte 32) 2)) ; dubious


;;;; Surfaces represent textures and framebuffers in various modes of use.

(defclass bounded-surface ()            ; Dubious.
  (#+NIL (width  :reader width  :initarg :width)
   #+NIL (height :reader height :initarg :height)))


;;; Should we use a common set of width functions between surfaces, images, etc?
(defgeneric width (object))
(defgeneric height (object))

(defclass source-surface ()
  ()
  (:documentation "A surface which can be used as an image source in drawing operations."))

(defclass opengl-texture-surface (source-surface)
  ((texture-id :initarg :texture-id :reader texture-id :initform nil)
   #+NIL
   (texture-initialized :initarg :texture-initialized :reader texture-initialized
                        :initform nil)))

(defun realize-texture-surface (surface)
  (unless (texture-id surface)
    (with-slots (texture-id) surface
      (with-opengl () (setf texture-id (first (gl:gen-textures 1))))
      (mirror-push-rectangle surface 0 0 (width surface) (height surface)))))

(defclass mirrored-surface (bounded-surface)
  ((image :initarg :image :reader mirrored-surface-image))
  (:documentation "A mirrored surface is backed by a lisp array, and their contents are kept synchronized."))

(defmethod width  ((this mirrored-surface))
  (image-width (mirrored-surface-image this)))

(defmethod height ((this mirrored-surface))
  (image-height (mirrored-surface-image this)))

(defgeneric mirror-push-rectangle (surface x0 y0 x1 y1)
  (:documentation "Ensure that changes to a rectangle within the array backing a mirrored surface is reflected in its hardware state"))

(defgeneric mirror-pull-rectangle (surface x0 y0 x1 y1)
  (:documentation "Ensure that changes by hardware drawing operations within a rectangle of a surface are reflected in its array."))

(defun opengl-image-formats (image-format)
  (let ((pf (pixel-format image-format)))
    (cond
      ((eql pf +rgb+)   (values 3 :rgb   :unsigned-byte))
      ((eql pf +rgba+)  (values 4 :rgba  :unsigned-byte))
      ((eql pf +alpha+) (values 1 :alpha :unsigned-byte)))))

(defmethod mirror-push-rectangle ((surface mirrored-surface) x0 y0 x1 y1)
  (declare (type texcoord x0 y0 x1 y1)
           (type mirrored-surface surface))
  (with-opengl ()
    (gl:bind-texture :texture-2d (texture-id surface))
    (let ((image (mirrored-surface-image surface))
          (width (- x1 x0))
          (height (- y1 y0)))
      (multiple-value-bind (internal format type) (opengl-image-formats image)
        (with-array-pointer (pointer (image-matrix-data image))
          (gl:pixel-store :unpack-row-length (image-pitch image))
          (let ((data (cffi:inc-pointer pointer (image-row-major-offset-byte image x0 y0))))
            (cond
              ((and (= width (image-width image))
                    (= height (image-height image)))
               (gl:tex-parameter :texture-2d :generate-mipmap :true)
               (gl:hint :generate-mipmap-hint :nicest)
               (gl:tex-image-2d :texture-2d 0 internal width height 0 format type data)
               (gl:check-error))
              (t
               (gl:tex-sub-image-2d :texture-2d 0 x0 y0 width height format type data)
               (gl:check-error))))
          (gl:pixel-store :unpack-row-length 0)
          (gl:check-error))))))

(defclass basic-mirrored-source (mirrored-surface opengl-texture-surface) ()) ; Dubious.

;;; A simple n-ary tree is used to represent the allocation of subimages
;;; within a texture:

(defstruct sctree
  (parent nil :type (or null sctree))
  (x0 0 :type texcoord)
  (x1 0 :type texcoord)
  (y0 0 :type texcoord)
  (y1 0 :type texcoord)
  (state :empty :type (member :empty :partial :full :allocated))
  (children nil :type list)
  (requested-width 0 :type texcoord)
  (requested-height 0 :type texcoord)
  (x-offset 0 :type texcoord)
  (y-offset 0 :type texcoord)
  (allocant nil))

(defun sctree-width (sctree)
  (- (sctree-x1 sctree) (sctree-x0 sctree)))

(defun sctree-height (sctree)
  (- (sctree-y1 sctree) (sctree-y0 sctree)))

(defun sctree-allocate (tree width height)
  (and (>= (sctree-width tree)  width)
       (>= (sctree-height tree) height)
       (case (sctree-state tree)
         ((:full :allocated) nil)
         (:partial
          (loop for child in (sctree-children tree)
                as result = (sctree-allocate child width height)
                when result return result))
         (:empty
          (assert (null (sctree-children tree)))
          (sctree-allocate-in-leaf tree width height)))))

(defvar *decal-spacing* 5)
(defparameter *smallest-reasonable-decal* 20)

(defun sctree-allocate-in-leaf (tree width height)
  "Partition and allocate a portion of a leaf node"
  (declare (type texcoord width height))
  (assert (eql :empty (sctree-state tree)))
  (assert (null (sctree-children tree)))
  (assert (>= (sctree-width tree) width))
  (assert (>= (sctree-height tree) height))
  (let* ((x-partition (and (<= (+ *decal-spacing* *smallest-reasonable-decal*)
                               (- (sctree-width tree) width))
                           (+ (sctree-x0 tree) width)))
         (y-partition (and (<= (+ *decal-spacing* *smallest-reasonable-decal*)
                               (- (sctree-height tree) height))
                           (+ (sctree-y0 tree) height)))
         (child (sctree-partition tree x-partition y-partition))
         (old-state (sctree-state child)))
    (setf (sctree-state child) :allocated
          (sctree-requested-width child) width
          (sctree-requested-height child) height
          (sctree-x-offset child) (truncate (- (sctree-width child) width) 2)
          (sctree-y-offset child) (truncate (- (sctree-height child) height) 2))
    (sctree-propagate-state-change (sctree-parent child) old-state :allocated)
    child))

(defun sctree-propagate-state-change (tree child-old-state child-new-state)
  (unless (or (null tree) (eql child-old-state child-new-state))
    (let ((parent-state (sctree-state tree)))
      (case child-new-state
        (:partial
         (setf (sctree-state tree) :partial)
         (sctree-propagate-state-change (sctree-parent tree) parent-state :partial))
        ((:empty :full :allocated)
         (sctree-recompute-state tree))))))

(defun sctree-reduced-state (tree)
  (let ((state (sctree-state tree)))
    (if (eql state :allocated) :full state)))

(defun sctree-recompute-state (tree)
  (sctree-propagate-state-change
   (sctree-parent tree)
   (sctree-state tree)
   (setf (sctree-state tree)
         (reduce (lambda (&optional x y)
                   (cond ((not (or x y)) :empty)
                         ((eql x y) x)
                         (t :partial)))
                 (sctree-children tree)
                 :key #'sctree-reduced-state))))

(defun sctree-partition (tree x y)
  "Partition tree into 2, 4, or no subtrees, along the vertical line
   'x' and the horizontal line 'y', which may be null or a texcoord.
   Returns the upper-left sector, or the tree itself if x and y are
   both nil."
  (declare (type (or null texcoord) x y))
  (assert (eql :empty (sctree-state tree)))
  (assert (null (sctree-children tree)))
  (if (not (or x y))              ; No partition? Allocate whole node.
      tree
      (let ((inner-child (make-sctree :parent tree
                                      :x0 (sctree-x0 tree)
                                      :x1 (or x (sctree-x1 tree))
                                      :y0 (sctree-y0 tree)
                                      :y1 (or y (sctree-y1 tree)))))
        ;; The order in which we push these determines the typical
        ;; direction of allocation within the texture.
        (push inner-child (sctree-children tree))
        (when (and x y)
          (push (make-sctree :parent tree
                             :x0 x :y0 y
                             :x1 (sctree-x1 tree)
                             :y1 (sctree-y1 tree))
                (sctree-children tree)))
        (when x
          (push (make-sctree :parent tree
                             :x0 x
                             :x1 (sctree-x1 tree)
                             :y0 (sctree-y0 tree)
                             :y1 (or y (sctree-y1 tree)))
                (sctree-children tree)))
        (when y
          (push (make-sctree :parent tree
                             :x0 (sctree-x0 tree)
                             :x1 (or x (sctree-x1 tree))
                             :y0 y
                             :y1 (sctree-y1 tree))
                (sctree-children tree)))
        inner-child)))

;;;; Cache surfaces dynamically arrange many small images within one large texture.

;;; .. why the fuck must the user explicitly manage the image? Stupid.

(defclass cache-surface (basic-mirrored-source)
  ((tree :reader cache-surface-tree)))

(defmethod initialize-instance :after ((object cache-surface) &key &allow-other-keys)
  (with-slots (tree image) object
    (setf tree (make-sctree :x0 0 :y0 0 :x1 (image-width image) :y1 (image-height image)))))

(defun copy-into-image (source sx sy destination dx dy width height)
  ;; FIXME: I'm a big cheater.
  (cond
    ;; Expand alpha to RGBA between matrices
    ((and (typep source 'image-matrix)
          (eql (pixel-format source) +alpha+)
          (typep destination 'image-matrix)
          (eql (pixel-format destination) +rgba+))
     (copy-a8-rectangle-to-rgba32-image
      width height (image-matrix-data source) sx sy
      (image-matrix-data destination) dx dy))
    ;; Copy rectangle between RGBA matrices.
    ((and (typep source 'image-matrix)
          (eql (pixel-format source) +rgba+)
          (typep destination 'image-matrix)
          (eql (pixel-format destination) +rgba+))
     (let ((source-matrix (image-matrix source))
           (dest-matrix   (image-matrix destination)))
       (declare (type (simple-array (unsigned-byte 32) 2)
                      source-matrix dest-matrix))
       (loop-rectangle (width height)
        ((source-index :pitch (array-dimension source-matrix 1)
                       :offset (xy->row-major-index source-matrix sx sy))
         (dest-index :pitch (array-dimension dest-matrix 1)
                     :offset (xy->row-major-index dest-matrix dx dy)))
        (setf (row-major-aref dest-matrix dest-index)
              (row-major-aref source-matrix source-index)))))
    (t (error "FIXME: General case for copy-into-image not implemented"))))

(defun mirror-push-leaf (surface leaf)
  (mirror-push-rectangle surface
                         (sctree-x0 leaf) (sctree-y0 leaf)
                         (sctree-x1 leaf) (sctree-y1 leaf)))

(defun cache-surface-allocate (surface new-image
                               x0 y0 x1 y1 allocant)
  (let* ((width (- x1 x0))
         (height (- y1 y0))
         (leaf (sctree-allocate (cache-surface-tree surface) width height)))
    (assert (>= (sctree-width leaf) width))
    (assert (>= (sctree-height leaf) height))
    (cond
      ((null leaf)
       (error "Cache surface full, and I haven't written the repacking code."))
      (t (setf (sctree-allocant leaf) allocant)
         (copy-into-image new-image x0 y0
                          (mirrored-surface-image surface)
                          (+ (sctree-x0 leaf) (sctree-x-offset leaf))
                          (+ (sctree-y0 leaf) (sctree-y-offset leaf))
                          width height)
         (mirror-push-leaf surface leaf)
         leaf))))

;;; Not sure how to partition surfaces and drawing between files.
;;; Preconditions:
;;;  * Texcoord matrix configured for integer coordinates.
;;;  * OpenGL is drawing quads.
(defun %draw-leaf (leaf x0 y0 x1 y1 &optional (z 0.0))
  (let ((u0 (+ (sctree-x0 leaf) (sctree-x-offset leaf)))
        (v0 (+ (sctree-y0 leaf) (sctree-y-offset leaf))))
    (%draw-rect x0 y0 x1 y1 z u0 v0
                (+ u0 (sctree-requested-width leaf))
                (+ v0 (sctree-requested-height leaf)))))

