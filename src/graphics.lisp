;;;; OpenGL graphics stuff.

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

(in-package :playpen)


(deftype texcoord   () '(unsigned-byte 16))
(deftype u32-matrix () '(simple-array (unsigned-byte 32) 2))
(deftype u32-vector () '(simple-array (unsigned-byte 32) 1))

;;;; Current drawing window

(defvar *window*)

(defmethod width  ((a window)) (window-width a))
(defmethod height ((a window)) (window-height a))

;;;; Context management

;;; These only make sense to use in/around the toplevel paint
;;; function.  It would be nice to have composability for nested
;;; widgets, but that can wait until I have compositing and some
;;; strategy for making that useful.

(defun reset-transforms ()
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:matrix-mode :projection)
  (gl:load-identity))

(defun use-pixel-projection ()
  (assert (not (null *window*)))
  (reset-transforms)
  (gl:ortho 0 (width *window*) (height *window*) 0 -1 1)
  (gl:check-error))

(defun use-centered-pixel-projection ()
  (assert (not (null *window*)))
  (reset-transforms)
  ;; I'm curious what happens when width/height are odd, and whether
  ;; we should nudge the coordinates by half a pixel to get precise
  ;; drawing.
  (gl:scale (/ (* 0.5 (width *window*)))
            (/ (* -0.5 (height *window*)))
            1.0)
  (gl:check-error))

(defun use-graphic-projection ()
  (assert (not (null *window*)))
  (reset-transforms)
  (let ((aspect  (/ (width *window*)
                    (height *window*))))
    (gl:scale (if (< aspect 1) 1 (/ aspect))
              (if (< aspect 1) aspect 1)
              1.0)))

(defvar *cache-surface*)

(defun call-with-graphics-context (window continuation)
  (begin-paint window)
  (unwind-protect
       (let ((*gl-context* (graphics-context window))
             (*cache-surface* nil)
             (*window* window))

         ;; Blending mode for textures with premultiplied alpha:
         (gl:enable :blend)
         (gl:blend-func :one :one-minus-src-alpha)

         (gl:disable :texture-2d)
         (gl:disable :depth-test)
         (gl:disable :cull-face)
         (gl:color 1.0 1.0 1.0 1.0)

         (gl:load-identity)
         (use-pixel-projection)
         (gl:matrix-mode :modelview)
         (funcall continuation)
         (gl:check-error))
    ;; Cleanup:
    (end-paint window)))

(defmacro with-graphics-context ((window) &body body)
  `(call-with-graphics-context ,window (lambda () ,@body)))

;;;; Little utilities

(defun clear-screen (&optional (color #(0.0 0.0 0.0 0.8)))
  (gl:clear-color (elt color 0)
                  (elt color 1)
                  (elt color 2)
                  (elt color 3))
  (gl:clear :color-buffer-bit
            :depth-buffer-bit))

;;; Do I want to transform geometry to window coordinates, or
;;; transform window coordinates (namely, the mouse pointer) to
;;; geometry coordinates?

(defun transform-to-screen (p &optional z)
  (multiple-value-bind (x y)
      (glu:project (realpart p) (imagpart p) (or z 0.0))
    (complex
     x
     (- (window-height *window*) y))))

(defun transform-from-screen (p &optional z)
  (multiple-value-bind (x y z)
      (glu:un-project (realpart p)
                      (- (window-height *window*) (imagpart p))
                      (or z 0.0))
    (values (complex x y)
            z)))

(defun transform-to-screen* (x y &optional z)
  (multiple-value-bind (x y) (glu:project x y (or z 0.0))
    (values x
            (- (window-height *window*) y))))

;;; I'm going to throw these draw-rect variants at the wall and see what sticks.

(defun %draw-rect* (x0 y0 x1 y1
                    &optional
                    (z 0.0f0)
                    (u0 0.0)
                    (v0 0.0)
                    (u1 1.0)
                    (v1 1.0))
  "Draw an optionally textured rectangle. Assumes OpenGL is already drawing quads."
  (gl:tex-coord u0 v0)
  (gl:vertex x0 y0 z)
  (gl:tex-coord u1 v0)
  (gl:vertex x1 y0 z)
  (gl:tex-coord u1 v1)
  (gl:vertex x1 y1 z)
  (gl:tex-coord u0 v1)
  (gl:vertex x0 y1 z))

(defun %draw-rect (p1 p2
                   &optional
                   (z 0.0f0)
                   (uv0 #c(0.0 0.0))
                   (uv1 #c(1.0 1.0)))
  (%draw-rect* (realpart p1) (imagpart p1)
               (realpart p2) (imagpart p2)
               z
               (realpart uv0) (imagpart uv0)
               (realpart uv1) (imagpart uv1)))

(defun %draw-rect+ (p1 offset
                    &optional
                    (z 0.0f0)
                    (uv0 #c(0.0 0.0))
                    (uv1 #c(1.0 1.0)))
  (%draw-rect p1 (+ p1 offset) z uv0 uv1))

(defun %draw-rect@ (center dimensions
                    &optional
                    (z 0.0f0)
                    (uv0 #c(0.0 0.0))
                    (uv1 #c(1.0 1.0)))
  (%draw-rect+ (- center (* 0.5 dimensions)) dimensions z uv0 uv1))

;;;; Textures / Surfaces

(defclass opengl-texture (opengl-resource dimension-trait)
  ((texture-id :initarg :texture-id
               :initform nil
               :reader texture-id)
   (mipmap-p :initarg :mipmap
             :initform nil
             :reader mipmap-p)))

(defun opengl-image-formats (image-format)
  (let ((pf (pixel-format image-format)))
    (cond
      ((eql pf +rgb+)   (values :rgb8  :rgb   :unsigned-byte 4))
      ((eql pf +rgba+)  (values :rgba8 :rgba  :unsigned-byte 4))
      ((eql pf +alpha+) (values 1      :alpha :unsigned-byte 1)))))

(defun allocate-texture (&key key mipmap)
  (assert-gl-context)
  (let ((texture (make-instance
                  'opengl-texture
                  :mipmap mipmap
                  :texture-id (first (gl:gen-textures 1)))))
    (gl:check-error)
    (attach-resource *gl-context* texture (or key texture))
    (values texture)))

(defvar *total-pixels* 0)

(defun texture-load-basic (texture image)
  (assert-ownership texture)
  (gl:bind-texture :texture-2d (texture-id texture))
  (gl:check-error)
  (multiple-value-bind (internal format type)
      (opengl-image-formats image)
    (print (list image (type-of (data-array image)) ))
    (with-array-pointer (pointer (data-array image))
      (print (list image (type-of (data-array image)) pointer))
      (gl:pixel-store :unpack-row-length (image-pitch image))
      (cond
        ((mipmap-p texture)
         (gl:tex-parameter :texture-2d :generate-mipmap :true)
         (gl:hint :generate-mipmap-hint :nicest))
        ((not (mipmap-p texture))
         (gl:tex-parameter :texture-2d :generate-mipmap :false)))
      (gl:check-error)
      (gl:tex-image-2d :texture-2d
                       0
                       internal
                       (width image)
                       (height image)
                       0
                       format
                       type
                       pointer)
      (setf (dimensions-of texture) (complex
                                     (width image)
                                     (height image)))
      (incf *total-pixels* (area texture))
      (print (list :total-pixels *total-pixels*))
      (gl:check-error)
      (gl:pixel-store :unpack-row-length 0)
      (gl:check-error))))

(defun texture-load (texture image)
  ;; FIXME: Make sure PBOs supported, fall back to basic glTexImage2D
  ;; path if not.
  #+NIL (texture-load-basic texture image)
  (texture-load-pbo texture image))

(defun allocate-and-upload (image &key mipmap)
  (let ((texture (allocate-texture :key image :mipmap mipmap)))
    (texture-load texture image)
    texture))

(defun texture-update-rectangle (image x0 y0 x1 y1)
  (use-texture image)
  (gl:pixel-store :unpack-row-length (image-pitch image))
  (multiple-value-bind (internal format type)
      (opengl-image-formats image)
    (declare (ignore internal))
    (with-array-pointer (pointer (data-array image))
      (gl:tex-sub-image-2d
       :texture-2d 0 x0 y0 (- x1 x0) (- y1 y0) format type
       (cffi:inc-pointer pointer (* 4 (+ x0 (* y0 (image-pitch image))))))))
  (gl:pixel-store :unpack-row-length 0)
  (gl:check-error))

(defun use-texture (image &key (mipmap t))
  (assert-gl-context)
  (gl:enable :texture-2d)
  (gl:tex-env :texture-env :texture-env-mode :modulate)

  (gl:matrix-mode :texture)
  (gl:load-identity)
  (gl:matrix-mode :modelview)

  (let (texture)

    (typecase image
      (null (error "NIL is not a legal image designator."))
      ;; Texture objects designate themselves.
      (opengl-texture
       (shiftf texture image nil))
      ;; Otherwise look it up in the map.
      (t (setf texture (gethash image (resource-map *gl-context*)))))

    (cond
      ;; Bind existing texture object.
      (texture
       (gl:bind-texture :texture-2d (texture-id texture))
       (gl:check-error))
      ;; Allocate and upload to new texture, which will leave
      ;; the :texture-2d target bound accordingly.
      ((null texture)
       (setf (gethash image (resource-map *gl-context*))
             (allocate-and-upload image :mipmap mipmap))
       (cond
         (mipmap
          (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear))
         ((not mipmap)
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear)))

       (gl:tex-parameter :texture-2d :texture-border-color #(0.0 0.0 0.0 0.0))
       (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
       (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)))

    (gl:check-error)))

;;;; Buffers

(defclass opengl-buffer (opengl-resource)
  ((buffer-id :initarg :buffer-id
              :initform nil
              :reader buffer-id)))

(defun allocate-buffer ()
  (let ((buffer (make-instance 'opengl-buffer
                               :buffer-id (first (gl:gen-buffers 1)))))
    (gl:check-error)
    (attach-resource *gl-context* buffer)
    (values buffer)))

;;; I'm still feeling out how to best use PBOs..

;; (defvar *test-buffer* nil)
;; (defvar *test-mem* nil)

;; (defun buffer-test ()
;;   (unless *test-buffer*
;;     (setf *test-buffer* (allocate-buffer)
;;           *test-mem* #+NIL (cffi:foreign-alloc :uint32 :initial-element #xc0 :count #x10000)
;;           (gl:alloc-gl-array :uint32 #x10000))
;;     (gl:bind-buffer :pixel-unpack-buffer (buffer-id *test-buffer*))
;;     (gl:check-error)
;;     (gl:buffer-data :pixel-unpack-buffer :static-draw *test-mem* :size (* 4 256 256))
;;     (gl:bind-buffer :pixel-unpack-buffer 0)
;;     (gl:check-error)
;;     (print :success!)))

;;; FIXME: not tested on non-RGBA textures, possibly has other issues.
(defun texture-load-pbo (texture image)
  (assert-ownership texture)
  (gl:bind-texture :texture-2d (texture-id texture))
    (gl:check-error)
  (let ((pbo (allocate-buffer)))
    (gl:bind-buffer :pixel-unpack-buffer (buffer-id pbo))
    (gl:check-error)
    (multiple-value-bind (internal format type bytes-per-pixel)
        (opengl-image-formats image)
      (with-array-pointer (pointer (data-array image))
        (gl:pixel-store :unpack-row-length (image-pitch image))
        (cond
          ((mipmap-p texture)
           (gl:tex-parameter :texture-2d :generate-mipmap :true)
           (gl:hint :generate-mipmap-hint :nicest))
          ((not (mipmap-p texture))
           (gl:tex-parameter :texture-2d :generate-mipmap :false)))
        (gl:check-error)
        (%gl:buffer-data :pixel-unpack-buffer
                         (* bytes-per-pixel
                            (image-pitch image)
                            (height image))
                         pointer
                         :static-draw)
        (gl:check-error)
        (gl:tex-image-2d :texture-2d
                         0
                         internal
                         (width image)
                         (height image)
                         0
                         format
                         type
                         (cffi:null-pointer))
        (setf (dimensions-of texture) (complex
                                       (width image)
                                       (height image)))
        (incf *total-pixels* (area texture))
        (gl:check-error)
        (gl:pixel-store :unpack-row-length 0)
        (gl:check-error)
        (gl:bind-buffer :pixel-unpack-buffer 0)
        (gl:delete-buffers (list (buffer-id pbo)))
        (gl:check-error)))))

;;;; Cache surface

(defstruct sctree
  (parent nil :type (or null sctree))
  (x0 0 :type texcoord)
  (x1 0 :type texcoord)
  (y0 0 :type texcoord)
  (y1 0 :type texcoord)
  (state :empty :type (member :empty :partial :full))
  (children nil :type list)
  ;; Pixels of padding within each side of coordinate bounds.
  (pad 0 :type texcoord)
  ;; Object allocated in this node, if a :full leaf. Otherwise, NIL.
  (allocant nil))

(defun sctree-width (sctree)
  (- (sctree-x1 sctree) (sctree-x0 sctree)))

(defun sctree-height (sctree)
  (- (sctree-y1 sctree) (sctree-y0 sctree)))

(defmethod width  ((this sctree)) (sctree-width this))
(defmethod height ((this sctree)) (sctree-height this))

(defun sctree-allocate (tree width height)
  (and (>= (sctree-width tree)  width)
       (>= (sctree-height tree) height)
       (ecase (sctree-state tree)
         (:full nil)
         (:partial
          (loop for child in (sctree-children tree)
                as result = (sctree-allocate child width height)
                when result return result))
         (:empty
          (assert (null (sctree-children tree)))
          (sctree-allocate-in-leaf tree width height)))))

(defun sctree-partition (tree x y)
  "Partition tree into 2, 4, or no subtrees, along the vertical line
   'x' and the horizontal line 'y', which may be null or a texcoord.
   Returns the upper-left sector, or the tree itself if x and y are
   both nil."
  (declare (type (or null texcoord) x y))
  (assert (eql :empty (sctree-state tree)))
  (assert (null (sctree-children tree)))
  (assert (or (null x) (> x (sctree-x0 tree))))
  (assert (or (null y) (> y (sctree-y0 tree))))
  (assert (or (null x) (<= x (sctree-x1 tree))))
  (assert (or (null y) (<= y (sctree-y1 tree))))
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

(defun sctree-recompute-state (tree)
  (sctree-propagate-state-change
   (sctree-parent tree)
   (sctree-state tree)
   (setf (sctree-state tree)
         (cond
           ((every (lambda (x) (eql :full (sctree-state x)))
                   (sctree-children tree))
            :full)
           ((every (lambda (x) (eql :empty (sctree-state x)))
                   (sctree-children tree))
            :empty)
           (t :partial)))))

(defun sctree-propagate-state-change (tree child-old-state child-new-state)
  (unless (or (null tree) (eql child-old-state child-new-state))
    (let ((parent-state (sctree-state tree)))
      (ecase child-new-state
        (:partial
         (setf (sctree-state tree) :partial)
         (sctree-propagate-state-change (sctree-parent tree) parent-state :partial))
        ((:empty :full)
         (sctree-recompute-state tree))))))

(defun sctree-change-state (tree new-state)
  (sctree-propagate-state-change
   (sctree-parent tree)
   (shiftf (sctree-state tree) new-state)
   new-state))

(defun sctree-allocate-in-leaf (tree width height)
  "Partition and allocate a portion of a leaf node"
  (declare (type texcoord width height))
  (assert (eql :empty (sctree-state tree)))
  (assert (null (sctree-children tree)))
  (assert (>= (sctree-width tree) width))
  (assert (>= (sctree-height tree) height))
  (setf (sctree-pad tree) 0)
  (let* ((lw (sctree-width tree))
         (lh (sctree-height tree))
         (slack-x (- lw width))
         (slack-y (- lh height)))
    (cond
      ;; If the leaf fits, allocate it.
      ((and (= lw width) (= lh height))
       (sctree-change-state tree :full)
       (values tree))
      ;; Split..
      ((> slack-x slack-y)
       (sctree-allocate-in-leaf
        (sctree-partition tree (+ (sctree-x0 tree) width) nil)
        width
        height))
      (t
       (sctree-allocate-in-leaf
        (sctree-partition tree nil (+ (sctree-y0 tree) height))
        width
        height)))))

(defclass cache-surface (opengl-texture)
  ((tree :accessor cache-surface-tree
         :initform nil)
   (map  :reader cache-surface-map
         :initarg :map
         :initform (make-hash-table))))

(defun allocant-key (object)
  (etypecase object
    (image-vector object)
    (image-matrix object)))

(defmethod width  ((a cache-surface)) (width  (cache-surface-tree a)))
(defmethod height ((a cache-surface)) (height (cache-surface-tree a)))

(defun make-cache-surface (context &key (width 1024) (height 1024))
  (let ((cache
         (make-instance 'cache-surface
                        :texture-id (first (gl:gen-textures 1)))))
    (attach-resource context cache :cache)
    (%cs-init-texture cache width height)
    (values cache)))

(defun %cs-init-texture (cache width height)
  (with-slots (tree) cache
    (assert (null tree))
    (setf tree (make-sctree :x1 width :y1 height))
    (gl:bind-texture :texture-2d (texture-id cache))
    (gl:check-error)
    (gl:tex-image-2d :texture-2d
                     0
                     :rgba8
                     (sctree-width tree)
                     (sctree-height tree)
                     0
                     :rgba
                     :unsigned-byte
                     (cffi:null-pointer))
    (gl:check-error)
    (setf (dimensions-of cache) (complex width height))
    ;; Hopefully I don't regret enabling filtering. I recall having
    ;; trouble in G1 on my old R300 when I wanted pixel-precise drawing
    ;; of UI elements, such that I was forced to turn it off. Hopefully
    ;; it was a bug in my code (or the open source driver).
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:matrix-mode :texture)           ; FIXME CAN'T TOUCH THIS
    (gl:load-identity)
    (gl:scale (/ 1.0d0 (width cache))
              (/ 1.0d0 (height cache))
              1.0d0)
    (gl:tex-parameter :texture-2d :texture-border-color #(0.0 0.0 0.0 0.0))
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
    (gl:pixel-store :unpack-row-length 0)
    (gl:check-error)))

;;; Remove allocant of leaf, or all children of tree.
(defun %cs-evict-tree (cache tree)
  (labels ((purge-allocants (tree)
             (when (sctree-allocant tree)
               (remhash (allocant-key (sctree-allocant tree))
                        (cache-surface-map cache)))
             (dolist (child (sctree-children tree))
               (purge-allocants child))))
    (purge-allocants tree)
    (setf (sctree-children tree) nil)
    (sctree-change-state tree :empty)
    (assert (eql :empty (sctree-state tree)))))

(defun %cs-find-evictee (tree width height)
  (assert (<= width (sctree-width tree)))
  (assert (<= height (sctree-height tree)))
  (setf (sctree-children tree) (alexandria:shuffle (sctree-children tree)))
  ;; TODO: Randomly evict whole node?
  (loop for child in (sctree-children tree)
        when (and (<= width (sctree-width child))
                  (<= height (sctree-height child)))
        return (%cs-find-evictee child width height)
        finally (return tree)))

(defun %cs-evict-to-fit (cache width height)
  (let ((victim
         (non-null
          (%cs-find-evictee (cache-surface-tree cache) width height))))
    (%cs-evict-tree cache victim)
    (assert (eql :empty (sctree-state victim)))

    (values victim)))

(defun %cs-evict-and-allocate (cache width height)
  (non-null
   (sctree-allocate
    (non-null (%cs-evict-to-fit cache width height))
    width height)))

(defun %cs-allocate-leaf (cache width height)
  (or (sctree-allocate (cache-surface-tree cache) width height)
      (%cs-evict-and-allocate cache width height)))

#+NIL                                   ; fuck it. later.
(defun texture-clear-rectangle (x0 y0 x1 y1)
  (let ((pbo (allocate-buffer)))
    (gl:bind-buffer :pixel-unpack-buffer (buffer-id pbo))
    (gl:check-error)
    (%gl:buffer-data )
    (gl:bind-buffer :pixel-unpack-buffer 0)
    (gl:delete-buffers (list (buffer-id pbo)))
    (gl:check-error)))

(defun upload-subimage (pointer x y width height format type &optional (pitch width))
  (gl:pixel-store :unpack-row-length pitch)
  ;; FIXME sometime maybe: use PBOs. Particularly beneficial for
  ;; background-loaded stuff, if we start the pixel transfer from
  ;; an event handler outside of repaint.
  (gl:tex-sub-image-2d :texture-2d
                       0
                       x
                       y
                       width
                       height
                       format type
                       pointer)
  (gl:check-error)
  (gl:pixel-store :unpack-row-length 0)
  (gl:check-error))

;;; This is fucking stupid.
(defun texture-clear-rectangle (x0 y0 x1 y1)
  (let* ((width (- x1 x0))
         (height (- y1 y0))
         (buffer (calloc (* 4 width height) 1)))
    (unless (cffi:null-pointer-p buffer) ;)
      (unwind-protect
           (upload-subimage buffer x0 y0 width height :bgra :unsigned-byte)
        (free buffer)))))

(defun %cs-upload-to-leaf (leaf image)
  (multiple-value-bind (internal format type)
      (opengl-image-formats image)
    (declare (ignore internal))         ; Eh?
    (when (sctree-pad leaf)             ; ARGH! The waste!
      (texture-clear-rectangle (sctree-x0 leaf) (sctree-y0 leaf)
                               (sctree-x1 leaf) (sctree-y1 leaf)))
    (with-array-pointer (pointer (data-array image))
      (upload-subimage pointer
                       (+ (sctree-pad leaf) (sctree-x0 leaf))
                       (+ (sctree-pad leaf) (sctree-y0 leaf))
                       (width image)
                       (height image)
                       format type))))

(defun %cs-upload-from-foreign (leaf rgba-pointer width height)
  (when (sctree-pad leaf)             ; ARGH! The waste!
      (texture-clear-rectangle (sctree-x0 leaf) (sctree-y0 leaf)
                               (sctree-x1 leaf) (sctree-y1 leaf)))
  (upload-subimage rgba-pointer
                   (+ (sctree-pad leaf) (sctree-x0 leaf))
                   (+ (sctree-pad leaf) (sctree-y0 leaf))
                   width
                   height
                   :rgba
                   :unsigned-byte))

(defun cache-surface-allocate-image (cache image &key (pad 1))
  ;; You *could* have multiple atlases in different formats, but it
  ;; seems counterproductive for my purposes.
  (let ((leaf (%cs-allocate-leaf cache
                                 (+ pad pad (width image))
                                 (+ pad pad (height image)))))
    (non-null leaf)
    (assert (= (dimensions leaf) (+ (dimensions image)
                                    (complex (* 2 pad) (* 2 pad)))))
    (setf (sctree-pad leaf) pad
          (sctree-allocant leaf) image
          (gethash (allocant-key image) (cache-surface-map cache)) leaf)
    (%cs-upload-to-leaf leaf image)
    (values leaf)))

(defun cache-surface-reset (cache &key (clear nil))
  (when clear
    (let ((tree (cache-surface-tree cache)))
      (%cs-evict-tree cache tree)
      (when clear (texture-clear-rectangle 0 0 (width tree) (height tree))))))

;;; ----------------------------------------------------------------------

(defun get-cache-surface ()
  (orf *cache-surface*
       (orf (gethash 'cache-surface (context-attributes *gl-context*))
            (make-cache-surface *gl-context*))))

(defun cached-image (image)
  (orf (gethash (allocant-key image) (cache-surface-map (get-cache-surface)))
       (cache-surface-allocate-image (get-cache-surface) image)))
