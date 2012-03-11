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

(defun use-graphic-projection (&key (square t))
  (assert (not (null *window*)))
  (reset-transforms)
  (let ((aspect  (/ (width *window*)
                    (height *window*))))
    (gl:scale (if (< aspect 1) 1 (/ aspect))
              (if (< aspect 1) aspect 1)
              1.0)))

;;; I thought I needed to do this, but I'm pretty sure I don't. In
;;; case it turns out I do, I'll leave it around for a while.
(defun fix-alpha-for-compositing ()
  (reset-transforms)
  (gl:enable :blend)
  (gl:blend-func-separate :zero :dst-alpha :zero :one)
  (gl:color 1.0 1.0 1.0 1.0)
  (gl:begin :quads)
  (%draw-rect* -1 -1 1 1)
  (gl:end))

(defun call-with-graphics-context (window continuation)
  (begin-paint window)
  (unwind-protect
       (let ((*gl-context* (graphics-context window))
             (*window* window))

         ;; Blending mode for textures with premultiplied alpha:
         (gl:enable :blend)
         (gl:blend-func :one :one-minus-src-alpha)

         (gl:matrix-mode :modelview)
         (gl:disable :texture-2d)
         (gl:disable :depth-test)
         (gl:disable :cull-face)
         (gl:color 1.0 1.0 1.0 1.0)

         (gl:load-identity)
         (use-pixel-projection)
         (funcall continuation)
         (gl:check-error)

         #+(or)
         (when (display-supports-compositing-p *display*)
           (fix-alpha-for-compositing)))
    ;; Cleanup:
    (end-paint window)))

(defmacro with-graphics-context ((window) &body body)
  `(call-with-graphics-context ,window (lambda () ,@body)))

;;;; Little utilities

(defun clear-screen (&optional (color #(0.0 0.0 0.0 0.0)))
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
      ((eql pf +rgb+)   (values 3 :rgb   :unsigned-byte))
      ((eql pf +rgba+)  (values 4 :rgba  :unsigned-byte))
      ((eql pf +alpha+) (values 1 :alpha :unsigned-byte)))))

(defun allocate-texture (&key key mipmap)
  (assert-gl-context)
  (let ((texture (make-instance
                  'opengl-texture
                  :mipmap mipmap
                  :texture-id (first (gl:gen-textures 1)))))
    (gl:check-error)
    (attach-resource *gl-context* texture (or key texture))
    (values texture)))

(defun texture-load (texture image)
  (assert-ownership texture)
  (gl:bind-texture :texture-2d (texture-id texture))
  (gl:check-error)
  (multiple-value-bind (internal format type)
      (opengl-image-formats image)
    (cffi:with-pointer-to-vector-data (pointer (data-array image))
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
                       (width (dimensions image))
                       (height (dimensions image))
                       0
                       format
                       type
                       pointer)
      (gl:check-error)
      (gl:pixel-store :unpack-row-length 0)
      (gl:check-error))))

(defun allocate-and-upload (image &key mipmap)
  (let ((texture (allocate-texture :key image :mipmap mipmap)))
    (texture-load texture image)
    texture))

(defun use-texture (image &key (mipmap t))
  (assert-gl-context)
  (gl:enable :texture-2d)
  (let ((texture (gethash image (resource-map *gl-context*))))
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
       (gl:tex-env :texture-env :texture-env-mode :modulate)
       (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
       (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
       (gl:check-error)))))

#+NIL
(defun texture-push-rectangle (texture-id source-image c0 c1)
  (asserted-gl-context)
  (let ((size (- c1 c0)))
    (multiple-value-bind (internal format type)
        (opengl-image-formats source)
      (gl:bind-texture :texture-2d texture-id)
      )))






