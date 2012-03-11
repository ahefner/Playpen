;;;; Drawing functions and OpenGL state management

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

(in-package :playpen)

;;; The way I imagine the drawing functions working is that they (in
;;; conjunction with the cache surface) will work to minimize OpenGL
;;; state changes. Drawing operations are coalesced into vertex arrays
;;; and dispatched in batches when state changes are necessary.

(defvar *opengl-owner*
  (lambda (request)
    (declare (ignore request))
    (error "Graphics have not been initialized in the current thread.")))

(defun opengl-locked-error (continuation)
  (declare (ignore continuation))
  (error "Recursive with-opengl with no available flush/restore handler"))

;;; with-opengl: Acquire control of OpenGL. This allows us to buffer
;;; drawing operations and cleanly make assumptions about the current
;;; OpenGL state. Eventually this will grow some hairy keywords
;;; providing more detail over how the body will use OpenGL, to
;;; minimize state changes in the common case of the owner being
;;; Playpen's toplevel state manager.

(defmacro with-opengl ((&key (monitor '#'opengl-locked-error)) &body body)
  `(flet ((body () (gl:check-error) ,@body))
     (declare (dynamic-extent #'body))
     (let ((owner *opengl-owner*)
           (*opengl-owner* ,monitor))
       (funcall owner #'body))))

;;; with-graphics: Initialize shared graphics state, so that built-in
;;; drawing functions may be used. Callled by playpen toplevel-loop,
;;; not needed in user code.

(defmacro with-graphics (() &body body)
  `(let ((*opengl-owner* 'funcall))     ; fixme!
     ,@body))

(defun %draw-rect (x0 y0 x1 y1 &optional (z 0.0f0) (u0 0.0) (v0 0.0) (u1 1.0) (v1 1.0))
  "Draw an optionally textured rectangle. Assumes OpenGL is already drawing quads."
  (gl:tex-coord u0 v0)
  (gl:vertex x0 y0 z)
  (gl:tex-coord u1 v0)
  (gl:vertex x1 y0 z)
  (gl:tex-coord u1 v1)
  (gl:vertex x1 y1 z)
  (gl:tex-coord u0 v1)
  (gl:vertex x0 y1 z))

