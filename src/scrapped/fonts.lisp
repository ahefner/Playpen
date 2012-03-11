;;;; Font support for playpen

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

(defun font-path () #P"/usr/share/fonts/truetype/ttf-dejavu/")
(defparameter *base-em-pixels* 64)

(defgeneric typeface-ensure-glyph (typeface char-code))

(defclass typeface () ())

;; Wow, this is completely pointless. I've stripped out all the other slots,
;; now it's just a wrapper around the font loader. Feh.
(defclass truetype-face (typeface)
  ((ttf-loader :reader ttf-loader :initarg :font-loader)
   (glyph-map :reader glyph-map
              :initform (make-array 256
                                    :initial-element nil
                                    :adjustable nil
                                    :fill-pointer nil))))

(defvar *font-loaders* (make-hash-table :test 'equal))

(defmethod ttf-loader ((filename string))
  (or (gethash filename *font-loaders*)
      (setf (gethash filename *font-loaders*)
            (zpb-ttf:open-font-loader filename))))

(defmethod ttf-loader ((name pathname)) (ttf-loader (namestring name)))

(defun render-truetype-glyph (font-loader char &optional (size *base-em-pixels*))
  "Render the glyph of a font. It will be rendered to an arbitrary pixel size
   where one Em is converted to 'size' pixels."
  (declare (optimize (debug 3)))
  (let* ((units->pixels (/ size (zpb-ttf:units/em font-loader)))
         (glyph (zpb-ttf:find-glyph char font-loader))
         ;;(left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
         ;;(right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
         (advance-width (float (* units->pixels (zpb-ttf:advance-width glyph))))
         (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing glyph)))
         (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                            (zpb-ttf:bounding-box glyph)))
         (min-x (elt bounding-box 0))
         (min-y (elt bounding-box 1))
         (max-x (elt bounding-box 2))
         (max-y (elt bounding-box 3))
         (width  (1+ (- (ceiling max-x) (floor min-x))))
         (height (1+ (- (ceiling max-y) (floor min-y))))
         (array (make-array (list height width)
                            :initial-element 0
                            :element-type '(unsigned-byte 8)))
         (state (aa:make-state))
         (paths (paths-ttf:paths-from-glyph 
                 glyph
                 :offset (paths:make-point (- (floor min-x)) (ceiling max-y))
                 :scale-x units->pixels
                 :scale-y (- units->pixels))))
    (dolist (path paths)
      (vectors:update-state state path))

    ;; This can be done more efficiently, but these are cached, so it 
    ;; shouldn't be a bottleneck.
    (aa:cells-sweep state
      (lambda (x y alpha)
        (when (and (<= 0 x (1- width))
                   (<= 0 y (1- height)))
          (setf alpha (min 255 (abs alpha))
                (aref array y x) (clamp
                                  (floor (+ (* (- 256 alpha) (aref array y x))
                                            (* alpha 255))
                                         256)
                                  0 255)))))
    (values (image-matrix +alpha+ :data array)
            (floor min-x)           ; Pixels to origin of glyph coordinate system
            (ceiling max-y)
            advance-width
            #+NIL :ascender (float (/ (zpb-ttf:ascender font-loader) (zpb-ttf:units/em font-loader)))
            #+NIL :descender (float (/ (zpb-ttf:descender font-loader) (zpb-ttf:units/em font-loader)))
            #+NIL :left-side-bearing (float left-side-bearing)
            (/ *base-em-pixels*))))    ; Conversion from pixels to Em

(defstruct
    (cached-glyph
      (:constructor cached-glyph
                    (code image offset-x offset-y advance-width
                    ascender descender left-side-bearing pixels->em)))
  code image
  offset-x offset-y
  advance-width ascender descender left-side-bearing
  pixels->em leaf)

(defun make-cached-truetype-glyph (typeface char-code)
  (multiple-value-call #'cached-glyph
    char-code
    (render-truetype-glyph
     (ttf-loader typeface) (code-char char-code) *base-em-pixels*)))

(defmethod typeface-ensure-glyph ((typeface truetype-face) char-code)
  (let ((map (glyph-map typeface)))
    (cond
      ((<= (length map) char-code)
       ;; Glyph map is too small; grow it and try again.
       (with-slots (glyph-map) typeface
         (setf glyph-map (make-array (+ char-code 256)
                                     :initial-element nil
                                     :fill-pointer nil
                                     :adjustable nil))
         (replace glyph-map map))
       (typeface-ensure-glyph typeface char-code))
      (t (or (aref map char-code)
             (setf (aref map char-code)
                   (make-cached-truetype-glyph typeface char-code)))))))

(defgeneric find-text-face (namespec face))

(defparameter *text-faces* (make-hash-table :test 'equal))

(defun text-face (family-or-filename &optional face)  
  (let ((key (list family-or-filename face)))
    (setf (gethash key *text-faces*)
          (gethash key *text-faces* 
                   (find-text-face family-or-filename face)))))

(defmethod find-text-face ((this pathname) face)
  (unless (null face)
    (error "Can't specify faces when directly loading a TTF file."))
  (make-instance 'truetype-face :font-loader (ttf-loader this)))

(defun canonical-face (face)
  "Canonicalize face specifier for standard typefaces"
  (cond
    ((equal face '(:bold :oblique)) :bold-italic)
    ((equal face '(:oblique :bold)) :bold-italic)
    ((equal face '(:bold :italic)) :bold-italic)
    ((equal face '(:italic :bold)) :bold-italic)
    (t (or face :roman))))

(defun standard-font-filename (base-filename oblique-p face)
 (setf face (canonical-face face))
  (if (eql face :roman)
      (format nil "~A.ttf" base-filename)
      (format nil "~A-~A~A.ttf"
              base-filename
              (ecase face 
                ((:bold :bold-italic) "Bold")
                (:italic ""))
              (ecase face
                ((:italic :bold-italic) 
                 (if oblique-p "Oblique" "Italic"))
                (:bold "")))))

(defun standard-font-pathname (base-filename oblique-p face)
  (merge-pathnames 
   (pathname (standard-font-filename base-filename oblique-p face))
   (font-path)))

(defmethod find-text-face ((family (eql :sans)) face)
  (find-text-face (standard-font-pathname "DejaVuSans" t face) nil))

(defmethod find-text-face ((family (eql :serif)) face)
  (find-text-face (standard-font-pathname "DejaVuSerif" nil face) nil))

(defmethod find-text-face ((family (eql :fixed)) face)
  (find-text-face (standard-font-pathname "DejaVuSansMono" t face) nil))
