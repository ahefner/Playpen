;;;; Pixel formats and images

;;;; Copyright (c) 2008-2012, Andy Hefner <ahefner@gmail.com>

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

;;; A pixel format describes how to obtain color component values
;;; from an object.

;;; Pixel formats must adhere to the following protocol:

(in-package :playpen)

(defgeneric pixel-format-element-type (pixel-format)
  (:documentation "Returns the preferred element-type for arrays of pixels in this format")
  (:method (pixel-format) t))

(defgeneric decode-pixel-float (pixel-format pixel)
  (:documentation "Given a pixel of the specified format, decodes its color components, returning the its red, green, blue, and alpha components as single-float values in that order.")
  (:method (pixel-format pixel)
    (multiple-value-bind (r g b a) (decode-pixel-integer pixel-format pixel)
        (values (byte->float r)
                (byte->float g)
                (byte->float b)
                (byte->float a)))))

(defgeneric decode-pixel-integer (pixel-format pixel)
  (:documentation "Given a pixel of the specified format, decodes its color components, returning the its red, green, blue, and alpha components as (unsigned-byte 8) values in that order.")
  (:method (pixel-format pixel)
    (multiple-value-bind (r g b a) (decode-pixel-float pixel-format pixel)
      (values (float->byte r)
              (float->byte g)
              (float->byte b)
              (float->byte a)))))

(defgeneric pixel-red-f (pixel-format pixel)
  (:documentation "Decode the red component of a pixel, returning a single-float value.")
  (:method (pixel-format pixel) (nth-value 0 (decode-pixel-float pixel-format pixel))))

(defgeneric pixel-green-f (pixel-format pixel)
  (:documentation "Decode the green component of a pixel, returning a single-float value.")
  (:method (pixel-format pixel) (nth-value 1 (decode-pixel-float pixel-format pixel))))

(defgeneric pixel-blue-f (pixel-format pixel)
  (:documentation "Decode the blue component of a pixel, returning a single-float value.")
  (:method (pixel-format pixel) (nth-value 2 (decode-pixel-float pixel-format pixel))))

(defgeneric pixel-alpha-f (pixel-format pixel)
  (:documentation "Decode the alpha component of a pixel, returning a single-float value.")
  (:method (pixel-format pixel) (nth-value 3 (decode-pixel-float pixel-format pixel))))

(defgeneric pixel-red-i (pixel-format pixel)
  (:documentation "Decode the red component of a pixel, returning an (unsigned-byte 8) value.")
  (:method (pixel-format pixel) (nth-value 0 (decode-pixel-integer pixel-format pixel))))

(defgeneric pixel-green-i (pixel-format pixel)
  (:documentation "Decode the green component of a pixel, returning an (unsigned-byte 8) value.")
  (:method (pixel-format pixel) (nth-value 1 (decode-pixel-integer pixel-format pixel))))

(defgeneric pixel-blue-i (pixel-format pixel)
  (:documentation "Decode the blue component of a pixel, returning an (unsigned-byte 8) value.")
  (:method (pixel-format pixel) (nth-value 2 (decode-pixel-integer pixel-format pixel))))

(defgeneric pixel-alpha-i (pixel-format pixel)
  (:documentation "Decode the alpha component of a pixel, returning an (unsigned-byte 8) value.")
  (:method (pixel-format pixel) (nth-value 3 (decode-pixel-integer pixel-format pixel))))

;;; Generic bitfield pixel format

(defstruct bf-format
    red-position   red-size   red-bias
  green-position green-size green-bias
   blue-position  blue-size  blue-bias
  alpha-position alpha-size alpha-bias)

(defvar *bitfield-pixel-formats* (make-hash-table :test 'equal))

(defun intern-bf-format (&rest args)
  (destructuring-bind
        #1=(red-position   red-size   red-bias
            green-position green-size green-bias
            blue-position  blue-size  blue-bias
            alpha-position alpha-size alpha-bias) args
    (or (gethash args *bitfield-pixel-formats*)
        (setf (gethash args *bitfield-pixel-formats*)
              (make-bf-format . #.(mapcan (lambda (x) (list (keywordify x) x)) '#1#))))))

(defmethod decode-pixel-integer ((format bf-format) pixel)
  (macrolet ((channel (name) `(+ (,(suffix name "-BIAS") format)
                                 (extend-shift
                                  (ldb (byte (,(suffix name "-SIZE") format)
                                             (,(suffix name "-POSITION") format))
                                       pixel)
                                  (- 8 (,(suffix name "-SIZE") format))))))
    (values (channel bf-format-red)
            (channel bf-format-green)
            (channel bf-format-blue)
            (channel bf-format-alpha))))

(defun rgb (red-bits green-bits blue-bits)
  (intern-bf-format (+ green-bits blue-bits) red-bits 0
                    blue-bits green-bits 0
                    0 blue-bits 0
                    0 0 255))

(defun rgba (red-bits green-bits blue-bits alpha-bits)
  (intern-bf-format (+ green-bits blue-bits alpha-bits) red-bits 0
                    (+ blue-bits alpha-bits) green-bits 0
                    alpha-bits blue-bits 0
                    0 alpha-bits 0))

;;; Predefined pixel formats

(defvar +rgb+ (rgb 8 8 8))
(defvar +rgba+ (rgba 8 8 8 8))
(defvar +alpha+ (intern-bf-format 0 0 255 0 0 255 0 0 255 0 8 0))

(defmethod pixel-format-element-type ((pixel-format (eql +rgb+))) '(unsigned-byte 32))
(defmethod pixel-format-element-type ((pixel-format (eql +rgba+))) '(unsigned-byte 32))
(defmethod pixel-format-element-type ((pixel-format (eql +alpha+))) '(unsigned-byte 8))

;;;; Images

;;; An image combines a pixel format with a description of how they
;;; are arranged (or can be obtained) from an object (e.g. as a 2D
;;; array, or a vector). Unlike pixel formats, the image array is
;;; bundled together with the description of its format. This is
;;; probably a mistake.

;;; Images implement the WIDTH/HEIGHT/DIMENSIONS functions, plus the following:
(defgeneric pixel-format (object))
(defgeneric image-pitch (image))
(defgeneric image-offset (image))
(defgeneric image-row-major-offset (image x y))
(defgeneric image-row-major-offset-bytes (image x y))
(defgeneric data-array (image))

;;; Images as vectors:

(defclass image-vector ()
   ((data   :initarg :data   :reader data-array)
    (width  :initarg :width  :reader width)
    (height :initarg :height :reader height)
    (pitch  :initarg :pitch  :reader image-pitch)
    (offset :initarg :offset :reader image-offset)
    (pixel-format :initarg :pixel-format :reader pixel-format)))

(defun image-vector (width height pixel-format
                     &key (offset 0) (pitch width) data
                     (element-type (pixel-format-element-type pixel-format)))
  (make-instance 'image-vector
                 :offset offset
                 :width width
                 :height height
                 :pitch pitch
                 :pixel-format pixel-format
                 :data (or data
                           (make-array (+ offset (* pitch height))
                                       :initial-element 0
                                       :adjustable nil
                                       :fill-pointer nil
                                       :element-type element-type))))

(defmethod image-row-major-offset ((image image-vector) x y)
  (+ (image-offset image)
     (* y (image-pitch image))
     x))

(defmethod image-row-major-offset-byte ((image image-vector) x y)
  (* (image-row-major-offset image x y)
     (let ((pf (pixel-format image)))
       (cond
         ((equal pf '(unsigned-byte 32)) 4)
         ((equal pf '(unsigned-byte 8)) 1)
         (t (error "Element-type must be one of (unsigned-byte 32) or (unsigned-byte 8)"))))))

;;; Images as 2D arrays

(defstruct image-matrix data pixel-format)

(defun image-matrix (pixel-format &key width height data
                     (element-type (pixel-format-element-type pixel-format) e-t-p))
  (when (and data (or width height e-t-p))
    (error "Data argument precludes width, height, and element-type"))
  (make-image-matrix
   :pixel-format pixel-format
   :data (or data
             (make-array (list height width)
                         :element-type element-type))))

(defmethod width ((image image-matrix))
  (array-dimension (image-matrix-data image) 1))

(defmethod height ((image image-matrix))
  (array-dimension (image-matrix-data image) 0))

(defmethod image-pitch ((image image-matrix))
  (array-dimension (image-matrix-data image) 1))

(defmethod image-offset ((image image-matrix)) 0)

(defmethod pixel-format ((image image-matrix)) (image-matrix-pixel-format image))

(defmethod image-row-major-offset ((image image-matrix) x y)
  (+ x (* y (array-dimension (image-matrix-data image) 1))))

(defmethod image-row-major-offset-byte ((image image-matrix) x y)
  (* (image-row-major-offset image x y)
     (let ((pf (array-element-type (image-matrix-data image))))
       (cond
         ((equal pf '(unsigned-byte 32)) 4)
         ((equal pf '(unsigned-byte 8)) 1)
         (t (error "Element-type must be one of (unsigned-byte 32) or (unsigned-byte 8)"))))))

(defmethod data-array ((image image-matrix))
  (image-matrix-data image))


;;;; Random pixel munging stuff

;;; TODO: Rewrite using loop-rectangle, pixel format macros

(defun a8-to-rgba32 (mask rgb-color)
  (declare (type (simple-array (unsigned-byte 8) 2) mask))
  (let* ((width  (array-dimension mask 1))
         (height (array-dimension mask 0))
         (rgba (make-array (list height width)
                           :element-type '(unsigned-byte 32))))
    (loop for i from 0 below height do
          (loop for j from 0 below width
                as alpha = (aref mask i j) do
                (setf (aref rgba i j)
                      (logior (ash alpha 24) rgb-color))))
    rgba))

(defun copy-a8-rectangle-to-rgba32-image (width height source sx sy destination dx dy)
  (declare (type (simple-array (unsigned-byte 32) 2) destination)
           (type (simple-array (unsigned-byte 8) 2) source))
  (loop-rectangle (width height)
     ((source-index :pitch (array-dimension source 1)
                    :offset (xy->row-major-index source sx sy))
      (dest-index :pitch (array-dimension destination 1)
                  :offset (xy->row-major-index destination dx dy)))
   (setf (row-major-aref destination dest-index)
         (dpb (row-major-aref source source-index) (byte 8 24) #xFFFFFF)))
  destination)


;; Disable array type declaration in copy-a8-rectangle-to-rgba32-image to try this:
#+NIL
(assert
 (equalp (copy-a8-rectangle-to-rgba32-image
          2 3
          #2a((#x90 #x03 #x03 #x50 #x56 #x70)
              (#x90 #xff #x20 #x05 #x45 #x30)
              (#x90 #x80 #x10 #x50 #x34 #x76)
              (#x09 #x80 #xff #x03 #x23 #x76)
              (#x09 #x00 #x00 #x20 #x12 #x74)) 1 1
          #2a((#x00500000 #x00600000 #x03030000 #x01220000 #x00200000 #x01200000 #x01200000)
              (#x00005000 #x00000030 #x00000330 #x00000012 #x00002000 #x00023200 #x00200000)
              (#x04000000 #x00003000 #x00000300 #x00012000 #x00100000 #x00003200 #x00200000)
              (#x00040000 #x00500000 #x00402000 #x12000000 #x88888888 #x33333333 #x00020000)
              (#x00000200 #x00050000 #x00003200 #x00000120 #x77777777 #x33333333 #x00002200)
              (#x00600000 #x00000200 #x03200000 #x00210000 #x66666666 #x11111111 #x00000300)
              (#x00004000 #x00020000 #x00034000 #x10000000 #x55555555 #x11111111 #x00230000)) 4 3)

         #2a((#x00500000 #x00600000 #x03030000 #x01220000 #x00200000 #x01200000 #x01200000)
             (#x00005000 #x00000030 #x00000330 #x00000012 #x00002000 #x00023200 #x00200000)
             (#x04000000 #x00003000 #x00000300 #x00012000 #x00100000 #x00003200 #x00200000)
             (#x00040000 #x00500000 #x00402000 #x12000000 #xffffffff #x20ffffff #x00020000)
             (#x00000200 #x00050000 #x00003200 #x00000120 #x80ffffff #x10ffffff #x00002200)
             (#x00600000 #x00000200 #x03200000 #x00210000 #x80ffffff #xffffffff #x00000300)
             (#x00004000 #x00020000 #x00034000 #x10000000 #x55555555 #x11111111 #x00230000))))

(defun array-fill-rectangle (array x0 y0 x1 y1 value)
  (loop-rectangle ((- x1 x0) (- y1 y0))
   ((index :pitch (array-dimension array 1)
           :offset (xy->row-major-index array x0 y0)))
   (setf (row-major-aref array index) value)))
