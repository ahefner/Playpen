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
                         :initial-element 0
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
