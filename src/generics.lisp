;;;; Generic functions / protocols

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

;;;; Dimension protocol

(defgeneric width (object)
  (:documentation "Returns the width of an object."))

(defgeneric height (object)
  (:documentation "Returns the height of an object."))

(defgeneric dimensions (object)
  (:documentation "Returns the dimensions of an object as a complex
  number, WIDTH as the real part and HEIGHT as the imaginary part.")
  (:method (object)
    (complex (width object)
             (height object))))

(defgeneric area (object)
  (:method (object)
    (* (width object) (height object)))
  (:method ((number complex))
    (* (realpart number) (imagpart number))))

(defgeneric aspect-ratio (object)
  (:method (object)
    (float (/ (width object)
              (height object)))))

(defmethod width  ((x complex)) (realpart x))
(defmethod height ((x complex)) (imagpart x))

(defclass dimension-trait ()
  ((dimensions :accessor dimensions-of :initarg :dimensions)))

(defmethod dimensions ((this dimension-trait)) (dimensions-of this))

(defmethod width  ((x dimension-trait)) (realpart (dimensions-of x)))
(defmethod height ((x dimension-trait)) (imagpart (dimensions-of x)))

(defgeneric coordinate (object))

