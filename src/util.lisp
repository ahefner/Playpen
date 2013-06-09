;;;; Utilities

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

(in-package :putil)

(define-condition textual-condition ()
  ((text :initarg :text))
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

(defun suffix (symbol suffix)
  (intern (format nil "~A~A" (string symbol) (string suffix))))

(defun keywordify (string-designator)
  (values (intern (string string-designator) :keyword)))

(declaim (inline clamp extend-shift non-null max-or min-or))

(defun non-null (value)
  (assert (not (null value)))
  value)

(define-compiler-macro non-null (expr)
  `((lambda (value)
      (or value
          (error ,(format nil "Must be non-NULL: ~A" expr))))
    ,expr))

(defun clamp (lower value upper) (max lower (min value upper)))

(defun birand (r) (- (random (* 2 r)) r))

(defun min-or (x y)
  (if (and x y)
      (min x y)
      (or x y)))

(defun max-or (x y)
  (if (and x y)
      (max x y)
      (or x y)))

(defun scalec (s c)
  (complex (* (realpart s) (realpart c))
           (* (imagpart s) (imagpart c))))

(defconstant 2pi (* 2 pi))

(defun extend-shift (value count)
  "Shift VALUE left by COUNT bits, extending the least significant bit
to the right (e.g. (extend-shift #b101 3) => #b101111"
  (logior (ash value count)
          (* (logand value 1)
             (1- (ash 1 count)))))

(assert (and (= (extend-shift #b000 0) #b0)
             (= (extend-shift #b001 0) #b1)
             (= (extend-shift #b010 0) #b10)
             (= (extend-shift #b011 0) #b11)
             (= (extend-shift #b000 3) #b0000)
             (= (extend-shift #b001 3) #b1111)
             (= (extend-shift #b101 2) #b10111)
             (= (extend-shift #b100 2) #b10000)))

(define-modify-macro orf (x) or)
(define-modify-macro minf (other) min)
(define-modify-macro maxf (other) max)

;;; Define a structure with a boa constructor of the same
;;; name. Apologies..

;;; FIXME: I have a much cooler DEFTUPLE macro somewhere. It might
;;; even be worth using.
(defmacro deftuple (name &rest slots)
  (flet ((slot-name (slotspec)
           (etypecase slotspec
             (cons (first slotspec))
             (symbol slotspec))))
    `(defstruct (,name (:constructor ,name ,(mapcar #'slot-name slots))) ,@slots)))

;;; Pixel helpers

(declaim (inline float->byte
                 byte->float
                 component*
                 rgba-avg
                 rgba-avg-quad
                 rescale-component
                 xy->row-major-index))

(defun float->byte (float) (clamp 0 (round (* (the single-float float) 255.0f0)) 255))
(defun byte->float (byte)  (/ (float byte 0.0f0) 255.0f0))

(defun rescale-component (x)
  (declare (type (integer 0 254) x))
  (+ x (ldb (byte 1 7) x)))

(defun component* (x y)
  (declare (type (integer 0 255) x y))
  (rescale-component (ash (* x y) -8)))

(defmacro process-components-rgba ((&rest fragment-exprs) (&rest component-vars) expression)
  "Generates code applying an expression to each color channel of the 32-bit RGBA values
   computed by FRAGMENT-EXPRS. Within EXPRESSION, each COMPONENT-VAR is bound to the channel
   value of the corresponding fragment."
  (unless (= (length fragment-exprs) (length component-vars))
    (error "Each fragment expression must have a corresponding component var"))
  (let ((fragment-vars (loop for expr in fragment-exprs collect (gensym "FRAGMENT"))))
    `((lambda ,fragment-vars
        (logior
         ,@(loop for channel-position from 0 below 32 by 8 collect
                 `(ash (let ,(loop for component in component-vars
                                   for fragment in fragment-vars
                                   collect `(,component (ldb (byte 8 ,channel-position) ,fragment)))
                         ,expression)
                       ,channel-position))))
      ,@fragment-exprs)))

;;; FIXME: This are both wrong, due to gamma curves.
(defun rgba-avg (x y)
  (declare (type (unsigned-byte 32) x y))
  (process-components-rgba (x y) (x y) (ash (+ x y) -1)))

(defun rgba-avg-quad (x y z w)
  (declare (type (unsigned-byte 32) x y z w))
  (process-components-rgba (x y z w) (x y z w) (ash (+ x y z w) -2)))

(defun xy->row-major-index (array x y)
  (+ x (* (array-dimension array 1) y)))

;;; TODO: The uses of loop-rectangle so far contain a fair amount of
;;; redundancy. It would be nicer to add a syntax for the var-specs
;;; which would establish pitch and offset for a 2D array
;;; automatically.

(defmacro loop-rectangle ((width-expr height-expr &optional (x-var (gensym "X")) (y-var (gensym "Y")))
                          (&rest var-specs)
                          &body body)
  (destructuring-bind (width height) (loop repeat 2 collect (gensym))
    (let ((wrap-vars (loop repeat (length var-specs) collect (gensym "WRAP"))))
      `(let ((,width ,width-expr)
             (,height ,height-expr))
         (let ,(loop for var-spec in var-specs
                     for wrap-var in wrap-vars
                     for index upfrom 0
                     nconcing (list `(,(first var-spec) ,(getf (rest var-spec) :offset 0))
                                    `(,wrap-var (- ,(getf (rest var-spec) :pitch width) ,width))))
           (loop for ,x-var from 0 below ,height do
                 (loop for ,y-var from 0 below ,width do
                       (tagbody ,@body)
                       ,@(loop for index-var in (mapcar #'first var-specs)
                               for index upfrom 0
                               collect `(incf ,index-var)))
                 ,@(loop for index-var in (mapcar #'first var-specs)
                         for index upfrom 0
                         collect `(incf ,index-var ,(nth index wrap-vars)))))))))

;;;; ------------------------------------------------------------------
;;;; File names

(deftype bytevec () '(simple-array (unsigned-byte 8) 1))

(defclass filename ()
  ((monkey-string :initform nil :accessor %monkey-string)
   (machine-string :initform nil :accessor %machine-strhing)
   (vector :reader filename-vector :initarg :vector)))

(defun human-string (filename)
  (etypecase filename
    (string filename)
    (pathname
     (sb-ext:native-namestring filename))
    (filename
     (let ((vector (filename-vector filename)))
       (or (%monkey-string filename)
           (setf (%monkey-string filename)
                 (etypecase vector
                   (bytevec
                    (or
                     (ignore-errors
                      (sb-ext:octets-to-string vector :external-format :utf-8))
                     (sb-ext:octets-to-string vector :external-format :latin-1))))))))))

(defun machine-string (filename)
  (etypecase filename
    (string
     ;; This code is not the problem. We are the problem.
     ;; Fuck off, dear reader.
     (sb-ext:octets-to-string
      (sb-ext:string-to-octets filename :external-format :utf-8)
      :external-format sb-impl::*default-external-format*))
    (pathname
     ;;(warn "You're going to lose. Maybe not today, maybe not tomorrow, but some day.")
     (sb-ext:native-namestring filename))
    (filename
     (unless (eql sb-impl::*default-external-format* :latin-1)
       (warn "Don't say I didn't warn you."))
     (sb-ext:octets-to-string
      (filename-vector filename)
      :external-format sb-impl::*default-external-format*))))

(defmacro with-filename-pointer ((pointer-to filename) &body body)
  `((lambda (filename continuation)
      (etypecase filename
        (string
         (cffi:with-foreign-string (ptr filename)
           (funcall continuation ptr)))
        (pathname
         (cffi:with-foreign-string
             (ptr (cffi-sys:native-namestring filename))
           (funcall continuation ptr)))
        (filename
         (cffi:with-pointer-to-vector-data
             (ptr (filename-vector filename))
           (funcall continuation ptr)))))
    ,filename
    (lambda (,pointer-to) ,@body)))

;;;; Comparisons of complex numbers as points in the plane.

(defun p<<= (a b)
  (and (<= (realpart a) (realpart b))
       (<= (imagpart a) (imagpart b))))

(defun p>>= (a b)
  (and (>= (realpart a) (realpart b))
       (>= (imagpart a) (imagpart b))))

(defun rectangle-contains-p (upper-left lower-right point)
  (and
   (p<<= upper-left point)
   (p<<= point lower-right)))

;;;; C idiocy

;;; cffi-grovel? I've heard of such things..
#-CFFI-FEATURES:X86-64
(cffi:defctype size_t :unsigned-int)
#+CFFI-FEATURES:X86-64
(cffi:defctype size_t :uint64)

#-CFFI-FEATURES:X86-64
(cffi:defctype off_t :int)
#+CFFI-FEATURES:X86-64
(cffi:defctype off_t :int64)

(cffi:defcfun malloc :pointer
  (size size_t))

(cffi:defcfun free :void
  (ptr :pointer))

(cffi:defcfun calloc :pointer
  (nmemb size_t)
  (size size_t))

(cffi:defcfun memset :void
  (s :pointer)
  (c :int)
  (n size_t))


(defun underlying-vector (array)
  (typecase array
    ((simple-array * 1) array)
    (t (sb-kernel:%array-data-vector array))))

(defmacro with-array-pointer ((pointer-var array-expr) &body body)
  `(cffi:with-pointer-to-vector-data
       (,pointer-var (underlying-vector ,array-expr))
     ,@body))
