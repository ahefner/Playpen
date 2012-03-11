;;;; Pixels and such

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

;;; There used to be a fair amount of code here. I've been able to
;;; move or eliminate most of it.

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






