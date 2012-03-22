;;;; Image file loading

;;;; Copyright (c) 2008, 2012, Andy Hefner <ahefner@gmail.com>

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

;;;; ------------------------------------------------------------
;;;; JPEG Loader
;;;; ------------------------------------------------------------

(cffi:defcenum jpeg-decoder-state :uninitialized :error :warning :working :finished)

;;; There are more slots to this, but they're private.
(cffi:defcstruct jpeg-decoder
  (state jpeg-decoder-state)
  (input-width   :unsigned-int)
  (input-height  :unsigned-int)
  (output-width  :unsigned-int)
  (output-height :unsigned-int)
  (buffer :char :count 200))            ; JMSG_LENGTH_MAX

(cffi:defcfun jlo-new-decoder :pointer)

(cffi:defcfun jlo-free-decoder :void
  (decoder :pointer))

(cffi:defcfun jlo-work :void
  (decoder :pointer)
  (maxrows :int))

(cffi:defcfun (%jlo-init-from-filename "jlo_init_from_filename")
    :boolean
  (decoder :pointer)
  (filename :pointer))

(cffi:defcfun jlo-start-decompress :int
  (decoder :pointer))

(cffi:defcfun jlo-mode-fastest :void
  (decoder :pointer))

(cffi:defcfun jlo-mode-high-quality :void
  (decoder :pointer))

(cffi:defcfun (%jlo-set-downscale "jlo_set_downscale") :void
  (decoder :pointer)
  (denominator :int))

(cffi:defcfun (%jlo-copy-output "jlo_copy_output") :void
  (decoder :pointer)
  (output :pointer))

(defun jlo-set-downscale (decoder denominator)
  (ecase denominator
    ((1 2 4 8)
     (%jlo-set-downscale decoder denominator))))

(defun jlo-init-from-file (decoder filename)
  (etypecase filename
    (cffi:foreign-pointer
     (%jlo-init-from-filename decoder filename))
    (string
     (warn "You deserve to lose.")
     (cffi:with-foreign-string (ptr filename)
       (%jlo-init-from-filename decoder ptr)))
    (pathname
     (warn "You deserve to lose.")
     (jlo-init-from-file
      decoder
      (sb-ext:native-namestring filename)))))

(define-condition jpeg-decoder-error (error textual-condition)
  ())

(define-condition jpeg-decoder-warning (warning textual-condition)
  ())

(defun jpeg-decoder-message (decoder)
  (or
   (ignore-errors
    (cffi:convert-from-foreign
     (cffi:foreign-slot-value decoder 'jpeg-decoder 'buffer)
     :string))
   "(internal error)"))

(defun jlo-copy-output (decoder)
  (cffi:with-foreign-slots ((output-width output-height) decoder jpeg-decoder)
    (let ((vector (make-array (* output-width output-height)
                              :element-type '(unsigned-byte 32)
                              :adjustable nil
                              :fill-pointer nil)))
      (cffi:with-pointer-to-vector-data (pointer vector)
        (%jlo-copy-output decoder pointer))
      (values vector))))

(defun read-jpeg-file (filename &key (configurator 'identity) (error t))
  (let ((decoder (jlo-new-decoder))
        (batch-size 100))
    (macrolet ((slot (name)
                 `(cffi:foreign-slot-value decoder 'jpeg-decoder ',name)))
      (unwind-protect
           (handler-bind ((jpeg-decoder-error
                            (lambda (c)
                              (declare (ignore c))
                              (unless error
                                (return-from read-jpeg-file nil)))))
             ;; Init JPEG decoder.
             (with-filename-pointer (ptr filename)
               (jlo-init-from-file decoder ptr))
             (funcall configurator decoder)
             (jlo-start-decompress decoder)
             ;; Loop until decoder is in :finished state.
             (loop
              (ecase (slot state)
                (:error
                 (error 'jpeg-decoder-error :text (jpeg-decoder-message decoder)))
                (:warning
                 (warn 'jpeg-decoder-warning :text (jpeg-decoder-message decoder))
                 (jlo-work decoder batch-size))
                (:working
                 (jlo-work decoder batch-size))
                (:finished
                 (return
                   #+NIL
                   (list :width  (slot output-width)
                         :height (slot output-height))
                   (image-vector
                    (slot output-width)
                    (slot output-height)
                    +rgba+
                    :data
                    (jlo-copy-output decoder)))))))
        ;; Cleanup:
        (jlo-free-decoder decoder)))))

(defun compare-jpg-signature (sig)
  ;; This is a little too permissive..
  (and (eql #xFF (aref sig 0))
       (eql #xD8 (aref sig 1))))

;;;; ------------------------------------------------------------
;;;; GIF Loader (via SKIPPY)
;;;; ------------------------------------------------------------

(defun convert-gif-data (table bg data)
  (map '(simple-array (unsigned-byte 32) 1)
       (lambda (x)
         (let ((tmp
                (if (eql x bg)
                    0
                    (logior #xFF000000
                            (aref table x)))))
           (rotatef (ldb (byte 8 0) tmp)
                    (ldb (byte 8 16) tmp))
           tmp))
       data))

(defun read-gif-file (filename &key (error t))
  (handler-bind
      ((file-error
        (lambda (c)
          (declare (ignore c))
          (unless error (return-from read-gif-file nil))))
       (skippy:skippy-error
        (lambda (c)
          (declare (ignore c))
          (unless error (return-from read-gif-file nil))))
       (skippy:skippy-warning #'muffle-warning))
   (let* ((ds (skippy:load-data-stream
               (machine-string filename)))
          (img (elt (skippy:images ds) 0))
          (bg (skippy:transparency-index img))
          (table (skippy::entries
                  (skippy:color-table ds))))
     (image-vector (skippy:width img)
                   (skippy:height img)
                   +rgba+
                   :data
                   (convert-gif-data table bg (skippy:image-data img))))))

(defun compare-gif-signature (sig)
  (or (eql 6 (mismatch sig #(71 73 70 56 55 97)))
      (eql 6 (mismatch sig #(71 73 70 56 57 97)))))

;;;; ------------------------------------------------------------
;;;; PNG Loader
;;;; ------------------------------------------------------------

(cffi:defcstruct png-reader
  (width      :unsigned-int)
  (height     :unsigned-int)
  (channels   :unsigned-int)
  (png-struct :pointer)
  (png-info   :pointer))

(cffi:defcfun free-png-reader :void
  (png-reader :pointer))

(cffi:defcfun (%open-and-decode-png-file "open_and_decode_png_file")
    :pointer
  (filename :pointer))

(cffi:defcfun (%png-copy-output "png_copy_output")
    :void
  (png-reader :pointer)
  (destination :pointer))

(defun png-copy-output (png-reader)
  (assert (not (cffi:null-pointer-p png-reader)))
  (cffi:with-foreign-slots ((width height)
                            png-reader
                            png-reader)
    (assert (not (zerop width)))
    (assert (not (zerop height)))
    (let ((data (make-array (* width height)
                            :element-type '(unsigned-byte 32)
                            :adjustable nil
                            :fill-pointer nil)))
      (cffi:with-pointer-to-vector-data (pointer data)
        (%png-copy-output png-reader
                          pointer))
      data)))

(define-condition png-error (error textual-condition) ())

(defun read-png-file (filename &key (error t))
  (handler-bind
      ((png-error (lambda (c)
                    (declare (ignore c))
                    (unless error (return-from read-png-file nil)))))
   (with-filename-pointer (ptr filename)
     (let ((png-reader (%open-and-decode-png-file ptr)))
       (cond
         ((cffi:null-pointer-p png-reader)
          (error 'png-error
                 :text (format nil "Unable to read PNG file ~A"
                               (human-string filename))))
         (t
          (unwind-protect
               (cffi:with-foreign-slots ((width height channels)
                                         png-reader
                                         png-reader)
                 (cond
                   ((or (zerop width) (zerop height))
                    (error "Zero pixel PNG image? This probably shouldn't happen."))
                   (t
                    (image-vector
                     width
                     height
                     +rgba+
                     :data (png-copy-output png-reader)))))
            ;; Cleanup:
            (free-png-reader png-reader))))))))

(cffi:defcfun (%compare-png-signature "compare_png_signature") :boolean
  (header :pointer))

(defun compare-png-signature (header)
  (check-type header (simple-array (unsigned-byte 8) 1))
  (cffi:with-pointer-to-vector-data (ptr header)
    (%compare-png-signature ptr)))

;;;; ------------------------------------------------------------
;;;; Generic image loader
;;;; ------------------------------------------------------------

(cffi:defcfun (%read-file-signature "read_file_signature")
    :boolean
  (filename :pointer)
  (array    :pointer))

(defun read-file-signature (filename)
  (let ((signature (make-array 16
                               :element-type '(unsigned-byte 8)
                               :adjustable nil
                               :fill-pointer nil)))
    (with-filename-pointer (fptr filename)
      (cffi:with-pointer-to-vector-data (sptr signature)
       (%read-file-signature fptr sptr)))
    (values signature)))

(defun identify-signature (sig)
  (cond
    ((compare-jpg-signature sig) :jpg)
    ((compare-png-signature sig) :png)
    ((compare-gif-signature sig) :gif)
    (t nil)))

(defun read-image-file (filename &key (error t))
  (ecase (or (identify-signature
              (read-file-signature filename))
             :unknown)
    (:jpg (read-jpeg-file filename :error error))
    (:gif (read-gif-file filename :error error))
    (:png (read-png-file filename :error error))
    (:unknown
     (when error
       (error "Can't determine image format of ~A" filename)))))
