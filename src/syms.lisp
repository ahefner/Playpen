
(in-package :pwin)

(defvar *keysym-map*
  (let ((table (make-hash-table))
        (val 1))
    (loop for sym-spec in
          '(:up :down :left :right
            :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12
            :tab :escape :return
            :control :meta :alt :super :hyper :shift
            :backspace :delete :home :end :pageup :pagedown
            :print :pause)
          as symbol = (if (consp sym-spec) (first sym-spec) sym-spec)
          do
          (when (consp sym-spec) (setf val (second sym-spec)))
          (setf (gethash val table) symbol)
          (incf val))
    table))

(defun dump-table (table header-name typename symfn valfn)
  (with-open-file (out (format nil "~A.h" header-name)
                       :direction :output
                       :if-exists :supersede)
    (let ((include-guard-sym (format nil "__PLAYPEN_~A_H" (string-upcase header-name)))
          (first t))
      (format out "#ifndef ~A~%#define ~A~%~%" include-guard-sym include-guard-sym)
      (format out "/* This file was automatically generated. Do not edit! */~%~%")
      (format out "enum ~A {~%" typename)
      (format out "    ")
      (maphash (lambda (val sym)
                 (unless first (format out ",~%    "))
                 (setf first nil)
                 (format out "~A = ~A" (funcall symfn sym) (funcall valfn val)))
               table)
      (format out "~%};~%~%");
      (format out "~&#endif~%"))))

(defun dump-keysym-header ()
  (dump-table *keysym-map* "keysym" "key_symbol"
              (lambda (sym) (format nil "KEY_~A" (symbol-name sym)))
              #'princ-to-string))
