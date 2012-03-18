(in-package :putil)

;; (cffi:defcfun (%unix-pipe "pipe") :int
;;   (pipefd :pointer))

;; (defstruct unix-pipe input output)

;; (defun unix-pipe ()
;;   (cffi:with-foreign-object (pipefd :int 2)
;;     (unless (zerop (%unix-pipe pipefd))
;;       (error "Cannot create pipe."))
;;     (make-unix-pipe :input  (cffi:mem-aref pipefd :int 0)
;;                     :output (cffi:mem-aref pipefd :int 1))))


(defun fd-write-byte (fd value)
  (cffi:with-foreign-object (var :uint8)
    (setf (cffi:mem-ref var :uint8) value)
    (osicat-posix:repeat-upon-eintr
      (osicat-posix:write fd var 1))))

(defun fd-read-byte (fd)
  (cffi:with-foreign-object (var :uint8)
    (osicat-posix:repeat-upon-eintr
      (osicat-posix:read fd var 1))
    (cffi:mem-ref var :uint8)))

