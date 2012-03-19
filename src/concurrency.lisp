(in-package :playpen)

;;;; ------------------------------------------------------------
;;;; Channel protocol
;;;;
;;;; This is built on the idea of atomic transactions. In sending a
;;;; message, a write transaction occurs: the receiver first ensures
;;;; it can consume a message. If not, the 'fail' continuation is
;;;; called. Otherwise the 'proceed' continuation is called with a
;;;; 'commit' argument, a function which can be called to deliver a
;;;; message to the receiver. Similarly, reading a message from a
;;;; channel invokes a read transaction. Here, the 'proceed'
;;;; continuation additionally receives the next message in the
;;;; channel, which is not removed from the channel unless it calls
;;;; 'commit'. In this way, a reader can atomically decline to handle
;;;; a particular message.

;;;; I have some doubts about whether this is 100% sensible. The basic
;;;; idea seems sound, but maybe I want to read or write multiple
;;;; items atomically, requiring a slightly different interface. Hmm.

(defgeneric transact (transaction-type receiver proceed fail)
  (:documentation "Perform transaction."))

(defgeneric channel-open-p (channel))

(defgeneric drain (readable-channel)
  (:documentation "Remove and return a list of all pending messages in
  the channel, in FIFO order."))

(define-condition channel-error (error)
  ((channel :reader channel-error-channel :initarg :channel)))

(define-condition channel-closed (channel-error)
  ())

(define-condition channel-empty (channel-error) ; ?
  ())

;;;; Messaging in terms of transactions:

(defun send (receiver message &key (fail 'error))
  (transact
   :write
   receiver
   (lambda (commit) (funcall commit message))
   fail))

(defun receive (receiver &key (fail (constantly nil)))
  (transact
   :read
   receiver
   (lambda (commit value)
     (funcall commit)
     value)
   fail))

;;;; Transducer channels

(defstruct transducer channel function)

(defmethod channel-open-p ((channel transducer))
  (channel-open-p (transducer-channel channel)))

(defstruct (input-transducer (:include transducer)))

(defmethod transact ((type (eql :write)) (this input-transducer) proceed fail)
  (transact
   :write
   (transducer-channel this)
   (lambda (commit)
     (funcall proceed
              (lambda (message)
                (funcall commit
                         (funcall (transducer-function this)
                                  message)))))
   fail))

;;; Beward side-effecting output-transducer functions, since by
;;; necessity they run outside the commit, so that PROCEED sees the
;;; transformed value. Safe if PROCEED always commits: therefore,
;;; compose transactions such that reads are nested inside writes,
;;; with an innermost PROCEED that always commits.

;;; I don't think that's a great constraint to force on people, so
;;; better you just not create situations where you care about
;;; side-effects here.

(defstruct (output-transducer (:include transducer)))

(defmethod transact ((type (eql :read)) (this output-transducer) proceed fail)
  (transact
   :read
   (transducer-channel this)
   (lambda (commit value)
     (funcall proceed
              commit
              (funcall (transducer-function this)
                       value)))
   fail))

(defmethod drain ((channel output-transducer))
  (mapcar (transducer-function channel)
          (drain (transducer-channel channel))))

;;;; Quick sketch of composing read/write transactions

(defun transduction-test (output-channel input-channel proceed fail)
  (transact
   :write
   output-channel
   (lambda (commit-write)               ; Output ready.
     (transact
      :read
      input-channel
      (lambda (commit-read next-object) ; Input available.
        (funcall
         proceed
         (lambda (output-value)         ; Commit result.
           (funcall commit-read)
           (funcall commit-write output-value))
         next-object))
      fail))
   fail))

;;;; Test code

(defstruct test-channel buffer fail)

(define-condition channel-test-error (channel-error) ())
(define-condition channel-test-write-error (channel-test-error) ())
(define-condition channel-test-read-error  (channel-test-error) ())

(defmethod transact ((type (eql :write)) (channel test-channel) proceed fail)
  (if (pop (test-channel-fail channel))
      (funcall fail (make-instance 'channel-test-write-error :channel channel))
      (funcall proceed (lambda (message)
                         (alexandria:appendf
                          (test-channel-buffer channel)
                          (list message))))))

(defmethod channel-open-p ((channel test-channel)) t)

(defmethod transact ((type (eql :read)) (channel test-channel) proceed fail)
  (cond
    ((pop (test-channel-fail channel))
     (funcall fail (make-instance 'channel-test-read-error :channel channel)))
    ((null (test-channel-buffer channel))
     (funcall fail (make-instance 'channel-empty :channel channel)))
    (t (funcall proceed
                (lambda () (pop (test-channel-buffer channel)))
                (first (test-channel-buffer channel))))))

(defun test-channel-tests ()
  ;; Simple send/receive test.
  (let ((chan (make-test-channel))
        first-empty-read-must-fail
        last-empty-read-must-fail)
    (receive chan :fail (lambda (c) (etypecase c (channel-empty (setf first-empty-read-must-fail t)))))
    (assert first-empty-read-must-fail)
    (send chan 1)
    (assert (equal (test-channel-buffer chan) '(1)))
    (send chan 2)
    (assert (equal (test-channel-buffer chan) '(1 2)))
    (send chan 3)
    (assert (equal (test-channel-buffer chan) '(1 2 3)))
    (assert (eql 1 (receive chan :fail 'error)))
    (assert (eql 2 (receive chan :fail 'error)))
    (assert (eql 3 (receive chan :fail 'error)))
    (receive chan :fail (lambda (c) (etypecase c (channel-empty (setf last-empty-read-must-fail t)))))
    (assert last-empty-read-must-fail))
  ;; Transduction test 1
  (let ((in (make-test-channel  :fail (list nil   t nil nil nil) :buffer (list 1 2 3)))
        (out (make-test-channel :fail (list nil nil nil   t nil)))
        t1-must-fail-at-end
        t1-inject-read-error-test
        t1-inject-write-error-test)
    (flet ((tr (commit x) (funcall commit (- x))))
      (transduction-test out in #'tr 'error)
      (transduction-test out in #'tr
                         (lambda (c)
                           (etypecase c
                             (channel-test-read-error
                              (setf t1-inject-read-error-test t)))))
      (assert t1-inject-read-error-test)
      (transduction-test out in #'tr 'error)
      (transduction-test out in #'tr
                         (lambda (c)
                           (etypecase c
                             (channel-test-write-error
                              (setf t1-inject-write-error-test t)))))
      (assert t1-inject-write-error-test)
      (transduction-test out in #'tr 'error)
      (transduction-test out in #'tr
                         (lambda (c)
                           (etypecase c
                             (channel-empty (setf t1-must-fail-at-end t))))))
    (assert t1-must-fail-at-end)
    (assert (null (test-channel-buffer in)))
    (assert (equal (test-channel-buffer out) '(-1 -2 -3))))
  :pass)
