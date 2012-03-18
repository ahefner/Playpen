(in-package :playpen)

;;;; ------------------------------------------------------------
;;;; Channel protocol

(defgeneric send (channel message))
(defgeneric receive (channel))
(defgeneric drain (channel))
(defgeneric channel-open-p (channel))

(define-condition channel-error (error)
  ((channel :reader channel-error-channel :initarg channel)))

(define-condition channel-closed-error (channel-error)
  ())

(defun %ensure-channel-open (channel)
  (unless (channel-open-p channel)
    (error 'channel-closed-error :channel channel)))


;;;; Transducer channel

(defstruct transducer channel function inverse)

(defmethod send ((channel transducer) message)
  (send (transducer-channel channel)
        (funcall (transducer-function channel) message)))

(defmethod receive ((channel transducer))
  (funcall (transducer-inverse channel)
           (receive (transducer-channel channel))))

(defmethod channel-open-p ((channel transducer))
  (channel-open-p (transducer-output channel)))

(defmethod drain ((channel transducer))
  (mapcar (transducer-inverse channel)
          (drain (transducer-channel channel))))




