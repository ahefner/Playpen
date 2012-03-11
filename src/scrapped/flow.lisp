
(in-package :playpen)

;;; Cell protocol. I suspect the motto "Throw one away" applies here.

;;; I could probably scrap current-value and just specify pull-value
;;; to return the current "value" if available, but this decomposition
;;; might be useful.

(defgeneric dependents (object))
(defgeneric dependencies (object))
(defgeneric pull-value (output))
(defgeneric compute (object))
(defgeneric current-value (object)
  (:documentation "Retrieve the current value of the given object, if available. Returns two values: the current value (or nil if none available), and a generalized boolean indicating that a value is available."))

(defgeneric ensure-dependency (object dependent)
  (:documentation "Ensure a dependent is recorded among the dependents of object"))

(defgeneric remove-dependency (object dependent))

(defgeneric push-value (value output)) ; wait, what?

;; I suppose part of the point of push-value is providing a way to
;; detect whether a COMPUTE method changed the value of an output -
;; COMPUTE would call PUSH-VALUE on the cell itself to supply to
;; recomputed value. At the moment this doesn't do me any good anyway.

;; If we want to track whether or not a value changed this cycle, I
;; think we need to keep an extra set which PUSH-VALUE will add its
;; object to. For discrete event channels, should a state of no output
;; count as having changed value versus a previous cycle where an
;; event was output?

(defclass input-cell () 
  ((dependencies :initform nil :accessor dependencies :initarg :dependencies)))

(defmethod initialize-instance :after ((object input-cell) &key dependencies &allow-other-keys)
  (when dependencies
    (dolist (dependency dependencies)
      (ensure-dependency dependency object))))

(defclass output-cell ()
  ((dependents :initform nil :accessor dependents))) ; make opaque? reader?

(defmethod ensure-dependency ((object output-cell) dependent)
  (pushnew dependent (dependents object)))

(defmethod remove-dependency ((object output-cell) former-dependent)
  (setf (dependents object) (remove former-dependent (dependents object))))

(defclass cell (input-cell output-cell) ())

(defvar *pending-cells* (make-hash-table))
(defvar *already-evaluated* (make-hash-table))

(defmethod pull-value (object)
  (unless (gethash object *already-evaluated*)
    (setf (gethash object *pending-cells*) t)))

(defmethod push-value (value place)
  ;; Note that 'value' is not used. It is expected that a more
  ;; specific method will do something with it before entering here.
  (setf (gethash place *already-evaluated*) t)
  (dolist (dep (dependents place))
    (unless (gethash dep *already-evaluated*)
      ;; When can we detect whether an output has changed, to avoid
      ;; waking deps unnecessarily? How?  Also, shouldn't it be an
      ;; error if a dependency already evaluated?  I guess right now we
      ;; allow cyclic graphs and silently do the wrong thing.
      (setf (gethash dep *pending-cells*) t))))

(defmethod current-value :before (object)
  (unless (gethash object *already-evaluated*)
    (error "Attempting to retrieve value of ~A which hasn't been evaluated yet." object))
  object)

(defun propagate ()
  ;; This strategy is obviously not efficient. I'll add the extra
  ;; bookkeeping to make it so once I think I've got the basic idea
  ;; right, although I can't see how to avoid touching nearly the
  ;; entire graph every time.
  (loop until (zerop (hash-table-count *pending-cells*)) do
        (let ((num-processed 0))
          (maphash
           (lambda (cell foo)
             (declare (ignore foo))
             (map nil (lambda (x) (unless (gethash x *already-evaluated*) (pull-value x))) (dependencies cell))
             (when (every (lambda (x) (gethash x *already-evaluated*)) (dependencies cell))
               (assert (not (gethash cell *already-evaluated*)))
               ;; If we want to manage ad-hoc dependencies, wrap
               ;; this in a handler-bind:
               (compute cell)
               (setf (gethash cell *already-evaluated*) t)
               (remhash cell *pending-cells*)
               (incf num-processed)))
           *pending-cells*)
          (when (and (zerop num-processed)
                     (not (zerop (hash-table-count *pending-cells*))))
            (error "Wedged! Fixme!"))))
  (clrhash *already-evaluated*)
  (clrhash *pending-cells*))

;;; Simple value cells have a slot to hold their output value.

(defclass value-cell () ((value :initform nil :initarg :initial-value)))
(defclass simple-value-cell (cell value-cell) ())

(defmethod push-value (event (cell value-cell))
  (setf (slot-value cell 'value) event)
  (call-next-method))

(defmethod current-value ((of value-cell))
  (values (slot-value of 'value) t))

;;; For external event inputs, the event loop will provide the event
;;; via PUSH-VALUE, then call PROPAGATE. If no event is provided, the
;;; event source will produce no output.

(defclass external-event-source (value-cell output-cell) ())

(defmethod pull-value ((source external-event-source))
  (unless (gethash source *already-evaluated*)
    ;; We'll never reach this if the event loop previously
    ;; called push-value.
    (setf (slot-value source 'value) nil)))

(defmethod compute ((cell external-event-source))
  (error "This shoudn't happen."))

(defmethod current-value ((source external-event-source))
  (values (call-next-method) (not (not (call-next-method)))))

;;; Filters have a single input.  If I ever got around to providing
;;; nice syntax for constructing cells, this might not be so useful.

(defclass filter-cell (value-cell output-cell)
  ((input :accessor input :initarg :input)))

(defmethod depdendencies ((cell filter-cell))
  (list (input cell)))

(defmethod (setf input) :before (new-value (object filter-cell))
  (remove-dependency object (input object)))

(defmethod (setf input) :after (new-value (object filter-cell))
  (ensure-dependency object new-value))

;;;; The Rodent

;;; Specials versus functions for these default objects: The initial
;;; idea here was to use functions, so that dependencies could be
;;; recorded. This is still a good idea, whenever I get around to
;;; doing automatic recording of dependencies. Also, who translates
;;; these coordinates? Who filters according to window? In most
;;; instances, you won't connect to these directly.

;;; Raw event stream

(defclass pointer-event-stream (external-event-source) ())
(defvar *default-pointer-stream* (make-instance 'pointer-event-stream))
(defun default-pointer-stream () *default-pointer-stream*)

;;; Pointer state

(defstruct pointer-state x y mask)

(defclass pointer-state-cell (value-cell output-cell)
  ((pointer-stream :reader pointer-stream :initarg :pointer-stream)))

(defvar *default-pointer-state*
  (make-instance 'pointer-state-cell
                 :initial-value (make-pointer-state :x 0 :y 0 :mask 0)
                 :pointer-stream *default-pointer-stream*))

(defun default-pointer-state () *default-pointer-state*)

(defmethod compute ((cell pointer-state-cell))
  (let* ((event (current-value (input cell)))
         (new-state (make-pointer-state
                     :x (pwin:event-x event)
                     :y (pwin:event-y event)
                     :mask (pwin:event-button-state event))))
    (unless (equalp new-state (current-value cell))
      (push-value new-state cell))))

#+NIL
(defun update-windows (windows)
  whatever)

#+NIL
(defun toplevel-loop ()
  (let ((toplevel-windows nil))
    (pwin:initialize-display)
    (with-graphics ()
      (unwind-protect
           (loop with block = nil
                 as event = (pwin:get-event (if block nil 0)) do
                 (typecase event
                   (timeout (update-windows toplevel-window)
                            (setf block t))
                   (t (distribute-event event)
                      (setf block nil))))
        (map nil #'pwin:destroy-window toplevel-windows)))))

;; Need a way to get windows into the toplevel-windows list..


        

