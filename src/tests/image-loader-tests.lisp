(in-package :playpen)


(defun jpeg-loader-test (files &key (configurator 'identity))
  (dolist (file files)
    (format t "~&~80A " (human-string file))
    (finish-output)
    (handler-case (print (read-jpeg file :configurator configurator))
      (jpeg-decoder-error (c)
        (format t "~&ERROR: ~A~%" c))
      (jpeg-decoder-warning (c)
        (format t "~&Warning: ~A~%" c)))))

(defun crude-work-threader (num-threads jobs worker-function)
  (let* ((lock (bordeaux-threads:make-lock))
         (workers (loop repeat num-threads
                        collect
                        (bordeaux-threads:make-thread
                         (lambda ()
                           (loop as job = (bordeaux-threads:with-lock-held (lock)
                                            (pop jobs))
                                 while job
                                 do
                                 (funcall worker-function job)))))))
    (map nil #'bordeaux-threads:join-thread workers)))

(defun threaded-jpeg-loader-test (num-threads files &key (configurator 'identity))
  (crude-work-threader
   num-threads
   files
   (lambda (file)
     (handler-case (print (cons (read-jpeg file :configurator configurator)
                                file))
       (jpeg-decoder-error (c)
         (format t "~&ERROR: ~A~%" c))
       (jpeg-decoder-warning (c)
         (format t "~&Warning: ~A~%" c))))))

(defun threaded-png-loader-test (num-threads files)
  (crude-work-threader
   num-threads
   files
   (lambda (file)
     (handler-case (print (read-png file))
       (png-error (c)
         (format t "~&~A~%" c))))))
