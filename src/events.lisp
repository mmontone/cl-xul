(in-package :xul)

(defparameter *callback-handlers* (make-hash-table :test #'equalp))

(defun get-callback-handler (id)
  (or (gethash id *callback-handlers*)
      (progn
	;(break "~A not found" id) 
	(error "Callback handler with id ~A not found" id))))

(defun register-callback-handler (function)
  (let ((id (symbol-name (gensym))))
    (setf (gethash id *callback-handlers*)
	  function)
    id))

(defun handle-callback (callback)
  ;(break "Handling callback: ~A" callback)
  (let ((handler-id (cdr (assoc :id callback))))
    (let ((handler (get-callback-handler handler-id)))
      (let ((*app* (get-application-named (cdr (assoc :app callback)))))
	
					;(break "~A" handler)
      (funcall handler callback)

      ;; Apply view updates after possible modifications
      (update-xul *app*)))))

(defun on-command= (function)
  (let ((handler-id
	 (register-callback-handler
	  (lambda (callback)
	    (declare (ignore callback))
	    (funcall function)))))
    (let ((callback (list :type "callback"
			 :id handler-id
			 :app (name *app*))))
      (<:on-command=
       (format nil "sendMessage('~A');"
	       (json:encode-json-plist-to-string callback))))))

(defmacro on-command=* (&body body)
  `(on-command= (lambda () ,@body)))

(defun on-change= (function)
  (let ((handler-id
	 (register-callback-handler
	  (lambda (callback)
	    (funcall function (cdr (assoc :value callback)))))))
    (when (or (not (slot-boundp *current-element* 'id))
	      (not (id *current-element*)))
      (setf (id *current-element*)
	    (symbol-name (gensym))))
    (let ((callback (list :type "callback"
			  :id handler-id
			  :app (name *app*)
			  :value (format nil "document.getElementById(\\\"~A\\\").value;"
					 (id *current-element*)))))
			  
      (<:onchange= 
       (format nil "sendMessage('~A');"
	       (json:encode-json-plist-to-string callback))
       ))))

(defmacro on-change=* ((value) &body body)
  `(on-change= (lambda (,value) ,@body)))

(defun on-select= (function)
  (let ((handler-id
	 (register-callback-handler
	  (lambda (callback)
	    (funcall function (cdr (assoc :value callback)))))))
    (when (or (not (slot-boundp *current-element* 'id))
	      (not (id *current-element*)))
      (setf (id *current-element*)
	    (symbol-name (gensym))))
    (let ((callback (list :type "callback"
			  :id handler-id
			  :app (name *app*)
			  :value (format nil "document.getElementById(\\\"~A\\\").selectedIndex;"
					 (id *current-element*)))))
			  
      (<:onselect=
       (format nil "sendMessage('~A');"
	       (json:encode-json-plist-to-string callback))
       ))))

(defmacro on-select=* ((value) &body body)
  `(on-select= (lambda (,value) ,@body)))
