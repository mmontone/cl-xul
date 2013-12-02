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

;; Dialogs

(defun open-dialog (dialog name features &rest args)
  (let ((dialog-element (with-xul (funcall dialog))))
    
    (flet ((serialize-features (features)
	     (format nil "~{~a~^, ~}"
		     (mapcar (lambda (feature)
			       (destructuring-bind (key . value) feature
				 (format nil "~A=~A"
					 (string-downcase (princ-to-string key))
					 (string-downcase (princ-to-string value)))))
			     (alexandria:plist-alist features)))))
      (let ((dialog-pathname 
	     (merge-pathnames
	      (pathname (format nil "chrome/content/dialog~A.xul" (id dialog-element)))
	      (app-folder *app*))))
      
	;; First, serialize the dialog to a xul file
	(with-open-file (stream dialog-pathname
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	  (let ((output (cxml:make-character-stream-sink stream :omit-xml-declaration-p t)))
	    (cxml:with-xml-output output
	      (serialize-xul dialog-element))))

	;; Then bind the dialog in the command
	(let ((dialog-uri (format nil "chrome://~A/content/dialog~A.xul"
				  (name *app*)
				  (id dialog-element))))
	  (format nil "openDialog('~A', '~A', '~A', ~A);"
		  dialog-uri
		  name
		  (serialize-features features)
		  (json:encode-json-plist-to-string args)))))))

(defmacro with-open-dialog ((name features &rest args) &body body)
  `(open-dialog (lambda () ,@body)
		,name
		,features
		,@args))
