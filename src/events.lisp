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
	      (format stream "<?xml-stylesheet href=\"chrome://global/skin/\" type=\"text/css\"?>")
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

(defparameter *icons* (list
		       :question "dialog-question-2.png"
		       :important "dialog-important-2.png"
		       :ok "dialog-ok-3.png"
		       :information "dialog-information.png"
		       :warning "dialog-warning-2.png"))
		       
(defun icon-path (id)
  (let ((pathname (or (getf *icons* id)
		      (error "Icon not found: ~A" id))))
    (or
     (probe-file
      (asdf:system-relative-pathname :cl-xul
				     (format nil "resources/icons/~A" pathname)))
     (error "Icon file path not found: ~A" pathname))))
  
(defun ask (question &key (name "Question")
		       on-accept
		       on-cancel
		       (icon :question))
  (with-open-dialog (name
		     '(:modal "yes"
		       :resizable "no"))
    (<:dialog
      (<:id= "question")
      (<:title= name)
      (<:buttons= "accept, cancel")
      (<:ondialogaccept= on-accept)
      (<:ondialogcancel= on-cancel)
      (<:hbox (<:flex= 1)
	      (when icon
		(<:hbox (<:align= :center)
			(<:flex= 1)
			(<:image (src= (icon-path icon)))))
	      (<:hbox (<:flex= 1) (<:align= :center)
		      (<:description  question))))))

(defun prompt (message
	       &key (name "Prompt")
		 on-accept
		 on-cancel
		 (icon :ok)
		 (value nil))
  (with-open-dialog (name
		     '(:modal "yes"
		       :resizable "no"))
    (let ((value value))
      (<:dialog
	(<:id= "prompt")
	(<:title= name)
	(<:buttons= "accept, cancel")
	(<:ondialogaccept= (lambda ()
			     (funcall on-accept value)))
	(<:ondialogcancel= on-cancel)
	(<:hbox (<:flex= 1)
		(when icon
		  (<:hbox (<:align= :center)
			  (<:flex= 1)
			  (<:image (src= (icon-path icon)))))
		(<:vbox
		  (<:hbox (<:flex= 1) (<:align= :center)
			  (<:description  message))
		  (<:hbox (<:flex= 1)
			  (<:text-box (on-change=* (val) (setf value val))
				      (when value (<:value= value))))))))))

(defun inform (message &key (name "Information")
			 on-accept
			 (icon :information))
  (with-open-dialog (name
		     '(:modal "yes"
		       :resizable "no"))
    (<:dialog
      (<:id= "information")
      (<:title= name)
      (<:buttons= "accept")
      (<:ondialogaccept= on-accept)
      (<:hbox (<:flex= 1)
	      (when icon
		(<:hbox (<:align= :center)
			(<:flex= 1)
			(<:image (src= (icon-path icon)))))
	      (<:hbox (<:flex= 1) (<:align= :center)
		      (<:description  message))))))

(defun open-window (window &optional features &rest args)
  (let ((window-element (with-xul (funcall window))))
    (flet ((serialize-features (features)
	     (format nil "~{~a~^, ~}"
		     (mapcar (lambda (feature)
			       (destructuring-bind (key . value) feature
				 (format nil "~A=~A"
					 (string-downcase (princ-to-string key))
					 (string-downcase (princ-to-string value)))))
			     (alexandria:plist-alist features)))))
      (let ((window-pathname 
	     (merge-pathnames
	      (pathname (format nil "chrome/content/window~A.xul" (id window-element)))
	      (app-folder *app*))))
      
	;; First, serialize the window to a xul file
	(with-open-file (stream window-pathname
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create)
	  (let ((output (cxml:make-character-stream-sink stream)))
	    (cxml:with-xml-output output
	      ;(format stream "<?xml-stylesheet href=\"chrome://global/skin/\" type=\"text/css\"?>")
	      (serialize-xul window-element))))

	;; Then bind the window in the command
	(let ((window-uri (format nil "chrome://~A/content/window~A.xul"
				  (name *app*)
				  (id window-element))))
	  (format nil "window.open('~A', '~A', ~A);"
		  window-uri
		  (serialize-features features)
		  (json:encode-json-plist-to-string args)))))))

(defmacro with-open-window ((&optional features &rest args) &body body)
  `(open-window (lambda () ,@body)
		,features
		,@args))
