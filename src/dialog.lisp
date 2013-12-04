(in-package :xul)

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
			(<:image (src= (or (and (symbolp icon) (icon-path icon))
					   icon)))))
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
			  (<:image (src= (or (and (symbolp icon) (icon-path icon))
					     icon)))))
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
			(<:image (src= (or (and (symbolp icon) (icon-path icon))
					   icon)))))
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
