(in-package :xul-widgets)

(define-widget button ()
  ((label :initarg :label
	  :initform (error "Provide the button label")
	  :accessor label))
  (:events
   (on-command :initarg :on-command
		:accessor on-command
		:arguments nil
		:documentation "Triggered when button clicked"))
  (:render (button)
	   (<:button
	     (<:label= (label button))
	     (when (or (on-command button)
		       (on-command* button))
	       (let ((client-command (or (on-command* button)
					 ""))
		     (server-command
		      (or (and (on-command button)
			       (let ((handler-id
				      (xul::register-callback-handler
				       (lambda (callback)
					 (declare (ignore callback))
					 (funcall (on-command button))))))
				 (let ((callback (list :type "callback"
						       :id handler-id
						       :app (xul::name xul::*app*))))
				   (format nil "sendMessage('~A');"
					   (json:encode-json-plist-to-string callback)))))
			  "")))
		 (<:on-command=
		  (format nil "~{~a~^; ~}" (list client-command server-command))))))))
