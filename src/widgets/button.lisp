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
	     (when (on-command button)
	       (xul::on-command=* (on-command button))))))
