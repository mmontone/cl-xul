(in-package :xul-widgets)

(define-widget text-box ()
  ((value :initarg :value
	  :initform nil
	  :accessor value))
  (:events
   (on-change :documentation "Triggered when the text box changes"))
  (:render (text-box)
	   (<:text-box
	     (when (value text-box)
	       (<:value= (value text-box)))
	     (xul::on-change=* (value)
	       (setf (value text-box) value)
	       (when (on-change text-box)
		 (funcall (on-change text-box) value))))))
