(in-package :xul-widgets)

(define-widget text-box ()
  ((value :initarg :value
	  :initform nil
	  :accessor value))
  (:render (text-box)
	   (<:text-box
	     (xul::on-change=* (value)
	       (setf (value text-box) value)))))
