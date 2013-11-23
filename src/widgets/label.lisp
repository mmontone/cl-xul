(in-package :xul)

(define-xul-element label (xul-widget)
  ((accesskey :initarg :accesskey
	      :accessor accesskey)
   (control :initarg :control
	    :accessor control)
   crop disabled href
   (value :initarg :value
	  :accessor value)))
