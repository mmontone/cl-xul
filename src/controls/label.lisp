(in-package :xul)

(define-xul-element label (xul-control)
  ((accesskey :initarg :accesskey
	      :accessor accesskey)
   (control :initarg :control
	    :accessor control)
   crop disabled href
   (value :initarg :value
	  :accessor value)))

(defmethod serialize-xul ((label label))
  (cxml:with-element "label"
    (cxml:attribute "control" (control label))
    (cxml:attribute "accesskey" (accesskey label))
    (cxml:attribute "value" (value label))))
