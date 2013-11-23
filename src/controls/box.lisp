(in-package :xul)

(define-xul-element box (container-element)
  ())

(define-xul-element hbox (box)
  ()
  (:default-initargs :orient :horizontal))

(define-xul-element vbox (box)
  ()
  (:default-initargs :orient :vertical))

(defmethod serialize-xul ((vbox vbox))
  (cxml:with-element "vbox"
    (loop for child in (children vbox)
	 do (serialize-xul child))))

(defmethod serialize-xul ((hbox hbox))
  (cxml:with-element "hbox"
    (loop for child in (children hbox)
	 do (serialize-xul child))))

(defmethod serialize-xul ((box box))
  (cxml:with-element "box"
    (when (orient box)
      (cxml:attribute "orient" (orient box))
      (loop for child in (children box)
	   do (serialize-xul child)))))
