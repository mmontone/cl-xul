(in-package :xul)

(define-component components-showcase ()
  ()
  (:render (comp)
	   (<:hbox
	     (loop for child being the hash-values of (children comp) do
		  (render comp))
	     (<:button (<:label= "Add")
		       (on-command=*
			 (add-component comp
					(gensym)
					(make-instance 'components-showcase))))
	     (<:button (<:label= "Remove")
		       (on-command=*
			 (remove-component (parent comp) comp))))))
