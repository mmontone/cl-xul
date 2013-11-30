(in-package :xul)

(define-component lists-showcase ()
  ()
  (:render (comp)
	   (<:list-box
	     (<:list-item (<:label= "Hello")))))
