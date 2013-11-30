(in-package :xul)

(define-component radiobuttons-showcase ()
  ()
  (:render (comp)
	   (<:radio-group
	     (<:radio (<:label= "Hello")))))
