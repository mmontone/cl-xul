(in-package :xul-test)

(define-component lists-showcase ()
  ()
  (:render (comp)
	   (<:list-box
	     (<:list-item (<:label= "Hello")))))
