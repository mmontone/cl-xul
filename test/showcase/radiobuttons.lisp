(in-package :xul-test)

(define-component radiobuttons-showcase ()
  ()
  (:render (comp)
	   (<:radio-group
	     (<:radio (<:label= "Hello")))))
