(in-package :xul-test)

(define-component lists-showcase ()
  ()
  (:render (comp)
	   (<:vbox
	     (<:list-box
	       (<:list-item (<:label= "Hello"))))))
