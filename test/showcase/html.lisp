(in-package :xul-test)

(define-component html-showcase ()
  ()
  (:render (comp)
	   (<html:div
	     (<html:h1 "<h1>HTML test</h1>")
	     (<html:p (<html:style= "color: white; background-color:blue")
		  "<p style=\"color: white; background-color:blue\">Blah blah</p>")
	     (<html:ul
	      (<html:li "Item 1")
	      (<html:li "Item 2"))
	     )))
