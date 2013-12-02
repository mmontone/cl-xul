(in-package :xul-test)

(define-component images-showcase ()
  ()
  (:render (comp)
	   (<:vbox
	     (<:image
	       (<:flex= 0)
	       (src= (asdf:system-relative-pathname
			     :cl-xul-test "test/showcase/images/lisplogo_fancy_256.png")))
	     (<:image
	       (<:flex= 0)
	       (src= (asdf:system-relative-pathname
			   :cl-xul-test "test/showcase/images/mozilla-foundation-onblack.png")))
	   )))
