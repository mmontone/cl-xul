(in-package :xul-test)

(define-component images-showcase ()
  ()
  (:render (comp)
	   (<:image (src= (asdf:system-relative-pathname
			   :cl-xul-test "test/showcase/images/lisplogo_fancy_256.png")))
	   (<:image (src= (asdf:system-relative-pathname
			   :cl-xul-test "test/showcase/images/mozilla-foundation-onblack.png")))
	   ))
