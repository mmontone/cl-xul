(in-package :xul-test)

(define-component trees-showcase ()
  ()
  (:render (comp)
	   (<:vbox
	     (<:flex= 1)

	     ;; Basic tree
	     
	     (<:tree (<:flex= 1)
		     (<:editable= t)
		     (<:tree-cols
		       (<:tree-col (<:label= "Active")
				   (<:type*= "checkbox")
				   (<:editable= t))
		       (<:tree-col (<:label= "Name")
				   (<:flex= 1)))
		     (<:tree-children
		       (<:tree-item
			 (<:tree-row
			   (<:tree-cell (<:value= "true"))
			   (<:tree-cell (<:label= "Alice"))))
		       (<:tree-item
			 (<:tree-row
			   (<:tree-cell (<:value= "false"))
			   (<:tree-cell (<:label= "Bob"))))))

	     ;; Directory explorer

	     (labels ((render-pathname (pathname)
			(if (pathname-name pathname)
			    ;; We assume this is a file, not a directory
			    (<:tree-item
			      (<:tree-row
				(<:tree-cell
				  (<:label= (file-namestring pathname))
				  (src= (asdf:system-relative-pathname
					 :cl-xul-test
					 "test/showcase/images/x-office-document.png")))))
			    ;; Else, we assume it is a directory
			    (<:tree-item (<:container= t)
					 (<:tree-row
					   (<:tree-cell
					     (<:label= (car (last (pathname-directory pathname))))
					     (src= (asdf:system-relative-pathname
						    :cl-xul-test
						    "test/showcase/images/folder-open-3.png"))))
					 (<:tree-children
					   (loop for pathname in (cl-fad:list-directory pathname)
					      do (render-pathname pathname)))))))
	       (<:tree (<:flex= 1)
		       (<:hidecolumnpicker= t)
		       (<:tree-cols
			 (<:tree-col (<:flex= 1)
				     (<:label= "Directory explorer")
				     (<:primary= t)))
		       (<:tree-children
			 (render-pathname (asdf:system-relative-pathname :cl-xul ""))))))))
