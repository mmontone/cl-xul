(in-package :xul-test)

(define-component trees-showcase ()
  ()
  (:render (comp)
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
		      (<:tree-cell (<:label= "Bob"))))))))
