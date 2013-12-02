(in-package :xul-test)

(define-component dialogs-showcase ()
  ()
  (:render (comp)
	   (<:button (<:label= "Open dialog")
		     (<:on-command=
		       (xul::with-open-dialog
			   ("Dialog test"
			    '(:modal "true"))
			 (<:dialog
			  (<:id= "mydialog")
			  (<:title= "My dialog")
			  (<:ondialogaccept= "alert('Accept!!');")
			  (<:ondialogcancel= "alert('Cancel!!');")
			  (<:description "This is a dialog")))))))
