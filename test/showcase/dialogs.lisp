(in-package :xul-test)

(define-component dialogs-showcase ()
  ()
  (:render (comp)
	   (<:button (<:label= "Open dialog")
		     (<:on-command=
		       (xul::with-open-dialog
			   ("Dialog test"
			    '(:modal "yes"
			      :resizable "no"))
			 (<:dialog
			  (<:id= "mydialog")
			  (<:title= "My dialog")
			  (<:buttons= "accept, cancel")
			  (<:ondialogaccept= "alert('Accept!!');")
			  (<:ondialogcancel= "alert('Cancel!!');")
			  (<:box
			    (<:description "This is a dialog"))))))))
