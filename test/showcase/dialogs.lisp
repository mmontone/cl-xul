(in-package :xul-test)

(define-component dialogs-showcase ()
  ()
  (:render (comp)
	   (<:vbox 
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
			      (<:description "This is a dialog"))))))
	     (<:button (<:label= "Open component in dialog")
		       (<:on-command=
			(xul::with-open-dialog ("Component in dialog"
						'(:modal "yes"
						  :resizable "no"))
			  (<:dialog
			    (<:id= "mydialog")
			    (<:title= "My dialog")
			    (<:buttons= "accept, cancel")
			    (<:ondialogaccept= "alert('Accept!!');")
			    (<:ondialogcancel= "alert('Cancel!!');")
			    (render (make-instance 'my-dialog))))))
	     )))

(define-component my-dialog ()
  ()
  (:render (comp)
	   (<:vbox
	     (<:description "This is my component in a dialog")
	     (<:button (<:label= "Hello")
		       (<:on-command= (ps:ps (alert "Hello!!")))))))
