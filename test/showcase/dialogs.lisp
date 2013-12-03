(in-package :xul-test)

(define-component dialogs-showcase ()
  ()
  (:render (comp)
	   (<:vbox 
	     (<:button (<:label= "Open dialog")
		       (<:on-command=
			(with-open-dialog
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
			(with-open-dialog ("Component in dialog"
						'(:modal "yes"
						  :resizable "no"))
			  (<:dialog
			    (<:id= "componentindialog")
			    (<:title= "Component in dialog")
			    (<:buttons= "accept")
			    (render (make-instance 'my-dialog))))))
	     (<:button (<:label= "Ask")
		       (<:on-command= (ask "Are you sure?")))			
	     (<:button (<:label= "Open component in window")
		       (<:on-command=
			(with-open-window ()
			  (<:window
			    (<:id= "componentinwindow")
			    (<:title= "Component in window")
			    (render (make-instance 'my-dialog))))))
	     )))

(define-component my-dialog ()
  ()
  (:render (comp)
	   (<:vbox
	     (<:description "This is my component in a dialog")
	     (<:button (<:label= "Hello")
		       (<:on-command= (ps:ps (alert "Hello!!")))))))
