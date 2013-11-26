(in-package :xul)

(defparameter *test-app-2*
  (make-instance 'xul-application
		 :name "TestApp"
		 :xul (with-xul
		       (<:window (<:title= "Test application")
				 (<:width= "500")
				 (<:height= "500")
			 (<:menu-bar (<:id= "sample-menubar")
			   (<:menu (<:id= "action-menu")
				   (<:label= "Action")
			     (<:menu-popup
			       (<:id= "action-popup")
			       (<:menu-item (<:label= "New"))
			       (<:menu-item (<:label= "Save")
					    (<:disabled= "true"))
			       (<:menu-item (<:label= "Close"))
			       (<:menu-separator)
			       (<:menu-item (<:label= "Quit"))))
			   (<:menu (<:id= "edit-menu")
				   (<:label= "Edit")
			     (<:menu-popup
			       (<:id= "edit-popup")
			       (<:menu-item (<:label= "Undo"))
			       (<:menu-item (<:label= "Redo")))))
			 (<:vbox
			   (<:box (<:label (<:value= "hello")))
			   (<:box (<:button (<:label= "Hello")
					    (<:on-command= "alert('Hello');")))))
			       )
		 :build-id "0001"
		 :id "TestApplication"))

(run-app *test-app-2*)
