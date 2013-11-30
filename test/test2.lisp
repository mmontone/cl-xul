(in-package :xul-test)

(defparameter *test-app-2*
  (make-instance 'xul-application
		 :name "test2"
		 :javascripts (list (asdf::system-relative-pathname :cl-xul #p"test/test-app.js"))
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
						     (<:on-command= "alert('Hello!!');"))
					   (<:button (<:label= "Connection")
						     (<:on-command= "alert(connection.readyState);"))))
				  (<:tab-box (<:id="myTabList")
					     (<:selected-index="2")
					     (<:tabs
					      (<:tab (<:label="A First tab"))
					      (<:tab (<:label="Second tab"))
					      (<:tab (<:label="Another tab"))
					      (<:tab (<:label="Last tab")))
					     (<:tab-panels
					      (<:tab-panel (<:label (<:value= "First panel")))
					      (<:tab-panel (<:label (<:value= "Second panel")))
					      (<:tab-panel (<:button (<:label= "Third  panel")))
					      (<:tab-panel (<:button (<:label= "Forth  panel")))))))
		 :build-id "0001"
		 :id "TestApplication"))

(run-app *test-app-2*)
