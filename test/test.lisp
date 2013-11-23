(in-package :xul)

(defparameter *test-app*
  (make-instance 'xul-application
		 :name "TestApp"
		 :xul (xul
		       (window (:title "Test application"
				       :width "500"
				       :height "500")
			       (menu-bar (:id "sample-menubar")
					 (menu (:id "action-menu" :label "Action")
					       (menu-popup (:id "action-popup")
							   (menu-item (:label "New"))
							   (menu-item (:label "Save" :disabled "true"))
							   (menu-item (:label "Close"))
							   (menu-separator ())
							   (menu-item (:label "Quit"))))
					 (menu (:id "edit-menu" :label "Edit")
					       (menu-popup (:id "edit-popup")
							   (menu-item (:label "Undo"))
							   (menu-item (:label "Redo")))))
			       (vbox ()
				     (label (:control "hello-label"
						      :accesskey "h"
						      :value "Hello"))
				     (label (:control "bye-label"
						      :accesskey "b"
						      :value "Bye"))
				     (button (:label "Hello"
						     :on-command "alert('Hello!!');"))
				     (button (:type "menu-button" :label "New")
					     (menu-popup ()
							 (menu-item (:label "New Document"))
							 (menu-item (:label "New Image"))))
				     (checkbox (:label "Enable JavaScript" :checked t))
				     (checkbox (:label "Enable Java" :checked nil))
				     (date-picker (:value "2007/03/26"))
				     (date-picker (:type :grid))
				     (date-picker (:type :popup :value "2008/08/24"))
				     (color-picker (:color "#FF0000"))
				     (color-picker (:type :button :color "#CC0080"))
				     )))
		 :build-id "0001"
		 :id "TestApplication"))

(run-app *test-app*)
