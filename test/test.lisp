(in-package :xul)

(defparameter *test-app*
  (make-instance 'xul-application
		 :name "TestApp"
		 :top (xul
		       (window (:title "Test application")
			       (vbox ()
				     (label (:control "hello-label"
						      :accesskey "h"
						      :value "Hello"))
				     (label (:control "bye-label"
						      :accesskey "b"
						      :value "Bye"))
				     (button (:label "Hello"
						     :on-command "alert('Hello!!');")))))
		 :build-id "0001"
		 :id "TestApplication"))

(run-app *test-app*)
