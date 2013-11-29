(in-package :xul)

(define-component list-test ()
  ((items :initarg :items
	  :initform nil
	  :accessor items)
   (value :initarg :value
	  :initform nil
	  :accessor value
	  :make-component-dirty-p nil)
   (selected-item :initarg :selected-item
		  :initform nil
		  :accessor selected-item))
  (:render (comp)
	   (flet ((remove-list-item ()
		    (when (selected-item comp)
		      (setf (items comp)
			    (remove (selected-item comp)
				    (items comp))))))
	     (<:vbox
	       (<:tooltip (<:id= "list-tip")
			  (<:orient= "vertical")
			  (<:noautohide= t)
			  (<:style= "background-color: #33DD00;")
			  (<:label (<:value="Click here to see more information"))
			  (<:label (<:value="Really!")
				   (<:style="color: red;")))
			  
	       (<:list-box
		 (<:tooltip= "list-tip")
		 (<:context= "list-menu")
		 (on-select=* (index) ;(break "List item:~A" (nth index (items comp)))
		   (setf (selected-item comp) (nth index (items comp))))
		 (loop for item in (items comp)
		    do
		      (<:list-item (<:label= item)
				   (when (eql item (selected-item comp))
				     (<:selected= t)))))
	       (<:menu-popup (<:id= "list-menu")
			     (<:menu-item
			       (<:label= "Remove")
			       (on-command=* (remove-list-item))))	     
	       (<:button (<:label= "Remove")
			 (<:tooltip-text= "Remove the selected item from the list")
			 (on-command=* (remove-list-item)))
	       (<:text-box
		 (<:tooltip-text= "Enter the item to add to the list")
		 (on-change=
		  (lambda (value)
					;(break "Setting value: ~A" value)
		    (setf (value comp) value))))
	       (<:button (<:label= "Add")
			 (<:tooltip-text= "Add and item to the list")
			 (on-command=*
					;(break "Add text: ~A" (value comp))
			   (when (value comp)
			     (push (value comp) (items comp)))))))))

(defparameter *list-test-app*
  (make-instance 'xul-application
		 :name "listtest"
		 :root-component (make-instance 'list-test)
		 :build-id "0001"
		 :id "ListTest"))

(run-app *list-test-app*)
