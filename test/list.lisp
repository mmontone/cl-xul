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
	   (<:vbox
	     (<:list-box
	       (on-select=* (index) ;(break "List item:~A" (nth index (items comp)))
		 (setf (selected-item comp) (nth index (items comp))))
	       (loop for item in (items comp)
		  do
		    (<:list-item (<:label= item)
				 (when (eql item (selected-item comp))
				   (<:selected= t)))))
	     (<:button (<:label= "Remove")
		       (on-command=*
			 (when (selected-item comp)
			   (setf (items comp)
				 (remove (selected-item comp)
					 (items comp))))))
	     (<:text-box
	       (on-change=
		(lambda (value)
					;(break "Setting value: ~A" value)
		  (setf (value comp) value))))
	     (<:button (<:label= "Add")
		       (on-command=*
					;(break "Add text: ~A" (value comp))
			 (when (value comp)
			   (push (value comp) (items comp))))))))

(defparameter *list-test-app*
  (make-instance 'xul-application
		 :name "listtest"
		 :root-component (make-instance 'list-test)
		 :build-id "0001"
		 :id "ListTest"))

(run-app *list-test-app*)
