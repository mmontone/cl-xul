(in-package :xul)

(define-component list-test ()
  ((items :initarg :items
	  :initform nil
	  :accessor items)
   (value :initarg :value
	  :initform nil
	  :accessor value)))

(defmethod render-component ((comp list-test))
  (<:vbox
    (<:list-box
      (loop for item in (items comp)
	 do
	   (<:list-item (<:label= item))))
    (<:text-box
      (on-change=
       (lambda (value)
	 ;(break "Setting value: ~A" value)
	 (setf (value comp) value)
	 (mark-clean comp))))
    (<:button (<:label= "Add")
	      (on-command=*
		;(break "Add text: ~A" (value comp))
		(when (value comp)
		  (push (value comp) (items comp)))))))

(defparameter *list-test-app*
  (make-instance 'xul-application
		 :name "listtest"
		 :root-component (make-instance 'list-test)
		 :build-id "0001"
		 :id "ListTest"))

(run-app *list-test-app*)
