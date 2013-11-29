(in-package :xul)

(define-component list-widget-test ()
  ()
  (:initialize (comp)
	       (let ((list (w::list-box))
		     (input (w::text-box)))
	       (add-component comp 'list list)
	       (add-component comp 'remove
			      (w::button :label "Remove"
					 :on-command
					 (lambda ()
					   (when (w::selected-item list)
					     (setf (w::items list)
						   (remove (w::selected-item list)
							   (w::items list)))))))
	       (add-component comp 'input input)
	       (add-component comp 'add
			      (w::button :label "Add"
					 :on-command
					 (lambda ()
					   (when (w::value input)
					     (push (w::value input)
						   (w::items list))))))))
  (:render (comp)
	   (with-child-components (list input add remove) comp
	     (<:vbox
	       (render list)
	       (render input)
	       (render add)
	       (render remove)))))

(defparameter *list-widget-test-app*
  (make-instance 'xul-application
		 :name "listwidgettest"
		 :root-component (make-instance 'list-widget-test)
		 :build-id "0001"
		 :id "ListWidgetTest"))

(run-app *list-widget-test-app*)
