(in-package :xul-widgets)

(defclass widget (xul::component)
  ((client-initialization :accessor client-initialization
			  :make-component-dirty-p nil))
  (:metaclass xul::component-class))

(defmacro define-widget (name super-classes slots &rest options)
  (let (class-options widget-options)
    (loop for option in options
       do
	 (cond
	   ((member (first option) (list :render
					 :initialize
					 :client-initialize
					 :events))
	    (push option widget-options))
	   (t (push option class-options))))
    (flet ((event-slot (event)
	     (destructuring-bind (event &key initarg accessor documentation &allow-other-keys)
		 event
	       (list event
		     :initarg (or initarg event)
		     :accessor (or accessor event)
		     :documentation documentation))))		     
      (let* ((events (cdr (assoc :events widget-options)))
	     (slots (append slots
			    (mapcar #'event-slot events))))
	`(progn
	   (defclass ,name (widget ,@super-classes)
	     ,slots
	     (:metaclass xul::component-class)
	     ,@class-options)
	   (defun ,name (&rest initargs)
	     (apply #'make-instance ',name initargs))
	   ,@(loop for option in widget-options
		collect
		  (case (first option)
		    (:render
		     (destructuring-bind (render (comp) &body body) option
		       (declare (ignore render))
		       `(defmethod xul::render ((,comp ,name))
			  ,@body)))
		    (:initialize
		     (destructuring-bind (initialize (comp &optional initargs) &body body) option
		       (declare (ignore initialize))
		       `(defmethod initialize-instance :after ((,comp ,name) &rest ,(or initargs 'initargs))
				   (declare (ignorable ,(or initargs 'initargs)))
				   ,@body)))
		    (:client-initialize
		     (destructuring-bind (client-initialize (comp) &body body)
			 option
		       (declare (ignore client-initialize))
		       `(defmethod client-initialize (,comp)
			  (setf (client-initialization ,comp)
				(ps:ps ,@body))))))))))))

(defgeneric client-initialize (widget)
  )

(defmethod initialize-instance :after ((widget widget) &rest initargs)
  (declare (ignore initargs))
  (setf (client-initialization widget)
	(client-initialize widget)))
