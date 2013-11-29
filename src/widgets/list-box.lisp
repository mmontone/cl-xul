(in-package :xul-widgets)

(define-widget list-box ()
  ((items :initarg :items
	  :initform nil
	  :accessor items)
   (selected-item :initarg :selected-item
		  :initform nil
		  :accessor selected-item))
  (:events
   (on-select :arguments (item)
	      :documentation "Triggered when item selected"))
  (:initialize (list))
  (:client-initialize (list))		      
  (:render (list)
	   (<:list-box
	     (xul::on-select=* (index)
	       (let ((selected-item (nth index (items list))))
		 (setf (selected-item list) selected-item)
		 (when (on-select list)
		   (funcall (on-select list)
			    selected-item))))
	     (loop for item in (items list)
		do (<:list-item
		     (<:label= (prin1-to-string item))
		     (when (eql (selected-item list)
				item)
		       (<:selected= t)))))))
