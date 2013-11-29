(in-package :xul-widgets)

(define-widget list-box ()
  ((items :initarg :items
	  :initform nil
	  :accessor items)
   (selected-item :initarg :selected-item
		  :initform nil
		  :accessor selected-item)
   (menu-popup :initarg :menu-popup
	       :initform nil
	       :accessor menu-popup
	       :make-component-dirty-p nil))
  (:events
   (on-select :arguments (item)
	      :documentation "Triggered when item selected"))
  (:initialize (list))
  (:client-initialize (list))		      
  (:render (list)
	   (let ((id (symbol-name (gensym)))
		 (context (when (menu-popup list)
			    (symbol-name (gensym)))))
	     (<:list-box (<:id= id)
			 (when context
			   (<:context= context))
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
		       (<:selected= t)))))
	     (when context
	       (<:menu-popup
		 (<:id= context)
		 (loop for (label command) in (menu-popup list)
		    do (<:menu-item (<:label= label)
				    (xul::on-command= command))))))))
