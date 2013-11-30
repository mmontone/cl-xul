(in-package :xul)

(define-component components-showcase ()
  ()
  (:initialize (comp)
	       (add-component comp (gensym) (make-instance 'counter :counter 0)))
  (:render (comp)
	   (<:vbox
	     (<:vbox
	       (maphash (lambda (slot counter)
			  (declare (ignore slot))
			  (render counter)
			  (<:button (<:label= "Remove")
				    (on-command=* (remove-component comp counter))))
			(children comp)))
	     (<:button (<:label= "Add")
		       (on-command=* (add-component comp (gensym)
						    (make-instance 'counter :counter 0)))))))

(define-component counter ()
  ((counter :accessor counter
	    :type integer
	    :initform 0
	    :initarg :counter))
  (:render (counter)
	   (<:label (<:value= (counter counter)))
	   (<:button (<:label= "Increment")
		     (on-command=* (increment-counter counter)))
	   (<:button (<:label= "Decrement")
		     (on-command=* (decrement-counter counter)))))

(defun increment-counter (counter)
  ;(break "Incrementing counter: ~A" counter)
  (incf (counter counter)))

(defun decrement-counter (counter)
  ;(break "Decrementing counter: ~A" counter)
  (decf (counter counter)))
