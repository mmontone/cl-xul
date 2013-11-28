(in-package :xul)

(define-component multi-counters ()
  ())

(defmethod initialize-instance :after ((comp multi-counters) &rest initargs)
  (declare (ignore initargs))
  (add-component comp (gensym) (make-instance 'counter :counter 0)))

(defmethod render ((comp multi-counters))
  (<:vbox
    (<:vbox
      (maphash (lambda (slot counter)
		 (render counter)
		 (<:button (<:label= "Remove")
			   (on-command=* (remove-component comp counter))))
	       (children comp)))
    (<:button (<:label= "Add")
	      (on-command=* (add-component comp (gensym)
					   (make-instance 'counter :counter 0))))))

(define-component counter ()
  ((counter :accessor counter
	    :type integer
	    :initform 0
	    :initarg :counter)))

(defmethod render ((counter counter))
  (<:label (<:value= (counter counter)))
  (<:button (<:label= "Increment")
	    (on-command=* (increment-counter counter)))
  (<:button (<:label= "Decrement")
	    (on-command=* (decrement-counter counter))))

(defun increment-counter (counter)
  ;(break "Incrementing counter: ~A" counter)
  (incf (counter counter)))

(defun decrement-counter (counter)
  ;(break "Decrementing counter: ~A" counter)
  (decf (counter counter)))

(defparameter *multi-counters-app*
  (make-instance 'xul-application
		 :name "multicounters"
		 :root-component (make-instance 'multi-counters)
		 :build-id "0001"
		 :id "MultiCountersApplication"))

(run-app *multi-counters-app*)