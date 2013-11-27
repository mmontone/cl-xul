(in-package :xul)

(define-component my-component ()
  ())

(defmethod initialize-instance :after ((component my-component) &rest initargs)
  (add-component component 'counter-1 (make-instance 'counter :counter 0))
  (add-component component 'counter-2 (make-instance 'counter :counter 3)))

(defmethod render-component ((component my-component))
  (with-child-components (counter-1 counter-2) component
    (render counter-1)
    (<:label (<:value= "Hello"))
    (render counter-2)))

(define-component counter ()
  ((counter :accessor counter
	    :type integer
	    :initform 0
	    :initarg :counter)))

(defmethod render-component ((counter counter))
  (<:label (<:value= (counter counter)))
  (<:button (<:label= "Increment")
	    (on-command= (lambda ()
			   (increment-counter counter))))
  (<:button (<:label= "Decrement")
	    (on-command= (lambda ()
			   (decrement-counter counter)))))

(defun increment-counter (counter)
  (break "Incrementing counter: ~A" counter)
  (incf (counter counter)))

(defun decrement-counter (counter)
  (break "Decrementing counter: ~A" counter)
  (decf (counter counter)))
