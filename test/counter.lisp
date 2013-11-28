(in-package :xul)

(define-component counters ()
  ())

(defmethod initialize-instance :after ((counters counters) &rest initargs)
  (declare (ignore initargs))
  (add-component counters 'counter-1 (make-instance 'counter :counter 0))
  (add-component counters 'counter-2 (make-instance 'counter :counter 3)))

(defmethod render ((counters counters))
  (with-child-components (counter-1 counter-2) counters
    (<:vbox
      (render counter-1)
      (render counter-2))))

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

(defparameter *counters-app*
  (make-instance 'xul-application
		 :name "counters"
		 :root-component (make-instance 'counters)
		 :build-id "0001"
		 :id "CountersApplication"))

(run-app *counters-app*)
