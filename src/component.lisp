(in-package :xul)

(defclass component ()
  ((id :initform (gensym "COMPONENT-")
       :accessor id)
   (parent :initform nil
	   :accessor parent)
   (dirty-p :initarg :dirty-p
	    :initform nil
	    :accessor dirty-p)
   (children :initarg :children
	     :initform (make-hash-table :test #'equalp)
	     :accessor children)))

(defclass component-class (standard-class)
  ())

(defmethod closer-mop:validate-superclass ((class component-class)
					   (super standard-class))
  t)

(defclass component-slot ()
  ((make-component-dirty-p :initarg :make-component-dirty-p
			   :initform t
			   :accessor make-component-dirty-p)))

(defclass component-direct-slot (component-slot closer-mop:standard-direct-slot-definition)
  ())

(defclass component-effective-slot (component-slot closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class component-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'component-direct-slot))


(defmethod closer-mop:effective-slot-definition-class ((class component-class)
						       &rest initargs)
  (declare (ignore initargs))
  (find-class 'component-effective-slot))

(defmethod closer-mop:compute-effective-slot-definition ((class component-class) name direct-slots)
   (let ((effective-slot (call-next-method)))
     (setf (make-component-dirty-p effective-slot)
           (some #'make-component-dirty-p direct-slots))
     effective-slot))

(defmethod initialize-instance :around
  ((class component-class) &rest initargs
   &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'component)))

     ;; 'my-object is already one of the (indirect) superclasses
     (call-next-method)

     ;; 'my-object is not one of the superclasses, so we have to add it
     (apply #'call-next-method
            class
            :direct-superclasses
            (append direct-superclasses
                    (list (find-class 'component)))
            initargs)))

(defmethod reinitialize-instance :around
  ((class component-class) &rest initargs
   &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p

    ;; if direct superclasses are explicitly passed
    ;; this is exactly like above
    (if (loop for class in direct-superclasses
              thereis (subtypep class (find-class 'component)))
       (call-next-method)
       (apply #'call-next-method
              class
              :direct-superclasses
              (append direct-superclasses
                      (list (find-class 'component)))
              initargs))

    ;; if direct superclasses are not explicitly passed
    ;; we _must_ not change anything
    (call-next-method)))

(defmethod (setf closer-mop:slot-value-using-class)
    (new-value
     (class component-class)
     object
     slot-definition)
  (call-next-method)
  (when (make-component-dirty-p slot-definition)
    (setf (dirty-p object) t)))

(defun update-xul (app)
  (let ((root-component (root app)))
    (let ((changes (update-xul-from-component root-component))))))

(defun update-xul-from-component (component)
  (if (dirty-p component)
      ;; Generate the change
      (replace-component component)
      ;; else, see if the childs changed
      (loop for child in (children component)
	 collect (update-xul-from-component child))))

(defun map-component (component function)
  (funcall function component)
  (loop for child in (children component)
       do (map-component child function)))

(defun mark-dirty (component)
  (setf (dirty-p component) t))

(defun mark-clean (component &optional (recursive-p t))
  (setf (dirty-p component) nil)
  (when recursive-p
    (loop for child in (children component)
	 do (mark-clean child recursive-p))))

(defun add-component (component slot child-component)
  (setf (gethash slot (children component))
	child-component)
  (setf (parent child-component) component)
  (mark-dirty component))

(defmethod remove-component (component (slot symbol))
  (remhash slot (children component))
  (mark-dirty component))

(defun get-component (component slot)
  (gethash slot (children component)))

(defmacro with-child-components (children component &body body)
  `(let ,(loop for child in children
	      collect `(,child (get-component ,component ',child)))
     ,@body))

(defmacro define-component (name super-classes slots &rest options)
  `(defclass ,name ,super-classes
     ,slots
     (:metaclass component-class)
     ,@options))

(defparameter *callback-handlers* (make-hash-table :test #'equalp))

(defun get-callback-handler (id)
  (or (gethash id *callback-handlers*)
      (progn
	;(break "~A not found" id) 
	(error "Callback handler with id ~A not found" id))))

(defun register-callback-handler (function)
  (let ((id (symbol-name (gensym "CH-"))))
    (setf (gethash id *callback-handlers*)
	  function)
    id))

(defun handle-callback (callback)
  (break "Handling callback: ~A" callback)
  (let ((handler-id (cdr (assoc :id callback))))
    (let ((handler (get-callback-handler handler-id)))
      ;(break "~A" handler)
      (funcall handler))))

(defun on-command= (function)
  (let ((handler-id
	 (register-callback-handler function)))
    (<:on-command= (format nil "sendMessage('{\"type\":\"callback\", \"id\":\"~A\"}');"
			   handler-id))))

(defmacro on-command=* (&body body)
  `(on-command= (lambda () ,@body)))
						   
(defmethod render ((component component))
  (render-component component))
  
