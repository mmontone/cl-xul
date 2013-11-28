(in-package :xul)

(defclass component ()
  ((id :initform (gensym)
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
           (some (lambda (slot)
		   (and (typep slot 'component-slot)
			(make-component-dirty-p slot))) direct-slots))
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
  (let ((root-component (root-component app)))
    (let ((changes (update-xul-from-component root-component)))
      (loop for change in changes
	 do (clws:write-to-client-text
	     *client*
	     (serialize-change change))))))			       

(defun update-xul-from-component (component)
  (if (dirty-p component)
      ;; Generate the changes
      (replace-component component)
      ;; else, see if the childs changed
      (loop for child being the hash-values of (children component)
	 appending (update-xul-from-component child))))

(defun replace-component (component)
  (mark-clean component)
  (list
   (make-instance 'replace-component-change
		  :component component)))

(defclass xul-change ()
  ())

(defclass replace-component-change (xul-change)
  ((component :initarg :component
	      :initform (error "Provide the component")
	      :accessor component)))

(defmethod serialize-change ((change replace-component-change))
  (json:encode-json-plist-to-string
   (list :type "replaceComponent"
	 :id (id (component change))
	 :content
	 (with-output-to-string (s)
	   (let ((sink (cxml:make-character-stream-sink s :omit-xml-declaration-p t)))
	     (cxml:with-xml-output sink
	       (serialize-xul
		(with-xul
		  (render (component change))))))))))

(defun map-component (component function)
  (funcall function component)
  (loop for child in (children component)
       do (map-component child function)))

(defun mark-dirty (component)
  (setf (dirty-p component) t))

(defun mark-clean (component &optional (recursive-p t))
  (setf (dirty-p component) nil)
  (when recursive-p
    (loop for child being the hash-values of (children component)
	 do (mark-clean child recursive-p))))

(defun add-component (component slot child-component)
  (setf (gethash slot (children component))
	child-component)
  (setf (parent child-component) component)
  (mark-dirty component))

(defmethod remove-component (component (slot symbol))
  (remhash slot (children component))
  (mark-dirty component))

(defmethod remove-component (component (child-component component))
  (loop for child being the hash-values of (children component)
       using (hash-key slot)
       when (eql child child-component)
       do (return (remhash slot (children component))))
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

(defgeneric render (component))

(defmethod render :around ((component component))
  (<:box (<:id= (id component))
	 (call-next-method)))
