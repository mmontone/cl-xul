(in-package :xul)

(defclass xul-class (standard-class)
  ())

(defmethod attributes ((class xul-class))
  (loop for slot in (closer-mop:class-slots class)
     when (and (typep slot 'xul-slot-definition)
	       (is-attribute slot))
       collect slot))

(defmethod closer-mop:validate-superclass ((class xul-class)
				(superclass standard-class))
  t)

(defclass xul-slot-definition (closer-mop:standard-slot-definition)
  ((is-attribute :initarg :attribute
		 :initform t
		 :accessor is-attribute
		 :type boolean
		 :documentation "Whether the slot is a XUL attribute")
   (attribute-name :initarg :attribute-name
		   :initform nil
		   :writer (setf attribute-name)
		   :type (or null string)
		   :documentation "The attribute name")))

(defmethod print-object ((slot xul-slot-definition) stream)
  (if (is-attribute slot)
      (print-unreadable-object (slot stream :type t :identity t)
	(format stream "attribute: ~A" (attribute-name slot)))
      (call-next-method)))

(defmethod attribute-name ((slot xul-slot-definition))
  (if (and (slot-boundp slot 'attribute-name)
	   (not (null (slot-value slot 'attribute-name))))
      (slot-value slot 'attribute-name)
      ; else
      ; convert the-slot-name to theslotname
      (string-downcase (remove #\- (symbol-name (closer-mop:slot-definition-name slot))))))
      
(defclass xul-direct-slot-definition (xul-slot-definition closer-mop:standard-direct-slot-definition)
  ())

(defclass xul-effective-slot-definition (xul-slot-definition closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class xul-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'xul-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class xul-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'xul-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition ((class xul-class) name direct-slots)
   (let ((effective-slot (call-next-method)))
     (setf (is-attribute effective-slot)
           (some (lambda (slot)
		   (and (typep slot 'xul-slot-definition)
			(is-attribute slot)))
		 direct-slots))
     (setf (attribute-name effective-slot)
	   (some (lambda (slot)
		   (and (typep slot 'xul-slot-definition)
			(attribute-name slot)))
		 direct-slots))
     effective-slot))

(defmethod initialize-instance :after ((slot xul-effective-slot-definition) &rest initargs)
  (declare (ignore initargs))
  (when (and (is-attribute slot)
	     (not (closer-mop:slot-definition-initargs slot)))
    ;; Add the initarg
    (setf (closer-mop:slot-definition-initargs slot)
	  (list (intern (symbol-name (closer-mop:slot-definition-name slot))
			:keyword)))))      

(defun ensure-accessor-for (class accessor-name effective-slot type)
  (let* ((gf (ensure-generic-function accessor-name :lambda-list (ecase type
                                                                   (:reader '(object))
                                                                   (:writer '(new-value object)))))
         (specializers (ecase type
                         (:reader (list class))
                         (:writer (list (find-class 't) class))))
         ;(current-method (find-method gf '() specializers nil))
	 )
    (let  ((method (closer-mop:ensure-method gf
				  (ecase type
				    (:reader
				     `(lambda (object)
					(declare (optimize (speed 1))) ; (speed 1) to ignore compiler notes when defining accessors
                                        (slot-value object ',(closer-mop:slot-definition-name effective-slot))))
				    (:writer
				     `(lambda (new-value object)
					(declare (optimize (speed 1))) ; (speed 1) to ignore compiler notes when defining accessors
					(setf (slot-value object ',(closer-mop:slot-definition-name effective-slot)) new-value))))
				  :specializers specializers
				  #+ensure-method-supports-method-class :method-class
				  #+ensure-method-supports-method-class (find-class 'xul-reader-method))))
      (declare (ignorable method))
      #+ensure-method-supports-method-class
      (setf (effective-slot-of method) effective-slot))))

(defun ensure-accessors-for (class)
  (loop for effective-slot in (closer-mop:class-slots class)
        when (and (typep effective-slot 'xul-effective-slot-definition)
		  (is-attribute effective-slot)) do
       (progn
	 (ensure-accessor-for class (closer-mop:slot-definition-name effective-slot) effective-slot :reader)
	 (ensure-accessor-for class `(setf ,(closer-mop:slot-definition-name effective-slot)) effective-slot :writer))))
        
(defmethod closer-mop:finalize-inheritance :after ((class xul-class))
  ;; Ensure accessors for the attributes
  (ensure-accessors-for class))
