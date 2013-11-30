(in-package :xul)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-xul-element
      (name super-elements attributes &rest options)
    `(progn
       (defclass ,name ,super-elements
	 ,attributes
	 (:metaclass xul-class)
	 ,@options)
     
       (defmacro ,(intern (symbol-name name) :xul-builder)
	   (&body body)
	 `(let ((element (make-instance ',',name)))
	    (when (null *current-element*)
	      (error "Can't bind ~A to a null *current-element*. Make sure this is being called under a with-xul scope" element))
	    (if (equalp *current-element* t)
		(let ((*current-element* element))
		  (let ((content (progn ,@body)))
		    (when (stringp content)
		      (setf (content element) content))
		    element))
		(progn
		  (setf (children *current-element*) (append (children *current-element*) (list element)))
		  (let ((*current-element* element))
		    (let ((content (progn ,@body)))
		      (when (stringp content)
			(setf (content element) content))
		      element))
		  ))))
       
       (export ',(intern (symbol-name name) :xul-builder) :xul-builder)

       ,@(loop for attribute in attributes
	    collect
	      (let* ((attribute-name (if (symbolp attribute)
					 attribute
					 (first attribute)))
		     (attribute-builder-name (intern (format nil "~A=" attribute-name) :xul-builder)))
		`(progn
		   (defmacro ,attribute-builder-name (value)
		     `(progn
			(setf (,',attribute-name *current-element*) ,value)
			nil))
		   (export ',attribute-builder-name :xul-builder)))
	      ))))
  
(defclass xml-element ()
  ((content :initarg :content
	    :accessor content
	    :initform nil)))

(define-xul-element xul-element (xml-element)
  (align allow-events allow-negative-assertions
	 (class* :attribute-name "class") coalesce-duplicate-arcs collapsed container containment context
	 context-menu data-sources dir empty equal-size flags flex height hidden id
	 insert-after insert-before left max-height max-width menu min-height min-width mouse-through
	 observes ordinal
	 (orient :type (or :horizontal :vertical) :documentation "Orientation") pack persist popup (position* :attribute-name "position") preference-editable query-type
	 ref remove-element sort-direction sort-resource sort-resource-2 status-text style
	 template tooltip tooltip-text top uri wait-cursor width

	 ;; Events
	 (on-command :attribute-name "oncommand")
	 ))

(define-xul-element container-element (xul-element)
  ((children :initarg :children
	     :initform nil
	     :accessor children
	     :attribute nil)
   ))

(define-xul-element xul-control (xul-element)
  ())

(defmethod element-name ((xul-element xul-element))
  (string-downcase
   (remove #\-
	   (symbol-name
	    (class-name (class-of xul-element))))))

(defgeneric serialize-xul (xul-element))

(defmethod serialize-xul ((xul-element xul-element))
  ;; Validation
  (when (and (content xul-element)
		 (and (typep xul-element 'container-element)
		      (children xul-element)))
	(error "Invalid xul element ~A. Cannot contain text and children at the same time"
	       xul-element))
      
  ;; Generic serialization
  (let ((element-name (element-name xul-element)))
    (cxml:with-element element-name
      (loop for attribute in (attributes (class-of xul-element))
	 when (slot-boundp xul-element (closer-mop:slot-definition-name attribute))
	 do
	   (cxml:attribute (attribute-name attribute)
			   (serialize-xml-value
			    (slot-value xul-element (closer-mop:slot-definition-name attribute)))))
      (when (content xul-element)
	(cxml:text (content xul-element)))
      (when (typep xul-element 'container-element)
	(loop for child in (children xul-element)
	     do (serialize-xul child))))))

(defmethod serialize-xml-value ((value (eql nil)))
  "false")

(defmethod serialize-xml-value ((value (eql t)))
  "true")

(defmethod serialize-xml-value ((value symbol))
  (string-downcase (symbol-name value)))

(defmethod serialize-xml-value (x)
  (princ-to-string x))
