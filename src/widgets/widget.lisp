(in-package :xul)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-xul-element
    (name super-elements attributes &rest options)
  `(defclass ,name ,super-elements
     ,attributes
     (:metaclass xul-class)
     ,@options)))

(define-xul-element xul-element ()
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

(define-xul-element xul-widget (xul-element)
  ())

(defgeneric serialize-xul (xul-element))

(defmethod serialize-xul ((xul-element xul-element))
  ;; Generic serialization
  (let ((element-name (string-downcase (remove #\- (symbol-name (class-name (class-of xul-element)))))))
    (cxml:with-element element-name
      (loop for attribute in (attributes (class-of xul-element))
	 when (slot-boundp xul-element (closer-mop:slot-definition-name attribute))
	 do
	   (cxml:attribute (attribute-name attribute)
			   (serialize-xml-value
			    (slot-value xul-element (closer-mop:slot-definition-name attribute)))))
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
