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
	 observes ordinal (orient :initarg :orient
	   :initform nil
	   :accessor orient
	   :type (or :horizontal :vertical)) pack persist popup (position* :attribute-name "position") preference-editable query-type
	 ref remove-element sort-direction sort-resource sort-resource-2 status-text style
	 template tooltip tooltip-text top uri wait-cursor width ))

(define-xul-element container-element (xul-element)
  ((children :initarg :children
	     :initform nil
	     :accessor children)
   ))

(define-xul-element xul-control (xul-element)
  ())
