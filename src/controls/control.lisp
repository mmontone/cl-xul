(in-package :xul)

(defclass xul-element ()
  (align allow-events allow-negative-assertions
	 class coalesce-duplicate-arcs collapsed container containment context
	 context-menu data-sources dir empty equal-size flags flex height hidden id
	 insert-after insert-before left max-height max-width menu min-height min-width mouse-through
	 observes ordinal (orient :initarg :orient
	   :initform nil
	   :accessor orient
	   :type (or :horizontal :vertical)) pack persist popup position preference-editable query-type
	 ref remove-element sort-direction sort-resource sort-resource-2 status-text style
	 template tooltip tooltip-text top uri wait-cursor width ))

(defclass container-element (xul-element)
  ((children :initarg :children
	     :initform nil
	     :accessor children)
   ))

(defclass xul-control (xul-element)
  ())
