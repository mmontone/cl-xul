(in-package :xul)

(define-xul-element button (container-element)
  (accesskey autocheck checkState checked command crop dir disabled dlgtype group
	     icon image label (open* :attribute-name "open" :initarg :open) orient tabindex (type* :attribute-name "type" :initarg :type))
  (:documentation "A button that can be pressed by the user. Event handlers can be used to trap mouse, keyboard and other events. It is typically rendered as a grey outset rectangle. You can specify the label of the button using the label attribute or by placing content inside the button."))

(define-xul-element checkbox (xul-element)
  (accesskey checked command crop disabled src label preference tabindex)
  (:documentation "An element that can be turned on and off. This is most commonly rendered as a box when the element is off and a box with a check when the element is on. The user can switch the state of the check box by selecting it with the mouse. A label, specified with the label attribute, may be added beside the check box."))
