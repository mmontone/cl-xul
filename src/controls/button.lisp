(in-package :xul)

(define-xul-element button (container-element)
  (accesskey autocheck checkState checked command crop dir disabled dlgtype group
	     icon image label (open* :attribute-name "open" :initarg :open) orient tabindex (type* :attribute-name "type" :initarg :type) default)
  (:documentation "A button that can be pressed by the user. Event handlers can be used to trap mouse, keyboard and other events. It is typically rendered as a grey outset rectangle. You can specify the label of the button using the label attribute or by placing content inside the button."))

(define-xul-element checkbox (xul-element)
  (accesskey checked command crop disabled src label preference tabindex default)
  (:documentation "An element that can be turned on and off. This is most commonly rendered as a box when the element is off and a box with a check when the element is on. The user can switch the state of the check box by selecting it with the mouse. A label, specified with the label attribute, may be added beside the check box."))

(define-xul-element radio (xul-control)
  (accesskey command crop disabled focused group image label selected tabindex value src)
  (:documentation "An element that can be turned on and off. Radio buttons are almost always grouped together in groups. Only one radio button within the same radiogroup may be selected at a time. The user can switch which radio button is turned on by selecting it with the mouse or keyboard. Other radio buttons in the same group are turned off. A label, specified with the label attribute may be added beside the radio button."))

(define-xul-element radio-group (container-element)
  (disabled focused preference tabindex value)
  (:documentation "A group of radio buttons. Only one radio button inside the group can be selected at a time. The radio buttons may either direct children of the radiogroup or descendants. Place the radiogroup inside a groupbox if you would like a border or caption for the group. The radiogroup defaults to vertical orientation."))
