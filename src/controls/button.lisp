(in-package :xul)

(define-xul-element button (container-element)
  (accesskey autocheck checkState checked command crop dir disabled dlgtype group
	     icon image label (open* :attribute-name "open" :initarg :open) orient tabindex (type* :attribute-name "type" :initarg :type)))
