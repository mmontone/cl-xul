(in-package :xul)

(define-xul-element button (xul-element)
  (accesskey autocheck checkState checked command crop dir disabled dlgtype group
	     icon image label (open* :attribute-name "open") orient tabindex (type* :attribute-name "type")))
