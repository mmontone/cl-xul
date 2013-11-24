(in-package :xul)

(define-xul-element box (container-element)
  ())

(define-xul-element hbox (box)
  ()
  (:default-initargs :orient :horizontal))

(define-xul-element vbox (box)
  ()
  (:default-initargs :orient :vertical))

(define-xul-element spacer (xul-control)
  ()
  (:documentation "An element that takes up space but does not display anything. It is usually used to place spacing within a container. If you don't specify that the spacer has a size or is flexible, the spacer does not occupy any space. If you want a small gap, consider using a separator instead."))
