(in-package :xul)

(define-xul-element box (container-element)
  ())

(define-xul-element hbox (box)
  ()
  (:default-initargs :orient :horizontal))

(define-xul-element vbox (box)
  ()
  (:default-initargs :orient :vertical))
