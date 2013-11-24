(in-package :xul)

(define-xul-element label (xul-control)
  (accesskey control crop disabled href value)
  (:documentation "This element is used to provide a label for a control element. If the user clicks the label, it will move the focus to the associated control, specified with the control attribute."))

(define-xul-element image (xul-control)
  ()
  (:documentation "An element that displays an image, much like the HTML img element. The src attribute can be used to specify the URL of the image."))
