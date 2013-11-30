(in-package :xul)

(define-xul-element splitter (container-element)
  (collapse resizeafter resizebefore state substate)
  (:documentation "An element which should appear before or after an element inside a container.
When the splitter is dragged, the sibling elements of the splitter are resized.
If a grippy is placed inside the splitter, one sibling element of the splitter is collapsed when the grippy is clicked."))

(define-xul-element grippy (xul-element)
  ()
  (:documentation "An element that may be used inside a splitter which can be used to collapse a sibling element of the splitter."))
