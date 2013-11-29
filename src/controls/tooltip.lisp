(in-package :xul)

(define-xul-element tooltip (container-element)
  (crop default label noautohide onpopuphidden onpopuphiding onpopupshowing onpopupshown page (position* :attribute-name "position"))
  (:documentation "This element is used for the tooltip popups.
For text-only tooltips, this element doesn't need to be used; instead you may just add a tooltiptext attribute to an element."))
