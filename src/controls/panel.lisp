(in-package :xul)

(define-xul-element panel (container-element)
  (backdrag close consumeoutsideclicks fade flip ignorekeys label left level  noautofocus noautohide norestorefocus onpopuphidden onpopuphiding onpopupshowing onpopupshown position titlebar top type)
  (:documentation "A panel is a used as a temporary floating popup window that may contain any type of content. It does not have any window decorations. When open, it floats above the window and may extend outside the border of the main window. Typically, it will be attached to a button using the button's type attribute so that when the button is pressed, the panel will be displayed. A panel may also be opened via a script using the openPopup method."))

(define-xul-element group-box (container-element)
  ()
  (:documentation "The groupbox is used to group related elements together. If a caption element is placed inside the groupbox, it will be used as a caption which appears along the top of the groupbox. Typically, the groupbox will be drawn with a border around it and the caption either above or over the top border, however the actual appearance will be platform specific. On Linux, for instance, only the caption appears with no border around the group."))

(define-xul-element caption (xul-control)
  (accesskey crop image label tabindex)
  (:documentation "A header for a groupbox. It may contain either a text label, using the label attribute, or child elements for a more complex caption."))

(define-xul-element deck (container-element)
  ((selected-index :attribute-name "selectedIndex"))
  (:documentation "An element that displays only one of its children at a time. The selectedIndex attribute determines which child is displayed."))
