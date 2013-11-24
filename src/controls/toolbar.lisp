(in-package :xul)

(define-xul-element toolbox (container-element)
  ()
  (:documentation "A container for toolbars. It is a type of box but defaults to vertical orientation. If a toolbar is placed inside a toolbox, a grippy is displayed on its left or upper edge. The user may click the grippy to collapse the toolbar. If multiple toolbars are placed in the same toolbox, they will all collapse into the same row. The Firefox browser does not have grippies so toolbars cannot be collapsed and expanded."))
