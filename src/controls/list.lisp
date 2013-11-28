(in-package :xul)

(define-xul-element list-box (container-element)
  (disabled disableKeyNavigation preference rows seltype suppressonselect tabindex value onselect)
  (:documentation "This element is used to create a list of items where one or more of the items may be selected. A listbox may contain multiple columns. There are numerous methods which allow the items in the listbox to be retrieved and modified.

You may specify the number of rows to display in the list using the rows attribute. Additional rows can be viewed using a scroll bar. A listbox is expected to contain listitem elements. All the rows in the listbox are the same height, which is the height of the tallest item in the list. If you wish to create a list with variable height rows, or with non-text content, you should instead use the richlistbox element."))

(define-xul-element list-item (container-element)
  (accesskey checked command crop current disabled image label preference selected tabindex (type* :attribute-name "type" :initarg :type) value)
  (:documentation "A single row in a listbox. The text of the listitem is specified either using listcell elements, or by placing a label attribute directly on the listitem element. By default it contains a single listcell element of type and class appropriate to that of the listitem."))

(define-xul-element list-cols (container-element)
  ()
  (:documentation "A container for the columns of a listbox, each of which are created with the listcol element. There should be only one listcols element in a list box. If there is no listcols element, the list box has a single column. "))

(define-xul-element list-col (xul-control)
  ()
  (:documentation "A column in a listbox. You can make some columns flexible and other columns non-flexible."))

(define-xul-element list-cell (xul-control)
  (crop disabled image label (type* :attribute-name "type" :initarg :type))
  (:documentation "A single cell of a listbox. By default it only contains text but iconic and checkbox listcells are also available"))

(define-xul-element list-head (container-element)
  (disabled)
  (:documentation "A header row of a listbox. It is usual to place listheader elements inside the listhead, one for each column."))

(define-xul-element list-header (xul-control)
  (disabled label)
  (:documentation "A header for a single column in a listbox."))
