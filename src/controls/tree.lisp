(in-package :xul)

(define-xul-element tree (container-element)
  (disableKeyNavigation disabled editable enableColumnDrag flags hidecolumnpicker onselect rows seltype statedatasource tabindex treelines)
  (:documentation "A container which can be used to hold a tabular or hierarchical set of rows of elements.
The tree may contain any number of rows and any number of columns.
Each row of the tree may contain child rows which are displayed indented from the parent."))

(define-xul-element tree-cols (container-element)
  (picker-tooltip-text)
  (:documentation "A group of treecol elements. There should one and only one treecols element in a tree."))

(define-xul-element tree-col (xul-element)
  (crop cycler dragging editable fixed hidden hideheader ignoreincolumnpicker label primary
	(sort* :attribute-name "sort") sortActive sortDirection src (type* :attribute-name "type") width)
  (:documentation  "A column of a tree.
It displays the column header and holds the size and other information about the column.
You can also place splitter elements between the columns to allow column resizing.
You should always place an id attribute on a treecol element to ensure that the column positioning is handled properly."))

(define-xul-element tree-children (container-element)
  (alternating-background)
  (:documentation "This element is the body of the tree.
For content trees, the content will be placed inside this element.
This element is also used to define container rows in the tree."))

(define-xul-element tree-item (container-element)
  (container empty label (open* :attribute-name "open") uri)
  (:documentation "A treeitem should be placed inside a treechildren element and should contain treerow elements.
The treeitem can be clicked by the user to select the row of the tree.
The treeitem contains a single row and all of what appear to the user as that row's descendants."))

(define-xul-element tree-row (container-element)
  (properties)
  (:documentation "A single row in a tree.
It should be placed inside a treeitem element.
Children of the treerow should be treecell elements.
If child rows are necessary, they should be placed in a treechildren element inside the parent treeitem."))

(define-xul-element tree-cell (xul-element)
  (editable label mode properties ref src value)
  (:documentation "A single cell in a tree.
This element should be placed inside a treerow.
You can set the text for the cell using the label attribute."))

(define-xul-element tree-separator (xul-element)
  (properties)
  (:documentation "Used to place a separator row in a tree."))
