(in-package :xul)

(define-xul-element grid (container-element)
  ()
  (:documentation "A grid is a layout type that arranges elements in rows and columns. The grid is expected to contain both a columns element as a child which defines the structure of the columns and a rows element as child to define the rows. The size and attributes defined on columns and rows will affect the size and placement of elements in each cell. Elements placed within both the columns and rows will be displayed, although usually only elements will be placed inside one of these. In is most common to place elements within the rows and use the columns only to define their flexibility and width. Whichever of the columns and rows is last in the grid is displayed on top; commonly the columns element appears in the grid first."))

(define-xul-element columns (container-element)
  ()
  (:documentation "Defines the columns of a grid. Child column elements define the individual columns to appear in the grid. The columns element may be nested inside another columns element. Other elements placed inside a columns element occupy the full height of the grid and are placed in their corresponding positions between the columns."))

(define-xul-element column (container-element)
  ()
  (:documentation "A single column in a columns element. Each child of the column element is placed in each successive cell of the grid. The column with the most child elements determines the number of rows in each column.

It is common to use columns only to define width and flexibility for the grid column and not place elements directly inside the column."))

(define-xul-element rows (container-element)
  ()
  (:documentation "Defines the rows of a grid. Child row elements define the individual rows to appear in the grid. Each child of a rows element should be a row element.

The rows element may be nested inside another rows element. The children of these nested rows are counted as normal rows as if they were part of the parent. However, nesting rows elements allows groups of rows to have a separate appearance such as a different border, or separate scrolling may be defined for these rows.

Non-row related elements placed inside a rows element occupy the full width of the grid and are placed in their corresponding positions between the rows."))

(define-xul-element row (container-element)
  ()
  (:documentation "A single row in a rows element. Each child of the row element is placed in each successive cell of the grid. The row with the most child elements determines the number of columns in each row, which may be smaller or larger than the number of column elements defined in the grid."))


