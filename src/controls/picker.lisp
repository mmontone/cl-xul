(in-package :xul)

(define-xul-element color-picker (xul-element)
  (disabled color onchange preference tabindex (type* :attribute-name "type" :initarg :type))
  (:documentation "A palette of colors from which a user may select by clicking on one of the grid cells."))

(define-xul-element date-picker (xul-element)
  (disabled firstdayofweek readonly (type* :attribute-name "type" :initarg :type) tabindex value)
  (:documentation "A datepicker allows the user to enter a date. Three types are available, which can be specified using the type attribute.

    normal - a datepicker with three fields for entering the year, month and date.
    grid - a datepicker with a calendar grid for selecting a date.
    popup - a normal datepicker with three fields, but with an additional dropdown button to display a popup grid."))
