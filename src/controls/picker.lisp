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

(define-xul-element time-picker (xul-element)
  (disabled hideseconds increment readonly tabindex value)
  (:documentation "The timepicker is used to allow the user to enter a time. It contains three fields to enter the hour, minute and second. Arrow buttons next to the fields allow the values to be adjusted with the mouse. A fourth textbox appears for 12 hour clocks which allows selection of AM or PM.

To specify the initial, use the value attribute set to a value of either the form HH:MM:SS or HH:MM. The value may be retrieved and changed using the value property or the dateValue property. The former specifies the time as a string of the form HH:MM:SS whereas the latter specifies the time as a Date object. In addition, the hour, minute and second properties may be used to retrieve and modify each component of the time separately."))
