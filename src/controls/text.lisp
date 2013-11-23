(in-package :xul)

(define-xul-element description (xul-element)
  (crop disabled tabindex value)
  (:documentation "This element is used to create a block of text. The text can be set either with the value attribute or by placing text inside the open and close description tags. The value attribute is used to set text that appears in a single line. If text appears as a child of the description, it will wrap to multiple lines. It may contain arbitrary markup, which can be styled as needed."))

(define-xul-element text-box (xul-element)
  (cols decimalplaces disabled emptytext hidespinbuttons increment label (max* :attribute-name "max" :initarg :max) maxlength (min* :attribute-name "min" :initarg :min) multiline newlines onblur onchange onfocus oninput placeholder preference readonly rows searchbutton size spellcheck tabindex timeout (type* :initarg :type :attribute-name "type") value wrap wraparound)
  (:documentation "An input field where the user can enter text. It is similar to the HTML input element. Only one line of text is displayed by default. The multiline attribute can be specified to display a field with multiple rows."))
