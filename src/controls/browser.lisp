(in-package :xul)

(define-xul-element browser (xul-element)
  (autocomplete-enabled autocomplete-popup autoscroll
			disable-history disable-global-history disable-security
			homepage remoteshowcaret src
			(type* :attribute-name "type"))
  (:documentation "A frame which is expected to contain a view of a Web document. It is similar to an iframe except that it holds a page history and contains additional methods to manipulate the currently displayed page.

Most of the properties and methods of the browser will rarely be used and can only be called from chrome URLs. Other URLs will need to use the document and history objects to change the displayed document."))

(define-xul-element iframe (container-element)
  (showcaret src (type* :attribute-name "type") transparent)
  (:documentation "An inner frame that works much like the HTML iframe element.
The src attribute can be used to specify the content of the frame.
This content is a separate document. The children of the iframe are ignored."))

(define-xul-element script (container-element)
  (src (type* :attribute-name "type"))
  (:documentation "Much like the HTML script element, this is used to declare a script to be used by the XUL window.
You should usually put scripts in a separate file pointed to by the src attribute, but you may also place the script inline inside the opening and closing script tags."))
