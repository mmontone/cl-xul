(in-package :xul)

(define-xul-element dialog (container-element)
  (buttonaccesskeyaccept buttonaccesskeycancel
			 buttonaccesskeydisclosure buttonaccesskeyextra1 buttonaccesskeyextra2 buttonaccesskeyhelp
			 buttonalign buttondir buttondisabledaccept buttonlabelaccept
			 buttonlabelcancel buttonlabeldisclosure buttonlabelextra1 buttonlabelextra2
			 buttonlabelhelp buttonorient buttonpack buttons defaultButton
			 ondialogaccept ondialogcancel ondialogdisclosure ondialogextra1
			 ondialogextra2 ondialoghelp title)
  (:documentation "This element should be used in place of the window element for dialog boxes.
The buttons attribute may be used to set which buttons should appear in the dialog box.
These buttons will be placed in the correct locations for the user's platform."))


(defmethod serialize-xul ((dialog dialog))
  (cxml:with-element "dialog"
    (cxml:attribute "xmlns" "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")
    (cxml:attribute "xmlns:html" "http://www.w3.org/1999/xhtml")
    (loop for attribute in (attributes (class-of dialog))
	 when (slot-boundp dialog (closer-mop:slot-definition-name attribute))
	 do
	   (cxml:attribute (attribute-name attribute)
			   (serialize-xml-value
			    (slot-value dialog (closer-mop:slot-definition-name attribute)))))
    (loop for child in (children dialog)
       do (serialize-xul child))))
