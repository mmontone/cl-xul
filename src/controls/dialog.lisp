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

