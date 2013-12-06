(defpackage xul
  (:use :cl :alexandria)
  (:export #:define-component
	   #:render
	   #:component
	   #:get-component
	   #:add-component
	   #:remove-component
	   #:children
	   #:parent
	   #:with-child-components
	   #:xul-application
	   #:run-app
	   #:on-command=
	   #:on-command=*
	   #:on-change=
	   #:on-change=*
	   #:on-select=
	   #:on-select=*
	   #:on-input=
	   #:on-input=*
	   #:file
	   #:src=
	   #:image=
	   #:open-window
	   #:with-open-window
	   #:open-dialog
	   #:with-open-dialog
	   #:ask
	   #:prompt
	   #:inform))

(defpackage xul-builder
  (:nicknames :<)
  (:use :cl))

(defpackage html-builder
  (:nicknames :<html)
  (:use :cl))

(defpackage xul-widgets
  (:nicknames :w)
  (:use :cl))
