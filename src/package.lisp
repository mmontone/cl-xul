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
	   #:xul-application
	   #:run-app
	   #:on-command=
	   #:on-command=*
	   #:on-change=
	   #:on-change=*
	   #:on-select=
	   #:on-select=*
	   #:file
	   #:src=))

(defpackage xul-builder
  (:nicknames :<)
  (:use :cl))

(defpackage xul-widgets
  (:nicknames :w)
  (:use :cl))
