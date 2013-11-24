(require :asdf)

(defpackage xul-system
  (:use :cl :asdf))

(in-package xul-system)

(defsystem cl-xul
    :description "Mozilla XUL bindings"
    :version "0.1"
    :author "Mariano Montone <marianomontone@gmail.com>"
    :maintainer "Mariano Montone <marianomontone@gmail.com>"
    :licence "
Copyright (c) 2013 Mariano Montone

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE."
    :components
    ((:module src
	      :components
	      ((:file "package")
	       (:file "xul")
	       (:module controls :components
			((:file "mop")
			 (:file "control")
			 (:file "window")
			 (:file "box")
			 (:file "label")
			 (:file "button")
			 (:file "text")
			 (:file "menu")
			 (:file "list")
			 (:file "panel")
			 (:file "toolbar")
			 (:file "picker")
			 (:file "grid"))
			:serial t))
	      :serial t))
    :serial t
    :depends-on
    (:alexandria
     :hunchentoot
     :log5
     :fiveam
     :cl-who
     :parenscript
     :cxml
     :closer-mop))
