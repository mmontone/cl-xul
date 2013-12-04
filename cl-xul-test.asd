(require :asdf)

(defpackage xul-test-system
  (:use :cl :asdf))

(in-package xul-test-system)

(defsystem cl-xul-test
    :description "Common Lisp Mozilla XUL bindings tests"
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
    ((:module test
	      :components
	      ((:file "package")
	       (:module showcase
			:components
			((:file "showcase")
			 (:file "buttons")
			 (:file "checkboxes")
			 (:file "radiobuttons")
			 (:file "images")
			 (:file "lists")
			 (:file "trees")
			 (:file "html")
			 (:file "dialogs")
			 (:file "components"))
			:serial t)
	      (:file "info"))
	      :serial t))
    :serial t
    :depends-on
    (:cl-xul
     :fiveam))
