(in-package :xul-test)

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(define-component showcase ()
  ((selected-page :accessor selected-page
		  :initform (first *showcase-pages*)))
  (:initialize (showcase)
	       (add-component showcase 'child
			      (make-instance (getf (cdr (selected-page showcase)) :component))))
  (:render (showcase)
	   (<:vbox (<:flex= 1)
		   (<:toolbox
		     (<:menu-bar
		       (<:menu (<:label= "File")
			       (<:accesskey= "f")
			       (<:menu-popup
				 (<:menu-item (<:label= "Quit")
					      (<:on-command= "window.close();"))))
		       (<:menu (<:label= "Help")
			       (<:accesskey= "h")
			       (<:menu-popup
				 (<:menu-item (<:label= "About")
					      (<:on-command= (show-help-about)))))))
		   (<:hbox (<:flex= 1)
			   (<:list-box
			     (<:style= "width:10em")
			     (on-select= (lambda (index)
					   (setf (selected-page showcase)
						 (nth index *showcase-pages*))
					   (add-component showcase 'child
							  (make-instance (getf (cdr (selected-page showcase)) :component)))))
			     (loop for page in *showcase-pages*
				do
				  (<:list-item (<:label (car page))
					       (when (equalp (selected-page showcase)
							     page)
						 (<:selected= t)))))
			   (<:splitter (<:collapse= :before)
				       (<:grippy))
			   (<:tab-box (<:flex= 1)
				      (<:tabs
					(<:tab (<:label= "Examples"))
					(<:tab (<:label= "Source")))
				      (<:tab-panels
					(<:tab-panel (<:height= 800)
						     (<:width= 600)
						     (<:vbox (<:flex= 1)
							     (<:style= "overflow:auto")
							     (render (get-component showcase 'child))))
					(<:tab-panel (<:style= "overflow:auto;")
						     (<:description (<:style= "white-space: pre-wrap;")
								    (let ((source (getf (cdr (selected-page showcase)) :source)))
								      (file-string source))))))))))

(defun show-help-about ()
  (xul::with-open-dialog
      ("About"
       '(:modal "yes"
	 :resizable "no"))
    (<:dialog
      (<:id= "about")
      (<:title= "About")
      (<:buttons= "accept")
      (<:hbox
	     (<:image (src= (asdf:system-relative-pathname
			     :cl-xul-test
			     "test/showcase/images/lisplogo_warning2_256.png")))
	     (<:div
	       (<:h1 "CL-XUL")
	       (<:p "Library for Mozilla XUL GUI programming in Common Lisp")
	       (<:p "Project homepage: ")
	       (<:a (<:href= "https://github.com/mmontone/cl-xul")
		    ;(<:target= "_blank")
		    "https://github.com/mmontone/cl-xul")
	       (<:p "Author: Mariano Montone"))))))

(defparameter *showcase-pages*
  (flet ((source (filename)
	   (asdf:system-relative-pathname :cl-xul (format nil "test/showcase/~A" filename))))
    `(("Buttons"
       :component buttons-showcase
       :source ,(source "buttons.lisp"))
      ("Checkboxes"
       :component checkboxes-showcase
       :source ,(source "checkboxes.lisp"))
      ("Radio buttons"
       :component radio-buttons-showcase
       :source ,(source "radiobuttons.lisp"))
      ("Images"
       :component images-showcase
       :source ,(source "images.lisp"))
      ("Lists"
       :component lists-showcase
       :source ,(source "lists.lisp"))
      ("Dialogs"
       :component dialogs-showcase
       :source ,(source "dialogs.lisp"))
      ("Components"
       :component components-showcase
       :source ,(source "components.lisp"))
      ("Embedded"
       :component showcase
       :source ,(source "showcase.lisp")))))

(defun showcase ()
  (let ((app
	 (make-instance 'xul-application
			:name "showcase"
			:root-component (make-instance 'showcase)
			:build-id "0001"
			:id "Showcase")))
    (run-app app)))
