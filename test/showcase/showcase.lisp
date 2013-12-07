(in-package :xul-test)

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
      ("Text boxes"
       :component text-boxes-showcase
       :source ,(source "text-boxes.lisp"))
      ("Images"
       :component images-showcase
       :source ,(source "images.lisp"))
      ("Lists"
       :component lists-showcase
       :source ,(source "lists.lisp"))
      ("Tabs"
       :component tabs-showcase
       :source ,(source "tabs.lisp"))
      ("Trees"
       :component trees-showcase
       :source ,(source "trees.lisp"))
      ("Dialogs"
       :component dialogs-showcase
       :source ,(source "dialogs.lisp"))
      ("HTML"
       :component html-showcase
       :source ,(source "html.lisp"))
      ("Components"
       :component components-showcase
       :source ,(source "components.lisp"))
      ("Embedded"
       :component showcase
       :source ,(source "showcase.lisp")))))

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
				       (<:resizebefore= :closest)
				       (<:resizeafter= :grow)
				       (<:grippy))
			   (<:box (<:flex= 1)
			     (<:tab-box (<:flex= 1)
					
				      (<:tabs
					(<:tab (<:label= "Examples"))
					(<:tab (<:label= "Source")))
				      (<:tab-panels (<:flex= 1)
					(<:tab-panel
					  (<:vbox (<:flex= 1)
						  (<:style= "overflow:auto")
						  (render (get-component showcase 'child))))
					(<:tab-panel
					  (<:vbox (<:flex= 1)
						  (<:style= "overflow:auto;")
						  (<:description (<:style= "white-space: pre-wrap;")
								 (let ((source (getf (cdr (selected-page showcase)) :source)))
								   (file-string source)))))
					(<:tab-panel))))))))

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
	     (<html:div
	       (<html:style= "background-color:white;margin-left:10px; padding:10px;")
	       (<html:h1 "CL-XUL")
	       (<html:p "Library for Mozilla XUL GUI programming in Common Lisp")
	       (<html:p "Project homepage: ")
	       (<html:a (<:style= "color:blue;")
		    (<html:href= "http://mmontone.github.io/cl-xul")
		    ;(<:target= "_blank")
		    "http://mmontone.github.io/cl-xul")
	       (<html:p "Author: Mariano Montone"))))))

(defclass showcase-application (xul-application)
  ()
  (:default-initargs
   :name "showcase"
    :root-component (make-instance 'showcase)
    :build-id "0001"
    :id "Showcase"))

(defmethod xul::initialize-window ((app showcase-application))
  (<:window (<:title= (xul::name app))
	    (<:size-mode= :maximized)
	    (render (xul::root-component app))))

(defun showcase ()
  (run-app (make-instance 'showcase-application)))
