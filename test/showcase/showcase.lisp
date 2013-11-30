(in-package :xul)

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
					   (file-string source))))))))

(defparameter *showcase-pages*
  (flet ((source (filename)
	   (asdf:system-relative-pathname :cl-xul (format nil "test/showcase/~A" filename))))
    `(("Buttons"
       :component buttons-showcase
       :source ,(source "buttons.lisp"))
      ("Checkboxes"
       :component checkboxes-showcase
       :source ,(source "checkboxes.lisp"))
      ("Radiobuttons"
       :component radiobuttons-showcase
       :source ,(source "radiobuttons.lisp"))
      ("Lists"
       :component lists-showcase
       :source ,(source "lists.lisp"))
      ("Embedded"
       :component embedded-showcase
       :source ,(source "embedded.lisp")))))

(defparameter *showcase-app*
  (make-instance 'xul-application
		 :name "showcase"
		 :root-component (make-instance 'showcase)
		 :build-id "0001"
		 :id "Showcase"))

(defun showcase ()
  (run-app *showcase-app*)) 
