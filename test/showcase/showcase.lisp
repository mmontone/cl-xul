(in-package :xul)

(define-component showcase ()
  ((selected-page :accessor selected-page))
  (:initialize (showcase))
  (:render (showcase)
	   (<:list-box
	     (<:style= "width:10em")
	     (on-select= (lambda (index)
			   (let* ((selected-page (nth index (pages showcase)))
				  (new-component (cadr selected-page)))
			     (setf (selected-page showcase) selected-page)
			     (add-component showcase 'child new-component))))
	     (loop for page in (pages showcase)
		do
		  (<:list-item (<:label (car page)))))
	   (<:splitter (<:collapse= :before)
		       (<:grippy))
	   (<:tab-box (<:flex= 1)
		      (<:tabs
			(<:tab (<:label= "Examples"))
			(<:tab (<:label= "Source")))
		      (<:tab-panels
			(<:tab-panel
			  (<:vbox (<:flex= 1)))
			(<:tab-panel
			  "Source code")))))

(defmethod pages ((showcase showcase))
  '(("Buttons" . buttons-showcase)
    ("Checkboxes" . checkboxes-showcase)
    ("Radiobuttons" . radiobuttons-showcase)
    ("Lists" . lists-showcase)))

(defparameter *showcase-app*
  (make-instance 'xul-application
		 :name "showcase"
		 :root-component (make-instance 'showcase)
		 :build-id "0001"
		 :id "Showcase"))		 
  
(defun showcase ()
  (run-app *showcase-app*)) 
