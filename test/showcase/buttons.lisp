(in-package :xul-test)

(define-component buttons-showcase ()
  ((current-state :initform "(no input yet)"
		  :accessor current-state)
   (access-state :initform "(no input yet)"
		 :accessor access-state)
   (state-state :initform "(no input yet)"
		:accessor state-state)
   (labeling-state :initform "(no input yet)"
		   :accessor labeling-state))
  (:render (comp)
	   (<:vbox ;(<:flex= 1)
		   (<:style= "overflow: auto")
		   (<:description "XUL Buttons")
		   (render-buttons-tab comp)
		   (render-buttons-access comp)
		   (<:hbox
		     (render-buttons-states comp)
		     (render-menu-buttons comp))
		   (render-buttons-labeling comp))))

(defun render-buttons-tab (comp)
  (<:group-box
    (<:caption (<:label= "These buttons tab oddly."))
    (flet ((tab-button (number)
	     (<:button (<:flex= 1)
		       (<:label= number)
		       (<:tabindex= number)
		       (on-command=*
			 (setf (current-state comp) number)))))
      (<:hbox
	(mapcar #'tab-button (list 1 2 3 4 5 6))))
    (<:hbox (<:pack= "center")
	    (<:description (princ-to-string (current-state comp))))))

(defun render-buttons-access (comp)
  (<:group-box
    (<:caption (<:label= "These buttons have access keys."))
    (flet ((access-button (label access-key)
	     (<:button
	       (<:flex= 1)
	       (<:label= label)
	       (<:accesskey= access-key)
	       (on-command=* (setf (access-state comp) label)))))
      
      (<:hbox 
	(mapcar (lambda (args)
		  (apply #'access-button args))
		'(("Animal" "A")
		  ("Bear" "B")
		  ("Cat" "C")
		  ("Dog" "D")
		  ("Deer" "E")
		  ("Fish" "F")))))
    (<:hbox (<:pack= :center)
	    (<:description (princ-to-string (access-state comp))))))
  
(defun render-buttons-states (comp)
  (<:group-box
    (<:caption (<:label= "These buttons have different button states."))
    (<:hbox 
      (<:button (<:flex= 1)
		(<:label= "Default")
		(<:default= t)
		(on-command=* (setf (state-state comp) "Default")))
      (<:button (<:flex= 1)
		(<:label= "Checked")
		(<:checked= t)
		(on-command=* (setf (state-state comp) "Checked")))
      (<:button (<:flex= 1)
		(<:label= "Normal")
		(on-command=* (setf (state-state comp) "Normal")))
      (<:button (<:flex= 1)
		(<:label= "Disabled")
		(<:disabled= t)
		(on-command=* (setf (state-state comp) "Disabled"))))
    (<:hbox (<:pack= :center)
	    (<:description (princ-to-string (state-state comp))))))

(defun render-buttons-labeling (comp)
  (<:group-box
   (<:caption (<:label= "These buttons show different labeling."))
   (let ((image (asdf:system-relative-pathname :cl-xul-test
					       "test/showcase/images/folder_yellow_open.png")))
     (<:hbox (<:pack= :center)
	     (<:vbox
	       (<:button (<:flex= 1)
			 (<:label "No image")
			 (on-command=* (setf (labeling-state comp) "A button with a label only")))
	       (<:button (<:label= "Left")
			 (image= image)
			 (on-command=* (setf (labeling-state comp) "A button with both label and image")))
	       (<:button (<:label= "Right")
			 (image= image)
			 (<:dir= :reverse)
			 (on-command=* (setf (labeling-state comp) "A button with the image to the right of the label"))))
	     (<:vbox
	       (<:button (<:label= "Above")
			 (image= image)
			 (<:orient= :vertical)
			 (<:dir= :forward)
			 (on-command=* (setf (labeling-state comp) "A button with the image above the label")))
	       (<:button (<:label= "Below")
			 (image= image)
			 (<:orient= :vertical)
			 (<:dir= :reverse)
			 (on-command=* (setf (labeling-state comp) "A button with the image above the label"))))
	     (<:vbox
	       (<:button (<:flex= 1)
			 (on-command=* (setf (labeling-state comp) "A button with neither image nor label")))
	       (<:button (image= image)
			 (on-command=* (setf (labeling-state comp) "A button with an image only")))))
     (<:hbox (<:pack= :center)
	     (princ-to-string (labeling-state comp))))))

(defun render-menu-buttons (comp))

