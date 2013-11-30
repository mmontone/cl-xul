(in-package :xul)

(define-component buttons-showcase ()
  ((current-state :initform "(no input yet)"
		  :accessor current-state)
   (access-state :initform "(no input yet)"
		 :accessor access-state)
   (state-state :initform "(no input yet)"
		:accessor state-state))
  (:render (comp)
	   (<:vbox (<:flex= 1)
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

(defun render-menu-buttons (comp))
(defun render-buttons-labeling (comp))
