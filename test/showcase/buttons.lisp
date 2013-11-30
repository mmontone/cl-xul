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
    (<:hbox
      (<:button (<:flex= 1)
		(<:label= 6)
		(<:tabindex= 6)
		(on-command=*
		  (setf (current-state comp) 6)))
      (<:button (<:flex= 1)
		(<:label= 5)
		(<:tabindex= 5)
		(on-command=*  (setf (current-state comp) 5)))
      (<:button (<:flex= 1)
		(<:label= 4)
		(<:tabindex= 4)
		(on-command=*  (setf (current-state comp) 4)))
      (<:button (<:flex= 1)
		(<:label= 3)
		(<:tabindex= 3)
		(on-command=*  (setf (current-state comp) 3)))
      (<:button (<:flex= 1)
		(<:label= 2)
		(<:tabindex= 2)
		(on-command=*  (setf (current-state comp) 2)))
      (<:button (<:flex= 1)
		(<:label= 1)
		(<:tabindex= 1)
		(on-command=*  (setf (current-state comp) 1))))
    (<:hbox (<:pack= "center")
	    (<:description (princ-to-string (current-state comp))))))

(defun render-buttons-access (comp)
  (<:group-box
    (<:caption (<:label= "These buttons have access keys."))
    (<:hbox 
      (<:button (<:flex= 1)
		(<:label= "Animal")
		(<:accesskey= "A")
		(on-command=* (setf (access-state comp) "Animal")))
      (<:button (<:flex= 1)
		(<:label= "Bear")
		(<:accesskey= "Bear")
		(on-command=* (setf (access-state comp) "Bear")))
      (<:button (<:flex= 1)
		(<:label= "Cat")
		(<:accesskey= "C")
		(on-command=* (setf (access-state comp) "Cat")))
      (<:button (<:flex= 1)
		(<:label= "Dog")
		(<:accesskey= "D")
		(on-command=* (setf (access-state comp) "Dog")))
      (<:button (<:flex= 1)
		(<:label= "Deer")
		(<:accesskey= "E")
		(on-command=* (setf (access-state comp) "Deer")))
      (<:button (<:flex= 1)
		(<:label= "Fish")
		(<:accesskey= "F")
		(on-command=* (setf (access-state comp) "Fish"))))
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
