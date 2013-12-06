(in-package :xul-test)

(define-component lists-showcase ()
  ((single-state :initform "(no input yet)"
		 :accessor single-state))
  (:render (comp)
	   (<:vbox
	     (render-list-states comp)
	     (render-list-single-select comp)
	     (render-list-multiple-select comp)
	     (render-list-multiple-columns comp))))

(defun render-list-states (comp)
  (declare (ignore comp))
  (<:group-box
    (<:flex= 1)
    (<:caption (<:label= "States"))
    (<:list-box
      (<:rows= 5)
      (<:list-item (<:label= "Normal"))
      (<:list-item (<:label= "Selected")
		   (<:selected= t))
      (<:list-item (<:label= "Disabled")
		   (<:disabled= t))
      (<:list-item (<:label= "Checkbox")
		   (<:type*= :checkbox))
      (<:list-item (<:label= "Checked")
		   (<:type*= :checkbox)
		   (<:checked= t)))))

(defun render-list-single-select (comp)
  (<:group-box
    (<:flex= 1)
    (<:caption (<:label= "With single selection"))
    (let ((items (list "Pearl" "Aramis" "Yakima" "Tribble" "Cosmo")))
      (flet ((list-item (item)
			(<:list-item (<:label= item)
				     (when (equalp item
						   (single-state comp))
				       (<:selected= t)))))
	(<:list-box
	  (<:rows= 5)
	  (on-select=* (index)
	    (setf (single-state comp) (nth index items)))
	  (loop for item in items
	     do (list-item item)))))
    (<:hbox
      (<:pack= :center)
      (<:description
	(single-state comp)))))

(defun render-list-multiple-select (comp)
  (declare (ignore comp)))

(defun render-list-multiple-columns (comp)
  (declare (ignore comp)))

