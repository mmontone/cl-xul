(in-package :xul-test)

(define-component calculator ()
  ((numbers :initform nil
	    :accessor numbers
	    :make-component-dirty-p nil)
   (operation :initform nil
	      :accessor operation
	      :make-component-dirty-p nil)
   (clean-display-p :initform nil
		    :accessor clean-display-p
		    :make-component-dirty-p nil))
  (:initialize (calc)
	       (flet ((process-operation (operation)
			(setf (clean-display-p calc) t)
			(xul::with-child-components (display) calc
			  (if (operation calc)
			      (progn
				(push (w::value display)
				      (numbers calc))
				(let ((result
				       (apply (operation calc)
					      (reverse (numbers calc)))))
				  (setf (w::value display)
					result)
				  (setf (operation calc) operation)
				  (setf (numbers calc) (list result))))
			      ;; else
			      (progn
				(setf (operation calc) operation)
				(push (w::value display)
				      (numbers calc))
				(setf (w::value display) 0)
				)))))
		 (add-component calc 'display (w::text-box :value 0))
		 (loop for i from 1 to 9 do
		      (add-component calc
				     (intern (format nil "BUTTON-~A" i))
				     (w::button :label (princ-to-string i)
						:on-command
						(let ((number i))
						  (lambda ()
						    (xul::with-child-components (display) calc
						      (when (clean-display-p calc)
							(setf (w::value display) 0)
							(setf (clean-display-p calc) nil))
						      (setf (w::value display)
							    (parse-integer
							     (format nil "~A~A"
								     (w::value display)
								     number)))))))))
		 (add-component calc 'plus-button
				(w::button :label "+"
					   :on-command
					   (lambda ()
					     (process-operation #'+))))
		 (add-component calc 'minus-button
				(w::button :label "-"
					   :on-command
					   (lambda ()
					     (process-operation #'-))))
		 (add-component calc 'division-button
				(w::button :label "/"
					   :on-command
					   (lambda ()
					     (process-operation
					      (alexandria:compose #'truncate #'/)))))
		 (add-component calc 'mult-button
				(w::button :label "*"
					   :on-command
					   (lambda ()
					     (process-operation #'*))))
		 (add-component calc 'reset-button
				(w::button :label "Reset"
					   :on-command
					   (lambda ()
					     (setf (clean-display-p calc) nil)
					     (setf (numbers calc) nil)
					     (setf (operation calc) nil)
					     (setf (w::value (get-component calc 'display)) 0))))
		 (add-component calc 'result-button
				(w::button :label "="
					   :on-command
					   (lambda ()
					     (setf (clean-display-p calc) t)
					     (xul::with-child-components (display)
						 calc
					       (push (w::value display)
						     (numbers calc))
					       (when (operation calc)
						 (let ((result
							(apply (operation calc)
							       (reverse (numbers calc)))))
						   (setf (w::value display)
							 result)
						   (setf (numbers calc) (list result))))
					       (setf (operation calc) nil)))))))
  (:render (calc)
	   (<:vbox
	     (xul::with-child-components (display) calc
	       (render display))
	     (xul::with-child-components
		 (button-1 button-2 button-3
			   button-4 button-5 button-6
			   button-7 button-8 button-9)
		 calc
	       (<:vbox
		 (<:hbox (render button-1)
			 (render button-2)
			 (render button-3))
		 (<:hbox (render button-4)
			 (render button-5)
			 (render button-6))
		 (<:hbox (render button-7)
			 (render button-8)
			 (render button-9))))
	     (xul::with-child-components (plus-button
					  reset-button
					  result-button
					  minus-button
					  division-button
					  mult-button)
		 calc
	       (<:vbox
		 (<:hbox
		   (render plus-button)
		   (render reset-button)
		   (render result-button))
		 (<:hbox
		   (render minus-button)
		   (render mult-button)
		   (render division-button)))))))

(defun run-calculator ()
  (let ((app (make-instance 'xul-application
		 :name "calculator"
		 :root-component (make-instance 'calculator)
		 :build-id "0001"
		 :id "CalculatorApplication")))
    (run-app app)))
