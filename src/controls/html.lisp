(in-package :xul)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-html-element
      (name super-elements attributes &rest options)
    `(progn
       (defclass ,name ,super-elements
	 ,attributes
	 (:metaclass xul-class)
	 ,@options)
     
       (defmacro ,(intern (symbol-name name) :html-builder)
	   (&body body)
	 `(let ((element (make-instance ',',name)))
	    (when (null *current-element*)
	      (error "Can't bind ~A to a null *current-element*. Make sure this is being called under a with-xul scope" element))
	    (if (equalp *current-element* t)
		(let ((*current-element* element))
		  (let ((content (progn ,@body)))
		    (when (stringp content)
		      (setf (content element) content))
		    element))
		(progn
		  (setf (children *current-element*) (append (children *current-element*) (list element)))
		  (let ((*current-element* element))
		    (let ((content (progn ,@body)))
		      (when (stringp content)
			(setf (content element) content))
		      element))
		  ))))
       
       (export ',(intern (symbol-name name) :html-builder) :html-builder)

       ,@(loop for attribute in attributes
	    collect
	      (let* ((attribute-name (if (symbolp attribute)
					 attribute
					 (first attribute)))
		     (attribute-builder-name (intern (format nil "~A=" attribute-name) :html-builder)))
		`(progn
		   (defmacro ,attribute-builder-name (value)
		     `(progn
			(setf (,',attribute-name *current-element*) ,value)
			nil))
		   (export ',attribute-builder-name :html-builder)))
	      ))))

(define-html-element html-element (xul::xml-element)
  (style))

(defmethod element-name ((element html-element))
  (let ((name (call-next-method)))
    (format nil "html:~A" name)))

(define-html-element a (html-element)
  (href target))

(define-html-element h1 (html-element)
  ())

(define-html-element div (html-element container-element)
  ())

(define-html-element p (html-element)
  ())
