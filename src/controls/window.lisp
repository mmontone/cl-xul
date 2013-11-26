(in-package :xul)

(define-xul-element window (container-element)
  (accelerated chrome-margin disable-chrome disable-fast-find draw-in-title-bar
	       fullscreen-button hide-chrome id lightweight-themes
	       lightweight-themes-footer screenX screenY
	       size-mode title window-type))

(defmethod serialize-xul ((window window))
  (cxml:with-element "window"
    (cxml:attribute "xmlns" "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")
    (loop for attribute in (attributes (class-of window))
	 when (slot-boundp window (closer-mop:slot-definition-name attribute))
	 do
	   (cxml:attribute (attribute-name attribute)
			   (serialize-xml-value
			    (slot-value window (closer-mop:slot-definition-name attribute)))))
    ;; Bind javascripts
    ;; Bind javascript files
  (loop for javascript in (javascripts *app*)
     do 
       (cxml:with-element "script"
	 ;(cxml:attribute "language" "javascript")
	 (cxml:attribute "type" "application/javascript")
	 (cxml:attribute "src" (format nil "chrome://myapp/content/a~a.~a"
				       (pathname-name javascript)
				       (pathname-type javascript)))))
  (loop for child in (children window)
     do (serialize-xul child))))
	    
