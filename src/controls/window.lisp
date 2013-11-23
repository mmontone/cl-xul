(in-package :xul)

(defclass window (container-element)
  (accelerated chrome-margin disable-chrome disable-fast-find draw-in-title-bar
	       fullscreen-button hide-chrome id lightweight-themes
	       lightweight-themes-footer screenX screenY
	       (size-mode :initarg :size-mode
			  :accessor size-mode
			  :initform :normal
			  :type (or :maximized :normal))
	       (title :initarg :title
		      :accessor title
		      :initform (error "Provide the window title")
		      :type string)
	       window-type))

(defmethod serialize-xul ((window window))
  (cxml:with-element "window"
    (cxml:attribute "xmlns" "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")
    (loop for child in (children window)
	 do (serialize-xul child))))
	    
