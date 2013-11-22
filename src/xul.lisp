(in-package :xul)

(defclass xul-sink (cxml::sink)
  ())

(defmethod sax:start-document ((sink xul-sink))
  (unless (or (cxml::canonical sink)
	      (cxml::sink-omit-xml-declaration-p sink))
    (call-next-method)
    (cxml::sink-write-rod #"<?xml-stylesheet href=\"chrome://global/skin/\" type=\"text/css\"?>" sink)
    (cxml::sink-write-rune #/U+000A sink)))

(defun make-xul-character-stream-sink (stream &rest initargs
				       &key encoding &allow-other-keys)
  (let* ((encoding (or encoding "UTF-8"))
	 (ystream (cxml::make-character-stream-ystream stream)))
    (setf (cxml::ystream-encoding ystream)
	  (runes:find-output-encoding encoding))
    (apply #'make-instance
	   'xul-sink
	   :ystream ystream
	   :encoding encoding
	   initargs)))

(defclass xul-application ()
  ((name :initarg :name
	 :initform (error "Provide the application name")
	 :accessor name
	 :type string
	 :documentation "Specifies the application name.")
   (vendor :initarg :vendor
	   :initform nil
	   :accessor vendor
	   :type string
	   :documentation "The application vendor.")
   (version :initarg :version
	    :initform "1.0"
	    :accessor version
	    :type string
	    :documentation "Specifies the application version number.")
   (build-id :initarg :build-id
	     :initform (error "Provide the build id")
	     :accessor build-id
	     :type string
	     :documentation "Specifies a unique build identifier. This is typically a date identifier, and should be different for every released version of an application.")
   (id :initarg :id
       :initform (error "Provide the application id")
       :accessor id
       :type string
       :documentation "Specifies the unique application ID. REQUIRED. The application ID, like extension IDs, can be formatted either like an email ApplicationName@vendor.tld or a UUID {12345678-1234-1234-1234-123456789abc}. The email format is recommended for newly developed applications. Example: ID=TestApplication@example.tld")
   (profile :initarg :profile
	    :initform nil
	    :accessor profile
	    :type pathname
	    :documentation "Specifies the path to use for the application's profile, based within the user's application data directory. OPTIONAL. Example: Profile=MyAppData")
   (gecko-min-version :initarg :gecko-min-version
		      :initform "1.8"
		      :accessor gecko-min-version
		      :type string
		      :documentation "Specifies the minimum XULRunner version needed by this application. If there are binary components, MinVersion must equal the version of the libxul SDK which is used to build the application.")
   (gecko-max-version :initarg :gecko-max-version
		      :initform "200.*"
		      :accessor gecko-max-version
		      :type string
		      :documentation "Specify the maximum XULRunner version needed by this application. OPTIONAL - default value is any XULRunner less than XULRunner 2")
   (top :initarg :top
	 :initform (error "Provide the top element")
	 :accessor top
	 :type xul-element)))

(defmethod print-object ((app xul-application) stream)
  (print-unreadable-object (app stream :type t :identity t)
    (format stream "~s" (name app))))

(defun run-xul (app)
  )

(defclass xul-element ()
  (align allow-events allow-negative-assertions
	 class coalesce-duplicate-arcs collapsed container containment context
	 context-menu data-sources dir empty equal-size flags flex height hidden id
	 insert-after insert-before left max-height max-width menu min-height min-width mouse-through
	 observes ordinal (orient :initarg :orient
	   :initform nil
	   :accessor orient
	   :type (or :horizontal :vertical)) pack persist popup position preference-editable query-type
	 ref remove-element sort-direction sort-resource sort-resource-2 status-text style
	 template tooltip tooltip-text top uri wait-cursor width ))

(defclass container-element (xul-element)
  ((children :initarg :children
	     :initform nil
	     :accessor children)
   ))

(defclass box (container-element)
  ())

(defclass hbox (box)
  ()
  (:default-initargs :orient :horizontal))

(defclass vbox (box)
  ()
  (:default-initargs :orient :vertical))
  
(defclass xul-control (xul-element)
  ())

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

(defclass label (xul-control)
  ((accesskey :initarg :accesskey
	      :accessor accesskey)
   (control :initarg :control
	    :accessor control)
   crop disabled href
   (value :initarg :value
	  :accessor value)))

(defmethod serialize-xul ((window window))
  (cxml:with-element "window"
    (cxml:attribute "xmlns" "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")
    (loop for child in (children window)
	 do (serialize-xul child))))

(defmethod serialize-xul ((label label))
  (cxml:with-element "label"
    (cxml:attribute "control" (control label))
    (cxml:attribute "accesskey" (accesskey label))
    (cxml:attribute "value" (value label))))

(defmethod serialize-xul ((vbox vbox))
  (cxml:with-element "vbox"
    (loop for child in (children vbox)
	 do (serialize-xul child))))
	    
(defun make-app-folder (app path)
  (ensure-directories-exist path)
  (let* ((chrome-path (merge-pathnames #p"chrome/" path))
	 (defaults-path (merge-pathnames #p"defaults/" path))
	 (content-path (merge-pathnames #p"content/" chrome-path))
	 (preferences-path (merge-pathnames #p"preferences/" defaults-path)))
    (ensure-directories-exist chrome-path)
    (ensure-directories-exist content-path)
    (ensure-directories-exist defaults-path)
    (ensure-directories-exist preferences-path)

    (with-open-file (stream (merge-pathnames "application.ini" path) :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (generate-initialization app stream))

    (with-open-file (stream (merge-pathnames "chrome.manifest" path) :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (generate-chrome-manifest app stream))

    (with-open-file (stream (merge-pathnames "prefs.js" preferences-path) :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (generate-preferences app stream))

    (with-open-file (stream (merge-pathnames "main.xul" content-path) :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (generate-xul app stream))))

(defun generate-initialization (app stream)
  (format stream "[App]~%")
  (format stream "Vendor=~a~%" (vendor app))
  (format stream "Name=~a~%" (name app))
  (format stream "Version=~a~%" (version app))
  (format stream "BuildID=~a~%" (build-id app))
  (format stream "ID=~a~%" (id app))
  (format stream "~%[Gecko]~%")
  (format stream "MinVersion=~a~%" (gecko-min-version app))
  (format stream "MaxVersion=~a~%" (gecko-max-version app)))

(defun generate-chrome-manifest (app stream)
  (format stream "content myapp file:chrome/content/"))

(defun generate-preferences (app stream)
  (format stream "pref(\"toolkit.defaultChromeURI\", \"chrome://myapp/content/main.xul\");~%")
  (format stream "/* debugging prefs, disable these before you deploy your application! */~%")
  (format stream "pref(\"browser.dom.window.dump.enabled\", true);~%")
  (format stream "pref(\"javascript.options.showInConsole\", true);~%")
  (format stream "pref(\"javascript.options.strict\", true);~%")
  (format stream "pref(\"nglayout.debug.disable_xul_cache\", true);~%")
  (format stream "pref(\"nglayout.debug.disable_xul_fastload\", true);~%"))

(defun generate-xul (app stream)
  (let ((output (make-xul-character-stream-sink stream)))
      (cxml:with-xml-output output
	(serialize-xul (top app)))))

(defparameter *xul-runner* "/usr/bin/firefox" "The xul runner. It can be the XUL runner or Mozilla firefox")

(defun run-app (app)
  (let ((app-folder (pathname (format nil "/tmp/~A/" (name app)))))
    (make-app-folder app app-folder)
    (sb-ext:run-program *xul-runner* (list "-app" (format nil"~Aapplication.ini" app-folder)))))

(defmacro xul (xul)
  `(make-xul ',xul))

(defun make-xul (xul)
  (destructuring-bind (xul-element attributes &rest children) xul
    (if children
	(apply #'make-instance xul-element (cons :children
						 (cons (mapcar #'make-xul children)
						       attributes)))
	(apply #'make-instance xul-element attributes))))
