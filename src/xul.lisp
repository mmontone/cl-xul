(in-package :xul)

(defparameter *current-element* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-xul (&body body)
    `(call-with-xul (lambda () ,@body)))

  (defmacro xul (xul)
    `(make-xul ',xul)))

(defun make-xul (xul)
  (destructuring-bind (xul-element attributes &rest children) xul
    (if children
	(apply #'make-instance xul-element (cons :children
						 (cons (mapcar #'make-xul children)
						       attributes)))
	(apply #'make-instance xul-element attributes))))


(defun call-with-xul (function)
  (let ((*current-element* t))
    (funcall function)))

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
   (xul :initarg :xul
	:initform nil
	:accessor app-xul
	:type xul-element)
   (root-component :initarg :root-component
		   :initform nil
		   :accessor root-component
		   :type component)
   (javascripts :initarg :javascripts
		:initform nil
		:accessor javascripts)))

(defmethod initialize-instance :after ((app xul-application) &rest initargs)
  (declare (ignore initargs))
  (register-application app)
  
  ;; Add xul.js to the list of javascript files
  (push (asdf::system-relative-pathname :cl-xul #p"src/xul.js")
	(javascripts app))
  )

(defmethod print-object ((app xul-application) stream)
  (print-unreadable-object (app stream :type t :identity t)
    (format stream "~s" (name app))))

(defun run-xul (app)
  )

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
      (generate-xul app stream))
    
    ;; Install javascript files
    (loop for javascript in (javascripts app)
       do
	 (let ((filename (make-pathname :name (pathname-name javascript)
					:type (pathname-type javascript))))
	   (cl-fad:copy-file javascript (merge-pathnames filename content-path) :overwrite t)))))
    
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
  (format stream "content ~A file:chrome/content/" (name app)))

(defun generate-preferences (app stream)
  (format stream "pref(\"toolkit.defaultChromeURI\", \"chrome://~A/content/main.xul\");~%" (name app))
  (format stream "/* debugging prefs, disable these before you deploy your application! */~%")
  (format stream "pref(\"browser.dom.window.dump.enabled\", true);~%")
  (format stream "pref(\"javascript.options.showInConsole\", true);~%")
  (format stream "pref(\"javascript.options.strict\", true);~%")
  (format stream "pref(\"nglayout.debug.disable_xul_cache\", true);~%")
  (format stream "pref(\"nglayout.debug.disable_xul_fastload\", true);~%")
  (format stream "pref(\"devtools.debugger.remote-enabled\", true);~%"))

(defun generate-xul (app stream)
  (let ((output (make-xul-character-stream-sink stream)))
    (cxml:with-xml-output output
      (cond
	((root-component app)
	 (let ((xul (with-xul
		      (initialize-window app))))
	   (serialize-xul xul)
	   
	   ;; Mark the whole root component clean before going on
	   (mark-clean (root-component app))
	   ))
	((app-xul app)
	 (serialize-xul (app-xul app)))
	(t (error "Either xul or root component is needed in application"))))
    stream))

(defgeneric initialize-window (app)
  (:method ((app xul-application))
    (<:window (<:title= (name app))
	      (render (root-component app)))))

(defparameter *xul-runner* "/usr/bin/firefox" "The xul runner. It can be the XUL runner or Mozilla firefox")

(defparameter *app* nil "Currently running application")

(defparameter *apps* (make-hash-table :test #'equalp))

(setf clws:*debug-on-server-errors* t) 
(setf clws:*debug-on-resource-errors* t)

(defun register-application (app)
  (setf (gethash (name app) *apps*) app))

(defun get-application-named (name)
  (or (gethash name *apps*)
      (error "Application named ~A not registered" name)))

(defun files-folder (app)
  (let ((app-folder (app-folder app)))
    (merge-pathnames "chrome/content/files/" app-folder)))

(defun file (pathname)
  (when (not (probe-file pathname))
    (error "~A not found" pathname))
  ;; Deploy the file rightaway
  (let ((filename
	 (replace-all 
	  (cl-base64:usb8-array-to-base64-string
	   (md5:md5sum-file pathname))
	  "/" "_")))
    (let ((target-filepath (make-pathname :name filename
					  :defaults (files-folder *app*))))
      (when (not (probe-file target-filepath))
	(ensure-directories-exist (files-folder *app*))
	(cl-fad:copy-file pathname target-filepath :overwrite t))
      (format nil "files/~A" filename))))

(defun src= (pathname)
  (<:src=
   (file pathname)))

(defun image= (pathname)
  (<:image=
   (file pathname)))

(defmethod app-folder ((app xul-application))
  (pathname (format nil "/tmp/~A/" (name app))))

(defun run-program (program args)
  #+sbcl(sb-ext:run-program program args)
  #+abcl(system:run-program program args))

(defun run-app (app &key (in-thread t))
  (let ((*app* app))
    (let ((app-folder (app-folder app)))
      
      ;; Deploy the application
      (make-app-folder app app-folder)

      ;; Start a web socket connection
      (start-websocket-server)
      (start-resource-listener)

      ;; Run
      (flet ((run ()
	       (run-program *xul-runner*
			    (list "-app"
				  (format nil"~Aapplication.ini"
					  app-folder)))))
	(if in-thread
	    (bordeaux-threads:make-thread #'run)
	    (run))
	app))))
