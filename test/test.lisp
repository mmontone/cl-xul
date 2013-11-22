(in-package :xul)

(defparameter *output* (make-xul-character-stream-sink *standard-output*))

(cxml:with-xml-output *output*
  (cxml:with-element "window"
    (cxml:attribute "xmlns" "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")
    (cxml:with-element "label"
      (cxml:attribute "control" "my-label")
      (cxml:attribute "accesskey" "a")
      (cxml:attribute "value" "Hello"))))

(defparameter *test-app* (make-instance 'xul-application
					:name "TestApp"
					:top (make-instance 'window :title "Test application"
							    :children (list (make-instance 'label
											   :control "hello-label"
											   :accesskey "a"
											   :value "Hello")
									    (make-instance 'label
											   :control "bye-label"
											   :accesskey "b"
											   :value "Bye")))
					:build-id "0001"
					:id "TestApplication"))

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

(make-app-folder *test-app* #p"/home/marian/src/lisp/cl-xul/test/app/")

(defun run-app (app)
  (let ((app-folder (pathname (format nil "/tmp/~A/" (name app)))))
    (make-app-folder app app-folder)
    (sb-ext:run-program "/usr/bin/xulrunner" (list "-app" (format nil"~Aapplication.ini" app-folder)))))

(run-app *test-app*)
