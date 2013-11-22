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
					:name "Test application"
					:top (make-instance 'window :title "Test application")
					:build-id "0001"
					:id "TestApplication"))

(defun make-app-folder (app path)
  (ensure-directories-exist path)
  (let* ((chrome-path (merge-pathnames #p"chrome" path))
	 (default-path (merge-pathnames #p"default" path))
	 (content-path (merge-pathnames #p"content" chrome-path))
	 (preferences-path (merge-pathnames #p"preferences" default-path)))
    (ensure-directories-exist chrome-path)
    (ensure-directories-exist content-path)
    (ensure-directories-exist default-path)
    (ensure-directories-exist preferences-path)

    (with-open-file (stream (merge-pathnames "application.ini" path) :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (format stream "[App]~%")
      (format stream "Vendor=~a~%" (vendor app))
      (format stream "Name=~a~%" (name app))
      (format stream "Version=~a~%" (version app))
      (format stream "BuildID=~a~%" (build-id app))
      (format stream "ID=~a~%" (id app))
      (format stream "~%[Gecko]~%")
      (format stream "MinVersion=~a~%" (gecko-min-version app))
      (format stream "MaxVersion=~a~%" (gecko-max-version app))))) 

(make-app-folder *test-app* #p"/home/marian/src/lisp/cl-xul/test/app/")
