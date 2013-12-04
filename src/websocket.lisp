(in-package :xul)

(defparameter *websocket-port* 9998)
(defparameter *server* nil)
(defparameter *client* nil)

(defun start-websocket-server ()
  (when (not *server*)
    (setf *server*
	  (bordeaux-threads:make-thread
	   (lambda ()
	     (clws:run-server *websocket-port*))
	   :name "XUL websocket server"))))

(defun start-resource-listener ()
  (bordeaux-threads:make-thread
   (lambda ()
     (clws:run-resource-listener
      (clws:find-global-resource "/xul")))
   :name "resource listener for /xul"))

(defclass xul-resource (clws:ws-resource)
  ())

(clws:register-global-resource
       "/xul"
       (make-instance 'xul-resource)
					;(clws:origin-prefix "http://127.0.0.1" "http://localhost")
					;(clws:origin-prefix "ws://127.0.0.1" "ws://localhost")
       #'ws::any-origin)

(defmethod clws:resource-client-connected ((res xul-resource) client)
  #+nil(break "got connection on xul server from ~s : ~s~%"
	  (clws:client-host client)
	  (clws:client-port client))
  (format t "got connection on xul server from ~s : ~s~%"
	  (clws:client-host client)
	  (clws:client-port client))
  t)

(defmethod clws:resource-client-disconnected ((resource xul-resource) client)
  ;(break "Client disconnected from resource ~A: ~A~%" resource client)
  (format t "Client disconnected from resource ~A: ~A~%" resource client))  

(defmethod clws:resource-received-text ((res xul-resource) client message)
  ;(break "got frame ~s from client ~s" message client)
  (format t "got frame ~s from client ~s" message client)
  ;(clws:write-to-client-text client message)
  (let ((object (ignore-errors (json:decode-json-from-string message))))
    ;(break "~A" object)
    (let ((*client* client))
      (if (not object)
	  (error "~A could'nt be parsed" message)
					;else
	  (cond
	    ((equalp (cdr (assoc :type object)) "callback")
	     (handle-callback object))
	    (t (break "Don't know how to handle ~A" object))))
      t)))

(defmethod clws:resource-received-binary((res xul-resource) client message)
  ;(break "got binary frame ~s from client ~s" (length message) client)
  (format t "got binary frame ~s from client ~s" (length message) client)
  (clws:write-to-client-binary client message))
