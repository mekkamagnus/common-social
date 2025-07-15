(in-package :common-social)

(defparameter *acceptor* nil)

(defun start-server (&optional (port common-social.config:*server-port*))
  "Start the web server with memory optimization"
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  
  ;; Initialize database
  (common-social.db:init-database)
  
  ;; Create and start acceptor
  (setf *acceptor* 
        (make-instance 'hunchentoot:easy-acceptor :port port))
  
  ;; Configure error handling
  (setf hunchentoot:*catch-errors-p* (not common-social.config:*debug-mode*)
        hunchentoot:*show-lisp-errors-p* common-social.config:*debug-mode*)
  
  ;; Setup routes
  (common-social.handlers:setup-routes)
  
  ;; Start server
  (hunchentoot:start *acceptor*)
  (format t "✅ Common Social server started on port ~A~%" port)
  (format t "🌐 Visit: http://localhost:~A~%" port)
  (format t "💾 Memory usage: ~A MB~%" (/ (sb-vm::dynamic-usage) 1048576)))

(defun stop-server ()
  "Stop the web server"
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (common-social.db:close-connection)
  (format t "🛑 Server stopped~%"))

(defun restart-server (&optional (port common-social.config:*server-port*))
  "Restart the web server"
  (stop-server)
  (sleep 1)
  (start-server port))