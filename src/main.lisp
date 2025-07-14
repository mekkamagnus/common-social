(in-package :common-social)

(defparameter *acceptor* nil)

(defun start-server (&optional (port common-social.config:*server-port*))
  "Start the web server"
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  
  ;; Initialize database
  (common-social.db:init-database)
  
  
  ;; Create and start acceptor
  (setf *acceptor* 
        (make-instance 'hunchentoot:easy-acceptor 
                       :port port
                       :document-root common-social.config:*static-directory*))
  
  ;; Configure error handling
  (setf hunchentoot:*catch-errors-p* (not common-social.config:*debug-mode*)
        hunchentoot:*show-lisp-errors-p* common-social.config:*debug-mode*)
  
  ;; Setup routes
  (common-social.handlers:setup-routes)
  
  ;; Start hot loading system
  (common-social.hotload:start-hotload-watcher)
  
  ;; Start server
  (hunchentoot:start *acceptor*)
  (format t "Server started on port ~A~%" port)
  (format t "Visit http://localhost:~A~%" port)
  (format t "🔥 Hot loading enabled - edit hotload-commands.lisp to update~%"))

(defun stop-server ()
  "Stop the web server"
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (common-social.hotload:stop-hotload-watcher)
  (common-social.db:close-connection)
  (format t "Server stopped~%"))

(defun restart-server (&optional (port common-social.config:*server-port*))
  "Restart the web server"
  (stop-server)
  (start-server port))