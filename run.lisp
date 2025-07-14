#!/usr/bin/env ros
;; Startup script for Common Social

(ql:quickload :common-social)

(defun main ()
  "Start the Common Social application"
  (format t "Starting Common Social MVP...~%")
  (common-social:start-server)
  
  ;; Keep the server running
  (format t "Press Ctrl+C to stop the server~%")
  (handler-case
      (loop (sleep 1))
    (condition ()
      (format t "~%Shutting down...~%")
      (common-social:stop-server))))

(main)