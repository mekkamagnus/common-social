(in-package :common-social.hotload)

;; Hot loading system for Common Social

(defparameter *hotload-thread* nil
  "Background thread for watching hot load files")

(defparameter *hotload-file* "hotload-commands.lisp"
  "File to watch for hot load commands")

(defparameter *last-modified* 0
  "Last modification time of hotload file")

(defparameter *hotload-enabled* nil
  "Whether hot loading is currently enabled")

(defun check-and-reload ()
  "Check if hotload file was modified and reload it"
  (when (and *hotload-enabled* (probe-file *hotload-file*))
    (let ((current-time (file-write-date *hotload-file*)))
      (when (> current-time *last-modified*)
        (setf *last-modified* current-time)
        (format t "🔥 Hot loading ~A at ~A~%" 
                *hotload-file* 
                (local-time:format-timestring nil (local-time:now)))
        (handler-case
            (load *hotload-file*)
          (error (e)
            (format t "❌ Hot load error: ~A~%" e)))))))

(defun start-hotload-watcher (&optional (interval 2))
  "Start background thread to watch for file changes"
  (when *hotload-thread*
    (stop-hotload-watcher))
  
  (setf *hotload-enabled* t)
  (setf *hotload-thread*
        (make-thread
         (lambda ()
           (loop
             (when *hotload-enabled*
               (check-and-reload))
             (sleep interval)))
         :name "common-social-hotload-watcher"))
  
  (format t "🔥 Common Social hot loading enabled~%")
  (format t "📁 Watching: ~A~%" *hotload-file*)
  (format t "⏱️  Check interval: ~A seconds~%" interval))

(defun stop-hotload-watcher ()
  "Stop the hot loading watcher thread"
  (setf *hotload-enabled* nil)
  (when *hotload-thread*
    (destroy-thread *hotload-thread*)
    (setf *hotload-thread* nil))
  (format t "🛑 Hot loading disabled~%"))

(defun hotload-status ()
  "Get current hot loading status"
  `(("enabled" . ,*hotload-enabled*)
    ("thread_alive" . ,(and *hotload-thread* 
                            (thread-alive-p *hotload-thread*)))
    ("watching_file" . ,*hotload-file*)
    ("last_modified" . ,*last-modified*)
    ("current_time" . ,(get-universal-time))))