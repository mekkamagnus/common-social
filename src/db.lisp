(in-package :common-social.db)

(defparameter *db-connection* nil
  "Current database connection")

(defun get-connection ()
  "Get or create database connection"
  (unless *db-connection*
    (setf *db-connection* 
          (sqlite:connect common-social.config:*database-path*)))
  *db-connection*)

(defun execute-query (sql &rest params)
  "Execute SQL query with parameters and return results"
  (sqlite:execute-to-list (get-connection) sql params))

(defun execute-non-query (sql &rest params)
  "Execute non-query SQL (INSERT, UPDATE, DELETE)"
  (sqlite:execute-non-query (get-connection) sql params))

(defun init-database ()
  "Initialize database schema"
  (let ((conn (get-connection)))
    ;; Create posts table
    (sqlite:execute-non-query conn
      "CREATE TABLE IF NOT EXISTS posts (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         content TEXT NOT NULL CHECK(length(content) <= 280 AND length(trim(content)) > 0),
         created_at DATETIME DEFAULT CURRENT_TIMESTAMP
       )")
    
    ;; Create index for performance
    (sqlite:execute-non-query conn
      "CREATE INDEX IF NOT EXISTS idx_posts_created_at ON posts(created_at DESC)")
    
    (format t "Database initialized successfully~%")))

(defun close-connection ()
  "Close database connection"
  (when *db-connection*
    (sqlite:disconnect *db-connection*)
    (setf *db-connection* nil)))