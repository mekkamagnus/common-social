(in-package :common-social.models)

(defun validate-post-content (content)
  "Validate post content meets requirements"
  (and (stringp content)
       (let ((trimmed (string-trim " " content)))
         (and (> (length trimmed) 0)
              (<= (length content) 280)))))

(defun create-post (content)
  "Create a new post and return its ID"
  (when (validate-post-content content)
    (let ((trimmed-content (string-trim " " content)))
      (execute-non-query
       "INSERT INTO posts (content) VALUES (?)"
       trimmed-content)
      
      ;; Get the last inserted row ID
      (caar (execute-query "SELECT last_insert_rowid()")))))

(defun get-all-posts (&key (limit 20) (offset 0))
  "Get all posts ordered by creation time (newest first)"
  (execute-query
   "SELECT id, content, created_at FROM posts 
    ORDER BY created_at DESC 
    LIMIT ? OFFSET ?"
   limit offset))

(defun get-post-count ()
  "Get total number of posts"
  (caar (execute-query "SELECT COUNT(*) FROM posts")))