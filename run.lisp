#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- "$0" "$@"
|#
;; Common Social - Memory-optimized single-file server

;; Load only essential dependencies
(ql:quickload :hunchentoot :silent t)
(ql:quickload :cl-who :silent t)
(ql:quickload :sqlite :silent t)

;; Main package for Common Social
(defpackage :common-social
  (:use :cl)
  (:export #:start-server #:stop-server))

(in-package :common-social)

;; Global variables
(defparameter *server* nil)
(defparameter *db* nil)

;; Database functions
(defun init-db ()
  (unless *db*
    (setf *db* (sqlite:connect "common-social.db"))
    (sqlite:execute-non-query 
      *db*
      "CREATE TABLE IF NOT EXISTS posts (
         id INTEGER PRIMARY KEY AUTOINCREMENT,
         content TEXT NOT NULL CHECK(length(content) <= 280),
         created_at DATETIME DEFAULT CURRENT_TIMESTAMP
       )")
    (format t "✅ Database ready~%")))

(defun get-posts (&optional (limit 20))
  (when *db*
    (sqlite:execute-to-list 
      *db*
      "SELECT id, content, created_at FROM posts ORDER BY created_at DESC LIMIT ?" 
      limit)))

(defun create-post (content)
  (when (and *db* 
             (stringp content)
             (> (length (string-trim " " content)) 0)
             (<= (length content) 280))
    (sqlite:execute-non-query 
      *db*
      "INSERT INTO posts (content) VALUES (?)" 
      (string-trim " " content))
    t))

;; HTML rendering
(defun render-page ()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head 
      (:title "Common Social")
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:script :src "https://cdn.tailwindcss.com")
      (:script :src "https://unpkg.com/htmx.org@1.9.10"))
     (:body :class "bg-gray-50 min-h-screen"
      (:header :class "bg-white border-b shadow-sm"
       (:div :class "max-w-2xl mx-auto px-4 py-4"
        (:h1 :class "text-2xl font-bold text-blue-600" "🐦 Common Social")))
      
      (:main :class "max-w-2xl mx-auto px-4 py-6"
       ;; Post form
       (:div :class "bg-white rounded-lg shadow-sm border p-4 mb-6"
        (:form :hx-post "/posts" :hx-target "#timeline" :hx-swap "afterbegin"
         (:textarea :name "content" :rows "3" :maxlength "280"
                    :placeholder "What's happening?"
                    :class "w-full p-3 border rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
                    :oninput "updateCount(this)")
         (:div :class "flex justify-between items-center mt-3"
          (:span :id "count" :class "text-sm text-gray-500" "280")
          (:button :type "submit" 
                   :class "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded-lg font-medium"
                   "Post"))))
       
       ;; Timeline
       (:div :id "timeline"
        (dolist (post (get-posts))
          (let ((content (second post))
                (created-at (third post)))
            (cl-who:htm
             (:div :class "bg-white rounded-lg shadow-sm border p-4 mb-4"
              (:p :class "text-gray-900 mb-2" (cl-who:str content))
              (:div :class "text-sm text-gray-500" (cl-who:str created-at))))))))
      
      (:script 
       "function updateCount(textarea) {
          const remaining = 280 - textarea.value.length;
          document.getElementById('count').textContent = remaining;
        }")))))

(defun render-new-post ()
  (let ((content (hunchentoot:parameter "content")))
    (if (create-post content)
        (let ((new-posts (get-posts 1)))
          (when new-posts
            (let ((post (first new-posts)))
              (cl-who:with-html-output-to-string (*standard-output*)
                (:div :class "bg-white rounded-lg shadow-sm border p-4 mb-4"
                 (:p :class "text-gray-900 mb-2" (cl-who:str (second post)))
                 (:div :class "text-sm text-gray-500" (cl-who:str (third post))))))))
        "")))

;; Handlers
(defun handle-home ()
  (setf (hunchentoot:content-type*) "text/html")
  (render-page))

(defun handle-posts ()
  (setf (hunchentoot:content-type*) "text/html")
  (render-new-post))

;; Server management
(defun start-server (&optional (port 8008))
  (init-db)
  (when *server* (hunchentoot:stop *server*))
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:define-easy-handler (home :uri "/") () (handle-home))
  (hunchentoot:define-easy-handler (posts :uri "/posts") () (handle-posts))
  (hunchentoot:start *server*)
  (format t "✅ Common Social server running on http://localhost:~A~%" port))

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil))
  (when *db*
    (sqlite:disconnect *db*)
    (setf *db* nil)))

(defun restart-server (&optional (port 8008))
  (stop-server)
  (sleep 1)
  (start-server port))

;; Main execution for ros build
(defun main (&rest args)
  "Start the Common Social application"
  (declare (ignore args))
  (format t "🚀 Starting Common Social MVP...~%")
  (start-server)
  
  ;; Keep the server running
  (format t "Press Ctrl+C to stop the server~%")
  (handler-case
      (loop (sleep 1))
    (condition ()
      (format t "~%Shutting down...~%")
      (stop-server))))

;; Auto-start only when run as script (not when building)
#-ros.build
(main)