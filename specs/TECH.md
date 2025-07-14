# Technical Design Documentation
# Common Social - Twitter-like Social Media MVP

## 1. System Architecture

### 1.1 High-Level Architecture
```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Web Browser   │◄──►│  Common Lisp     │◄──►│   SQLite DB     │
│  (HTMX/Tailwind)│    │  Application     │    │   (Local File)  │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                              │
                              ▼
                       ┌──────────────────┐
                       │   SMTP Server    │
                       │  (Magic Links)   │
                       └──────────────────┘
```

### 1.2 Application Layers
1. **Presentation Layer**: HTML templates with HTMX and Tailwind CSS
2. **Application Layer**: Common Lisp business logic and routing
3. **Data Layer**: SQLite database with direct SQL queries
4. **External Services**: SMTP for email delivery

### 1.3 Technology Stack
- **Runtime**: SBCL (Steel Bank Common Lisp) via Roswell
- **Web Server**: Hunchentoot HTTP server
- **Database**: SQLite with direct SQL bindings
- **Templating**: CL-WHO (Lisp-based HTML generation)
- **Frontend**: HTMX for interactions, Tailwind CSS for styling
- **Email**: cl-smtp for magic link delivery

## 2. Common Lisp Application Structure

### 2.1 Package Organization
```lisp
;; Main application packages
(defpackage :common-social
  (:use :cl)
  (:export #:start-server #:stop-server))

(defpackage :common-social.config
  (:use :cl)
  (:export #:*database-path* #:*server-port* #:*smtp-config*))

(defpackage :common-social.db
  (:use :cl :sqlite)
  (:export #:init-database #:get-connection))

(defpackage :common-social.models
  (:use :cl :common-social.db)
  (:export #:user #:post #:session #:magic-link))

(defpackage :common-social.auth
  (:use :cl :common-social.models)
  (:export #:generate-magic-link #:verify-magic-link #:create-session))

(defpackage :common-social.handlers
  (:use :cl :hunchentoot :common-social.models)
  (:export #:setup-routes))

(defpackage :common-social.utils
  (:use :cl)
  (:export #:generate-token #:hash-token #:timestamp-expired-p))
```

### 2.2 System Definition (common-social.asd)
```lisp
(asdf:defsystem #:common-social
  :description "Twitter-like social media application"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:hunchentoot
               #:sqlite
               #:cl-who
               #:cl-smtp
               #:alexandria
               #:local-time
               #:ironclad
               #:cl-json)
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "config")
                 (:file "utils")
                 (:file "db" :depends-on ("config"))
                 (:file "models" :depends-on ("db" "utils"))
                 (:file "auth" :depends-on ("models"))
                 (:file "handlers" :depends-on ("models" "auth"))
                 (:file "main" :depends-on ("handlers"))))))
```

## 3. Database Design

### 3.1 SQLite Schema
```sql
-- Users table
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    username TEXT UNIQUE NOT NULL,
    display_name TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Posts table  
CREATE TABLE posts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    content TEXT NOT NULL CHECK(length(content) <= 280),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- Sessions table
CREATE TABLE sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    token TEXT UNIQUE NOT NULL,
    expires_at DATETIME NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- Magic links table
CREATE TABLE magic_links (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT NOT NULL,
    token TEXT UNIQUE NOT NULL,
    expires_at DATETIME NOT NULL,
    used BOOLEAN DEFAULT FALSE,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for performance
CREATE INDEX idx_posts_user_id ON posts(user_id);
CREATE INDEX idx_posts_created_at ON posts(created_at DESC);
CREATE INDEX idx_sessions_token ON sessions(token);
CREATE INDEX idx_sessions_expires_at ON sessions(expires_at);
CREATE INDEX idx_magic_links_token ON magic_links(token);
CREATE INDEX idx_magic_links_email ON magic_links(email);
```

### 3.2 Database Connection Management
```lisp
(defparameter *db-connection* nil)

(defun get-db-connection ()
  "Get or create database connection"
  (unless *db-connection*
    (setf *db-connection* 
          (sqlite:connect (config:database-path))))
  *db-connection*)

(defun execute-query (sql &rest params)
  "Execute SQL query with parameters"
  (sqlite:execute-to-list (get-db-connection) sql params))

(defun execute-non-query (sql &rest params)
  "Execute non-query SQL (INSERT, UPDATE, DELETE)"
  (sqlite:execute-non-query (get-db-connection) sql params))
```

## 4. Authentication System

### 4.1 Magic Link Flow
```
1. User enters email address
2. System generates secure token
3. Token stored in magic_links table with expiration
4. Email sent with magic link URL
5. User clicks link with token
6. System verifies token and expiration
7. If valid: create user session, mark token as used
8. Redirect to application with session cookie
```

### 4.2 Magic Link Implementation
```lisp
(defun generate-magic-link (email)
  "Generate magic link for email address"
  (let ((token (utils:generate-secure-token))
        (expires-at (local-time:timestamp+ 
                     (local-time:now) 15 :minute)))
    (execute-non-query
     "INSERT INTO magic_links (email, token, expires_at) VALUES (?, ?, ?)"
     email token expires-at)
    (format nil "~A/auth/verify/~A" 
            (config:base-url) token)))

(defun verify-magic-link (token)
  "Verify magic link token and return user email if valid"
  (let ((result (execute-query
                 "SELECT email, expires_at, used FROM magic_links WHERE token = ?"
                 token)))
    (when (and result 
               (not (third (first result)))  ; not used
               (local-time:timestamp< (local-time:now) 
                                      (second (first result)))) ; not expired
      (execute-non-query
       "UPDATE magic_links SET used = TRUE WHERE token = ?"
       token)
      (first (first result))))) ; return email
```

### 4.3 Session Management
```lisp
(defun create-session (user-id)
  "Create new session for user"
  (let ((token (utils:generate-secure-token))
        (expires-at (local-time:timestamp+ 
                     (local-time:now) 30 :day)))
    (execute-non-query
     "INSERT INTO sessions (user_id, token, expires_at) VALUES (?, ?, ?)"
     user-id token expires-at)
    token))

(defun get-current-user (session-token)
  "Get current user from session token"
  (when session-token
    (let ((result (execute-query
                   "SELECT u.* FROM users u 
                    JOIN sessions s ON u.id = s.user_id 
                    WHERE s.token = ? AND s.expires_at > ?"
                   session-token (local-time:now))))
      (first result))))
```

## 5. Web Server Configuration

### 5.1 Hunchentoot Setup
```lisp
(defparameter *acceptor* nil)

(defun start-server (&optional (port 8080))
  "Start the web server"
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (setf *acceptor* 
        (make-instance 'hunchentoot:easy-acceptor 
                       :port port
                       :document-root #P"static/"))
  (hunchentoot:start *acceptor*)
  (setup-routes)
  (format t "Server started on port ~A~%" port))

(defun stop-server ()
  "Stop the web server"
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))
```

### 5.2 Route Definition
```lisp
(defun setup-routes ()
  "Define application routes"
  
  ;; Static files
  (hunchentoot:define-easy-handler (static-files :uri "/static/*")
      ()
    (hunchentoot:handle-static-file 
     (merge-pathnames (subseq (hunchentoot:script-name*) 8)
                      #P"static/")))
  
  ;; Home timeline
  (hunchentoot:define-easy-handler (home :uri "/") ()
    (if (current-user)
        (render-timeline)
        (hunchentoot:redirect "/login")))
  
  ;; Authentication routes
  (hunchentoot:define-easy-handler (login :uri "/login") ()
    (render-template "login.html"))
  
  (hunchentoot:define-easy-handler (request-magic-link :uri "/auth/request")
      ((email :parameter-type 'string))
    (handle-magic-link-request email))
  
  (hunchentoot:define-easy-handler (verify-magic-link :uri "/auth/verify/*")
      ()
    (handle-magic-link-verification))
  
  ;; Post routes
  (hunchentoot:define-easy-handler (create-post :uri "/posts" 
                                               :request-type :post)
      ((content :parameter-type 'string))
    (handle-create-post content))
  
  ;; User routes
  (hunchentoot:define-easy-handler (user-profile :uri "/users/*")
      ()
    (handle-user-profile)))
```

## 6. Template System

### 6.1 CL-WHO HTML Generation
```lisp
(defun render-base-template (title content)
  "Render base HTML template using CL-WHO"
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
     (:head 
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title (cl-who:str title))
      (:script :src "https://cdn.tailwindcss.com")
      (:script :src "https://unpkg.com/htmx.org@1.9.10"))
     (:body :class "bg-gray-50 min-h-screen"
      (:header :class "bg-white border-b border-gray-200 sticky top-0 z-50"
       (:div :class "max-w-2xl mx-auto px-4 py-3"
        (:h1 :class "text-xl font-bold text-gray-900" "🐦 Common Social")))
      (:main :class "max-w-2xl mx-auto"
       (cl-who:str content))))))

(defun render-timeline-posts (posts)
  "Render timeline posts using CL-WHO"
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (dolist (post posts)
     (:article :class "bg-white p-4 hover:bg-gray-50 transition-colors duration-150"
      (:div :class "flex space-x-3"
       (:div :class "flex-1 min-w-0"
        (:div :class "text-gray-900 leading-relaxed whitespace-pre-wrap break-words"
         (cl-who:str (cdr (assoc "content" post :test #'string=))))))))))
```

### 6.2 Template Function Structure
```
src/handlers.lisp:
├── render-base-template        # Base HTML structure
├── render-timeline-posts       # Post rendering logic  
├── render-timeline-page        # Complete timeline page
└── Helper functions for HTML generation
```

### 6.3 CL-WHO Advantages
- **Native Lisp**: Templates are Lisp code, enabling full language features
- **Type Safety**: Compile-time checking of HTML structure
- **Performance**: No template parsing overhead at runtime
- **Debugging**: Full Lisp debugging capabilities within templates
- **Composability**: Functions can be easily composed and reused

## 7. HTMX Integration Patterns

### 7.1 Post Creation
```html
<!-- Post composer form -->
<form hx-post="/posts" 
      hx-target="#timeline" 
      hx-swap="afterbegin"
      hx-on::after-request="this.reset()">
    <textarea name="content" 
              placeholder="What's happening?"
              class="w-full p-3 border rounded-md resize-none"
              rows="3"
              maxlength="280"></textarea>
    <div class="flex justify-between items-center mt-2">
        <span class="text-sm text-gray-500">
            <span id="char-count">280</span> characters remaining
        </span>
        <button type="submit" 
                class="btn-primary"
                disabled>
            Post
        </button>
    </div>
</form>
```

### 7.2 Timeline Updates
```html
<!-- Timeline container -->
<div id="timeline" 
     hx-get="/api/timeline" 
     hx-trigger="load, every 30s"
     hx-swap="innerHTML">
    <!-- Posts will be loaded here -->
</div>
```

### 7.3 Infinite Scroll
```html
<!-- Load more trigger -->
<div hx-get="/api/posts?offset={{ offset }}" 
     hx-trigger="intersect once" 
     hx-swap="outerHTML">
    <div class="text-center py-4">
        <button class="btn-secondary">Load More Posts</button>
    </div>
</div>
```

## 8. Security Implementation

### 8.1 CSRF Protection
```lisp
(defun generate-csrf-token ()
  "Generate CSRF token for forms"
  (utils:generate-secure-token))

(defun verify-csrf-token (provided-token session-token)
  "Verify CSRF token matches session"
  (string= provided-token 
           (gethash session-token *csrf-tokens*)))
```

### 8.2 Input Validation
```lisp
(defun validate-email (email)
  "Validate email address format"
  (and (stringp email)
       (> (length email) 3)
       (< (length email) 255)
       (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" email)))

(defun validate-post-content (content)
  "Validate post content"
  (and (stringp content)
       (> (length (string-trim " " content)) 0)
       (<= (length content) 280)))

(defun validate-username (username)
  "Validate username format"
  (and (stringp username)
       (>= (length username) 3)
       (<= (length username) 20)
       (cl-ppcre:scan "^[a-zA-Z0-9_]+$" username)))
```

### 8.3 Rate Limiting
```lisp
(defparameter *rate-limits* (make-hash-table :test 'equal))

(defun check-rate-limit (key limit-per-hour)
  "Check if action is within rate limit"
  (let* ((now (local-time:timestamp-to-unix (local-time:now)))
         (hour-start (- now (mod now 3600)))
         (rate-key (format nil "~A:~A" key hour-start))
         (current-count (gethash rate-key *rate-limits* 0)))
    (if (< current-count limit-per-hour)
        (progn
          (setf (gethash rate-key *rate-limits*) (1+ current-count))
          t)
        nil)))
```

## 9. Error Handling and Logging

### 9.1 Error Handling Strategy
```lisp
(defun handle-application-error (condition)
  "Global error handler"
  (log:error "Application error: ~A" condition)
  (if (hunchentoot:*request*)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
        (render-template "error.html" :error "Internal server error"))
      (format t "Error outside request context: ~A~%" condition)))

(setf hunchentoot:*catch-errors-p* t)
(setf hunchentoot:*show-lisp-errors-p* nil) ; In production
```

### 9.2 Logging Configuration
```lisp
(defun setup-logging ()
  "Configure application logging"
  (log:config :info :daily "logs/app.log")
  (log:config :error :daily "logs/error.log"))
```

## 10. Performance Optimizations

### 10.1 Database Query Optimization
```lisp
(defun get-timeline-posts (&optional (limit 20) (offset 0))
  "Get timeline posts with efficient query"
  (execute-query
   "SELECT p.id, p.content, p.created_at,
           u.username, u.display_name
    FROM posts p
    JOIN users u ON p.user_id = u.id
    ORDER BY p.created_at DESC
    LIMIT ? OFFSET ?"
   limit offset))
```

### 10.2 Template Caching
```lisp
(defparameter *template-cache* (make-hash-table :test 'equal))

(defun cached-render-template (template-name &rest args)
  "Render template with caching"
  (let ((cache-key (format nil "~A:~{~A~}" template-name args)))
    (or (gethash cache-key *template-cache*)
        (setf (gethash cache-key *template-cache*)
              (apply #'render-template template-name args)))))
```

## 11. Deployment Configuration

### 11.1 Production Setup
```lisp
(defun production-config ()
  "Configure application for production"
  (setf hunchentoot:*catch-errors-p* t
        hunchentoot:*show-lisp-errors-p* nil
        *template-cache-enabled* t
        *debug-mode* nil))

(defun development-config ()
  "Configure application for development"
  (setf hunchentoot:*catch-errors-p* nil
        hunchentoot:*show-lisp-errors-p* t
        *template-cache-enabled* nil
        *debug-mode* t))
```

### 11.2 Environment Variables
```lisp
(defun load-config ()
  "Load configuration from environment"
  (setf *server-port* (parse-integer (or (uiop:getenv "PORT") "8080"))
        *database-path* (or (uiop:getenv "DATABASE_PATH") "common-social.db")
        *smtp-host* (or (uiop:getenv "SMTP_HOST") "localhost")
        *base-url* (or (uiop:getenv "BASE_URL") "http://localhost:8080")))
```

## 12. Testing Strategy

### 12.1 Unit Testing with FiveAM
```lisp
(defpackage :common-social-test
  (:use :cl :fiveam :common-social))

(def-suite common-social-tests
  :description "Test suite for Common Social")

(in-suite common-social-tests)

(test test-user-validation
  "Test user validation functions"
  (is (validate-email "test@example.com"))
  (is (not (validate-email "invalid-email")))
  (is (validate-username "valid_user123"))
  (is (not (validate-username "ab"))))
```

### 12.2 Integration Testing
```lisp
(test test-magic-link-flow
  "Test complete magic link authentication flow"
  (let ((email "test@example.com"))
    (multiple-value-bind (link token)
        (generate-magic-link email)
      (is (stringp link))
      (is (stringp token))
      (is (string= email (verify-magic-link token)))
      (is (not (verify-magic-link token)))))) ; Should fail second time
```