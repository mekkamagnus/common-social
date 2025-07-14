# Common Lisp Development Guide

## Table of Contents
1. [Introduction to Live Coding](#introduction)
2. [SLY (Slime) Integration for Web Applications](#sly-integration)
3. [Hot Loading Methods](#hot-loading-methods)
4. [Web Application Specific Benefits](#web-application-benefits)
5. [Live Coding Workflow Examples](#workflow-examples)
6. [Advanced Techniques](#advanced-techniques)
7. [Best Practices](#best-practices)

## Introduction to Live Coding {#introduction}

One of Common Lisp's most powerful features is the ability to modify running applications without stopping them. This enables rapid development, debugging, and even production updates with zero downtime.

## Why Hot Loading Works in Common Lisp

Common Lisp is designed around the concept of a **live image** - the running Lisp system maintains all code, data, and state in memory. When you redefine functions or load new code, the system updates the live definitions immediately, allowing the running application to use the new code for subsequent operations.

This is fundamentally different from compiled languages that require restart, or interpreted languages that reload entire modules.

## SLY (Slime) Integration for Web Applications {#sly-integration}

SLY (Sylvester the Cat's Common Lisp IDE) significantly enhances hot loading and live coding capabilities, especially for web applications like Common Social.

### SLY's Core Capabilities

**Interactive REPL Integration**
- **Live Evaluation**: Send individual expressions or entire functions directly to the running Lisp image
- **Incremental Compilation**: Compile and load single functions without restarting the application
- **Image Persistence**: Keep the application state while updating code

**Advanced Debugging Features**
- **Live Inspection**: Examine and modify running objects in real-time
- **Condition System**: Handle errors interactively without crashing the application
- **Stack Unwinding**: Debug and fix issues while maintaining application state

**Code Navigation and Refactoring**
- **Cross-references**: Find all uses of functions/variables across the codebase
- **Symbol Completion**: Intelligent autocompletion for faster coding
- **Definition Jumping**: Navigate to function definitions instantly

### SLY Setup for Web Applications

**Emacs + SLY Configuration**
```elisp
;; In your .emacs or init.el
(require 'sly)
(setf sly-lisp-implementations
      '((sbcl ("ros" "run" "-Q" "--"))))
```

**Project Integration**
```lisp
;; Add to your common-social.asd
:depends-on (:hunchentoot
             :sqlite
             :cl-who
             #+dev :sly)  ; Include SLY in development
```

**Development Workflow**
```bash
# Start SLY server
ros run --load common-social.asd --eval "(ql:quickload :common-social)"

# In Emacs: M-x sly-connect
# Then evaluate: (common-social:start-server)
```

## Web Application Specific Benefits {#web-application-benefits}

### Real-Time User Session Preservation

**Live User Sessions During Updates**
```lisp
;; Users stay logged in while you update code
(defun update-user-profile-handler ()
  ;; Modify this function while users are actively browsing
  ;; Their sessions, login state, and current page remain intact
  )

;; Test with real user data without losing state
(let ((active-users (get-current-sessions)))
  (update-timeline-algorithm)
  ;; Users see changes immediately without re-login
  )
```

### Interactive HTMX Development

**Live HTMX Endpoint Modification**
```lisp
;; Modify HTMX endpoints while testing in browser
(defun /api/posts/create ()
  ;; Change validation logic live
  ;; Test different response formats instantly
  ;; Debug HTMX interactions in real-time
  )

;; Update CL-WHO templates and see immediate results
(cl-who:with-html-output-to-string (*standard-output*)
  (:div :class "post-card"  ; Modify CSS classes live
   ;; Change HTML structure without page reload
   ))
```

### Live Database Development

**Schema Evolution Without Downtime**
```lisp
;; Add new columns without losing test data
(execute-sql "ALTER TABLE posts ADD COLUMN mood TEXT")

;; Test queries interactively with real data
(mapcar #'analyze-post-sentiment (get-recent-posts 100))

;; Debug database performance issues
(time (get-user-timeline "demo-user"))  ; Test query speed live
```

**Safe Data Migration**
```lisp
;; Test migrations step-by-step
(let ((backup-data (backup-users-table)))
  (migrate-user-profiles)
  ;; If something goes wrong, rollback interactively
  (unless (validate-migration)
    (restore-from-backup backup-data)))
```

### Live A/B Testing and User Experience

**Runtime Algorithm Switching**
```lisp
;; Switch algorithms for different users
(defparameter *timeline-algorithm* :chronological)

;; While users browse, switch to engagement-based
(setf *timeline-algorithm* :engagement)
;; Users see different timelines immediately

;; Test different themes live
(setf *current-theme* "dark-emerald")
(broadcast-theme-update)  ; Push to all connected browsers
```

**Real-Time Bug Fixes**
```lisp
;; User reports: "I can't post tweets over 200 characters"
;; Debug the exact issue live:

(defun validate-post-content (content)
  ;; Add debugging while user waits
  (format t "Content length: ~A~%" (length content))
  (when (> (length content) 280)
    ;; Fix the validation logic immediately
    (error "Post too long: ~A characters" (length content))))
```

### Performance Optimization in Production

**Live Performance Monitoring**
```lisp
;; Monitor performance while users browse
(defun get-user-timeline (username)
  (time  ; Add timing to existing function
    (let ((posts (db-get-user-posts username)))
      ;; Optimize query while observing real usage
      (render-timeline posts))))

;; Cache optimization without restart
(defparameter *post-cache* (make-hash-table :test 'equal))
(defun cached-get-post (id)
  ;; Add caching layer live
  )
```

**Memory Usage Monitoring**
```lisp
;; Check memory usage while app runs
(room)  ; See memory statistics

;; Fix memory leaks without losing user sessions
(defun cleanup-expired-sessions ()
  ;; Modify garbage collection live
  )
```

### Interactive Feature Development

**Building Features While Users Browse**
```lisp
;; Build new features while users use existing ones
(defun implement-like-system ()
  ;; Users can continue posting while you add likes
  (create-likes-table)
  (add-like-handlers)
  (update-post-display-with-likes)
  ;; Feature appears for users without restart
  )
```

**HTMX Interaction Testing**
```lisp
;; Test complex interactions live
(defun test-infinite-scroll ()
  ;; Modify pagination logic
  ;; Test with real user scrolling behavior
  ;; Debug HTMX responses interactively
  )
```

### Production Debugging

**Live User Issue Resolution**
```lisp
;; User: "My posts aren't showing up"
;; Debug their specific case:

(let ((user (get-user-by-username "problematic-user")))
  (inspect user)  ; Examine user object live
  (get-user-posts (user-id user))  ; Check their posts
  ;; Fix the issue without affecting other users
  )
```

**Graceful Error Recovery**
```lisp
;; Handle errors gracefully without restart
(handler-case 
  (process-user-request request)
  (database-error (e)
    ;; Fix database connection live
    (reconnect-database)
    (retry-request request)))
```

### Common Social Project Benefits

**Complement File-based Hot Loading**
- Keep automatic theme updates from `hotload-commands.lisp`
- Add interactive development for complex debugging
- Fix syntax errors like parentheses balance in `enhanced-app.lisp` interactively

**Phase 2 Feature Validation**
```lisp
;; Test social interactions with real user behavior
(defun test-follow-system ()
  ;; Users can test following while you debug edge cases
  (let ((test-users (get-test-users)))
    (mapcar #'test-follow-interaction test-users)))

;; Debug database queries with live data
(defun optimize-timeline-query ()
  ;; Test different query approaches with real user data
  (time (get-personalized-timeline "active-user")))
```

**Enhanced Development Workflow**
```lisp
;; Current workflow: Edit file → Wait 2 seconds → See changes
;; With SLY: Think → Evaluate → See instant results

;; Example: Fixing syntax issues interactively
(defun render-posts (posts)
  ;; Test parentheses balance interactively
  ;; Fix CL-WHO syntax step by step
  ;; See HTML output immediately
  )
```

## Hot Loading Methods {#hot-loading-methods}

### 1. REPL Connection (Most Common)

Connect to the running Lisp process and evaluate new code directly:

```lisp
;; Connect to running server via REPL
;; Method 1: Start server with REPL access
(start-server)
;; Server runs, REPL remains available

;; Method 2: Connect to remote running process
;; (requires swank/slime setup)
(swank:create-server :port 4005 :dont-close t)
```

**Example: Update a handler function while server runs**
```lisp
;; Original function serving requests
(defun handle-home ()
  "Handle home timeline page"
  (render-timeline-page (get-all-posts :limit 20) (get-post-count)))

;; Update function in REPL - takes effect immediately
(defun handle-home ()
  "Handle home timeline page - now with caching!"
  (let ((cached-posts (get-cached-posts :limit 20)))
    (render-timeline-page cached-posts (get-post-count))))
```

### 2. File Reloading

Load updated files into the running system:

```lisp
;; Reload specific file
(load "src/handlers.lisp")

;; Reload entire system with force
(asdf:load-system :common-social :force t)

;; Reload only changed components
(asdf:operate 'asdf:compile-op :common-social)
```

**Example: Update template rendering**
```lisp
;; Edit src/handlers.lisp to add new features
(defun render-timeline-page (posts post-count)
  "Enhanced timeline with new features"
  (let ((timeline-content 
         (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
           ;; Add new feature: dark mode toggle
           (:div :class "theme-selector"
            (:button :onclick "toggleDarkMode()" "🌙 Dark Mode"))
           ;; Rest of existing template...
           )))
    (render-base-template "Home - Common Social" timeline-content)))

;; Reload just the handlers file
(load "src/handlers.lisp")
;; Next page request uses updated template immediately
```

### 3. Hot Swapping Individual Functions

Redefine specific functions while the server runs:

```lisp
;; Current post validation
(defun validate-post-content (content)
  "Basic validation"
  (and (stringp content)
       (> (length (string-trim " " content)) 0)
       (<= (length content) 280)))

;; Enhanced validation - evaluate in REPL
(defun validate-post-content (content)
  "Enhanced validation with profanity filter"
  (and (stringp content)
       (> (length (string-trim " " content)) 0)
       (<= (length content) 280)
       (not (contains-profanity-p content))
       (not (contains-spam-patterns-p content))))
```

### 4. Configuration and State Updates

Modify runtime configuration without restart:

```lisp
;; Update configuration
(setf *debug-mode* nil)           ; Toggle debug mode
(setf *max-posts-per-page* 50)    ; Increase pagination

;; Update database connections
(setf *db-pool-size* 20)

;; Add new route dynamically
(hunchentoot:define-easy-handler (api-stats :uri "/api/stats") ()
  (json-response `(("total_posts" . ,(get-post-count))
                   ("uptime" . ,(get-server-uptime)))))
```

### 5. Interactive Debugging

Debug running code without stopping the application:

```lisp
;; Add debugging to running function
(defun handle-create-post ()
  "Handle post creation with debugging"
  (let ((content (hunchentoot:parameter "content")))
    (format t "DEBUG: Creating post with content: ~A~%" content) ; Add logging
    (break "Post creation checkpoint")  ; Interactive breakpoint
    (handler-case
        (progn
          (create-post content)
          (render-timeline-posts (get-all-posts :limit 20)))
      (error (e)
        (format t "ERROR: ~A~%" e)  ; Enhanced error logging
        (setf (hunchentoot:return-code*) 400)
        (json-response `(("error" . ,(format nil "~A" e))))))))
```

## Live Coding Workflow Examples {#workflow-examples}

### Example 1: Adding a New Feature (Like Button)

**Step 1: Add database schema while running**
```lisp
;; Execute in REPL
(execute-non-query 
  "ALTER TABLE posts ADD COLUMN likes INTEGER DEFAULT 0")
```

**Step 2: Update model functions**
```lisp
;; Add new function
(defun like-post (post-id)
  "Increment like count for post"
  (execute-non-query 
    "UPDATE posts SET likes = likes + 1 WHERE id = ?" post-id))

;; Update existing function to include likes
(defun get-all-posts (&key (limit 20) (offset 0))
  "Get posts with like counts"
  (execute-query
   "SELECT id, content, created_at, likes FROM posts 
    ORDER BY created_at DESC 
    LIMIT ? OFFSET ?"
   limit offset))
```

**Step 3: Update templates**
```lisp
;; Enhanced post rendering with like button
(defun render-timeline-posts (posts)
  "Render timeline posts with like functionality"
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (dolist (post posts)
     (let ((id (first post))
           (content (second post))
           (created-at (third post))
           (likes (fourth post)))
       (:article :class "bg-white p-4"
        ;; Post content
        (:div :class "mb-3" (cl-who:str content))
        ;; Like button - NEW FEATURE
        (:div :class "flex items-center space-x-4"
         (:button :class "flex items-center space-x-1 text-gray-500 hover:text-red-500"
                  :hx-post (format nil "/posts/~A/like" id)
                  :hx-target "closest article"
                  :hx-swap "outerHTML"
          (:span "❤️") 
          (:span (cl-who:fmt "~A" (or likes 0))))))))))
```

**Step 4: Add route handler**
```lisp
;; Add new route without restart
(hunchentoot:define-easy-handler (like-post-route :uri "/posts/*/like") ()
  (when (eq (hunchentoot:request-method*) :post)
    (let ((post-id (parse-integer 
                    (car (last (split-sequence:split-sequence 
                               #\/ (hunchentoot:script-name*)))))))
      (like-post post-id)
      ;; Return updated post HTML
      (let ((updated-post (get-post-by-id post-id)))
        (render-single-post updated-post)))))
```

### Example 2: Performance Optimization

**Add caching layer without restart:**
```lisp
;; Add caching
(defparameter *post-cache* (make-hash-table :test 'equal))
(defparameter *cache-timeout* 300) ; 5 minutes

(defun get-cached-posts (&key (limit 20))
  "Get posts with caching"
  (let* ((cache-key (format nil "posts-~A" limit))
         (cached-result (gethash cache-key *post-cache*))
         (now (get-universal-time)))
    (if (and cached-result 
             (< (- now (first cached-result)) *cache-timeout*))
        (second cached-result)  ; Return cached data
        (let ((fresh-data (get-all-posts :limit limit)))
          (setf (gethash cache-key *post-cache*) 
                (list now fresh-data))
          fresh-data))))

;; Update handler to use cache
(defun handle-home ()
  "Handle home timeline page with caching"
  (render-timeline-page (get-cached-posts :limit 20) (get-post-count)))
```

## Advanced Techniques {#advanced-techniques}

### 1. Conditional Hot Loading

```lisp
;; Only apply changes in development
(when *debug-mode*
  (defun handle-create-post ()
    ;; Development version with extra logging
    ))

;; Feature flags
(when (feature-enabled-p :new-ui)
  (load "src/new-ui-handlers.lisp"))
```

### 2. Gradual Migration

```lisp
;; A/B testing new implementation
(defun handle-home ()
  (if (< (random 100) 50)  ; 50% of users
      (handle-home-new-version)
      (handle-home-old-version)))
```

### 3. State Migration

```lisp
;; Migrate data structures in running system
(defun migrate-user-sessions ()
  "Upgrade session format without restart"
  (maphash (lambda (key session)
             (when (old-session-format-p session)
               (setf (gethash key *sessions*) 
                     (upgrade-session-format session))))
           *sessions*))

;; Run migration
(migrate-user-sessions)
```

## Best Practices {#best-practices}

### Benefits of Live Coding

1. **Zero Downtime Deployments**: Update production servers without stopping them
2. **Rapid Development**: Test changes immediately without restart cycles
3. **Interactive Debugging**: Debug issues in running applications
4. **Gradual Rollouts**: Deploy features incrementally
5. **State Preservation**: Keep application state during updates
6. **Reduced Development Time**: No compilation/restart delays

### Development Best Practices

1. **Test in Development First**: Always test hot loading in development
2. **Backup State**: Save critical state before major updates
3. **Gradual Changes**: Make incremental updates rather than large changes
4. **Monitor Effects**: Watch for performance impacts of live changes
5. **Version Control**: Track what changes were applied live vs. in code
6. **Rollback Plan**: Know how to revert changes if needed

### SLY vs File-based Hot Loading Comparison

| Feature | File-based | SLY-based |
|---------|------------|-----------|
| **Speed** | 2 seconds | Instant |
| **Granularity** | File-level | Expression-level |
| **Debugging** | Limited | Full interactive |
| **State Preservation** | Restart required | Maintains state |
| **Error Handling** | App crashes | Interactive recovery |
| **User Sessions** | Lost on restart | Preserved |
| **Database Connections** | Reset | Maintained |

### Advantages of SLY for Web Applications

1. **Development Continuity**: Keep working while server updates
2. **Risk Mitigation**: Test changes without losing current session
3. **User Experience**: No interruption to active users
4. **Real-time Debugging**: Fix issues while users wait
5. **Performance Tuning**: Optimize with real usage data
6. **Feature Testing**: A/B test with live users
7. **Production Debugging**: Resolve issues without downtime

## Non-Blocking Hot Loading Methods

When working in development environments where you can't directly access the server's REPL, you can still hot load code without blocking your current session:

### 1. File-Based Hot Loading (Simplest)
Write code to files and load them when convenient:

```lisp
;; Create hotload-commands.lisp with new code
(hunchentoot:define-easy-handler (api-status :uri "/api/status") ()
  (setf (hunchentoot:content-type*) "application/json")
  "{\"status\": \"running\", \"updated\": \"live hotload works!\"}")

;; In server REPL (when accessible):
(load "hotload-commands.lisp")
```

### 2. File Watching with Auto-Reload
Set up automatic reloading when files change:

```lisp
;; Add to running server
(defparameter *hotload-thread* nil)
(defparameter *hotload-file* "hotload-commands.lisp")
(defparameter *last-modified* 0)

(defun check-and-reload ()
  "Check if hotload file was modified and reload it"
  (when (probe-file *hotload-file*)
    (let ((current-time (file-write-date *hotload-file*)))
      (when (> current-time *last-modified*)
        (setf *last-modified* current-time)
        (format t "🔥 Hot loading ~A~%" *hotload-file*)
        (handler-case
            (load *hotload-file*)
          (error (e)
            (format t "❌ Hotload error: ~A~%" e)))))))

(defun start-hotload-watcher ()
  "Start background thread to watch for file changes"
  (setf *hotload-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop
             (check-and-reload)
             (sleep 1)))
         :name "hotload-watcher"))
  (format t "🔥 Hotload watcher started~%"))
```

### 3. Signal-Based Hot Loading
Use Unix signals to trigger reloads:

```lisp
#+unix
(defun setup-signal-hotload ()
  "Setup SIGUSR1 handler for hotloading"
  (sb-unix::enable-interrupt 
   sb-unix:sigusr1
   (lambda (signal code scp)
     (declare (ignore signal code scp))
     (format t "🔥 Received SIGUSR1 - hotloading...~%")
     (load "hotload-commands.lisp")))
  (format t "📡 Signal hotload enabled~%"))

;; Usage: kill -USR1 <process-id>
```

### 4. Network-Based Code Injection
Send code via network sockets:

```bash
# Send Lisp code to running server
echo '(format t "Hot loaded via network!~%")' | nc localhost 4005
```

### 5. Scheduled Hot Loading
Periodic checks for updates:

```lisp
(defun setup-scheduled-hotload (&optional (interval 10))
  "Check for updates every N seconds"
  (bordeaux-threads:make-thread
   (lambda ()
     (loop
       (when (probe-file "update-flag.txt")
         (delete-file "update-flag.txt")
         (load "hotload-commands.lisp"))
       (sleep interval)))
   :name "scheduled-hotloader"))

;; Trigger update by creating: touch update-flag.txt
```

## Practical Hot Loading Examples

### Example: Adding New API Endpoint Without Restart

**Step 1: Create hotload file**
```lisp
;; hotload-commands.lisp
(hunchentoot:define-easy-handler (health-check :uri "/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  (format nil "{
    \"status\": \"healthy\",
    \"timestamp\": \"~A\",
    \"uptime\": \"~A seconds\",
    \"memory_usage\": \"~A MB\"
  }" 
    (get-universal-time)
    (- (get-universal-time) *server-start-time*)
    (/ (sb-vm::dynamic-usage) 1048576)))

(format t "✅ Health check endpoint added~%")
```

**Step 2: Hot load via any method**
```bash
# Method 1: Signal (if enabled)
kill -USR1 $(pgrep -f "app-server")

# Method 2: File trigger (if watcher enabled)
touch update-flag.txt

# Method 3: Manual load (when REPL accessible)
# (load "hotload-commands.lisp")
```

### Example: Update Existing Function Behavior

```lisp
;; Enhanced post validation with spam detection
(defun validate-post-content (content)
  "Enhanced validation with spam detection"
  (and (stringp content)
       (> (length (string-trim " " content)) 0)
       (<= (length content) 280)
       ;; New: spam detection
       (not (cl-ppcre:scan "(?i)(buy now|click here|free money)" content))
       ;; New: no excessive caps
       (< (count-if #'upper-case-p content) 
          (/ (length content) 2))))

;; Update post rendering with live indicator
(defun render-posts (posts)
  "Post rendering with hot-load indicator"
  (cl-who:with-html-output-to-string (*standard-output* nil)
    ;; Hot load indicator
    (:div :class "bg-green-100 text-green-800 text-xs px-2 py-1 rounded mb-2"
     "🔥 Live updated: Enhanced spam detection active")
    ;; Rest of posts...
    (dolist (post posts)
      ;; Existing post rendering code
      )))
```

## Hot Loading Best Practices for Development

### 1. Safe Hot Loading Patterns

```lisp
;; Always wrap risky changes in error handlers
(handler-case
    (progn
      ;; Your hot load changes here
      (defun risky-new-function () ...)
      (format t "✅ Hot load successful~%"))
  (error (e)
    (format t "❌ Hot load failed: ~A~%" e)
    ;; Optional: revert to previous version
    ))
```

### 2. Conditional Loading

```lisp
;; Only load in development
(when (and (boundp '*debug-mode*) *debug-mode*)
  (load "dev-hotload.lisp"))

;; Feature flags
(when (getenv "ENABLE_EXPERIMENTAL_FEATURES")
  (load "experimental-features.lisp"))
```

### 3. State Preservation

```lisp
;; Preserve important state during updates
(defparameter *preserved-state* nil)

(defun safe-hotload ()
  "Hot load with state preservation"
  (setf *preserved-state* (preserve-critical-state))
  (handler-case
      (load "hotload-commands.lisp")
    (error (e)
      (restore-critical-state *preserved-state*)
      (error e))))
```

## Integration with Development Tools

### SLIME/Emacs Integration
```lisp
;; Connect to running server
(slime-connect "localhost" 4005)
;; Edit code in Emacs, evaluate with C-c C-c
```

### VS Code with Alive Extension
```lisp
;; Connect VS Code to running Lisp
;; Edit and evaluate code directly
```

### Command Line REPL
```lisp
;; Connect via rlwrap
rlwrap ros run
(connect-to-running-server)
```

## Advantages of Non-Blocking Hot Loading

1. **Development Continuity**: Keep working while server updates
2. **Risk Mitigation**: Test changes without losing current session
3. **Automated Workflows**: Set up CI/CD with hot loading
4. **Multiple Environment Support**: Update dev, staging, production independently
5. **Collaborative Development**: Multiple developers can hot load simultaneously

This comprehensive hot loading approach makes Common Lisp exceptionally productive for web development, allowing true zero-downtime development and deployment workflows.