;; Main application package
(defpackage :common-social
  (:use :cl)
  (:export #:start-server #:stop-server))

;; Configuration package
(defpackage :common-social.config
  (:use :cl)
  (:export #:*database-path* #:*server-port* #:*template-directory* #:*static-directory* #:*debug-mode*))

;; Database package
(defpackage :common-social.db
  (:use :cl :sqlite)
  (:export #:init-database #:get-connection #:execute-query #:execute-non-query #:close-connection))

;; Models package
(defpackage :common-social.models
  (:use :cl :common-social.db)
  (:export #:create-post #:get-all-posts #:get-post-count #:validate-post-content))

;; Hot loading package
(defpackage :common-social.hotload
  (:use :cl :bordeaux-threads)
  (:export #:start-hotload-watcher #:stop-hotload-watcher #:hotload-status))

;; Handlers package
(defpackage :common-social.handlers
  (:use :cl :hunchentoot :common-social.models)
  (:export #:setup-routes))

;; Utilities package
(defpackage :common-social.utils
  (:use :cl)
  (:export #:timestamp-to-relative-string #:escape-html #:string-trim-whitespace))