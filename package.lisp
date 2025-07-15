;; Main application package
(defpackage :common-social
  (:use :cl)
  (:export #:start-server #:stop-server #:restart-server))

;; Configuration package
(defpackage :common-social.config
  (:use :cl)
  (:export #:*database-path* #:*server-port* #:*debug-mode*))

;; Database package
(defpackage :common-social.db
  (:use :cl :sqlite)
  (:export #:init-database #:get-connection #:execute-query #:execute-non-query #:close-connection))

;; Models package
(defpackage :common-social.models
  (:use :cl :common-social.db)
  (:export #:create-post #:get-all-posts #:get-post-count #:validate-post-content))

;; Handlers package
(defpackage :common-social.handlers
  (:use :cl :hunchentoot :common-social.models)
  (:export #:setup-routes))