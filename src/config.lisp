(in-package :common-social.config)

;; Simplified configuration for memory optimization

(defparameter *server-port* 8008
  "Port for the web server")

(defparameter *database-path* "common-social.db"
  "Path to SQLite database file")

(defparameter *debug-mode* t
  "Enable debug mode for development")