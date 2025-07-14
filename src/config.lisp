(in-package :common-social.config)

(defparameter *server-port* 8008
  "Port for the web server")

(defparameter *database-path* "common-social.db"
  "Path to SQLite database file")

(defparameter *template-directory* 
  (merge-pathnames #P"templates/" 
                   (asdf:system-source-directory :common-social))
  "Directory containing legacy HTML templates (now using CL-WHO)")

(defparameter *static-directory*
  (merge-pathnames #P"static/"
                   (asdf:system-source-directory :common-social))
  "Directory containing static files")

(defparameter *debug-mode* t
  "Enable debug mode for development")