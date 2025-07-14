(asdf:defsystem #:common-social
  :description "Twitter-like social media application MVP"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:hunchentoot
               #:sqlite
               #:cl-who
               #:alexandria
               #:local-time
               #:cl-json
               #:bordeaux-threads)
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "config")
                 (:file "utils")
                 (:file "db" :depends-on ("config"))
                 (:file "models" :depends-on ("db" "utils"))
                 (:file "hotload" :depends-on ("config"))
                 (:file "handlers" :depends-on ("models"))
                 (:file "main" :depends-on ("handlers" "hotload"))))))