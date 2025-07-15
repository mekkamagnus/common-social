(asdf:defsystem #:common-social
  :description "Twitter-like social media application MVP - Memory optimized single-file version"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:hunchentoot
               #:sqlite
               #:cl-who)
  :components ((:file "run"))
  :entry-point "common-social:main")