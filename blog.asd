(asdf:defsystem "blog"
  :description "Long-lived blog in Common Lisp"
  :author "Mingming Li"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("hunchentoot")
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "packages")
     (:file "config")
     (:file "time")
     (:file "html")
     (:file "storage")
     (:file "dsl")
     (:file "render")
     (:file "routes")                   ;
     (:file "server")                   ; start/stop hunchentoot server
     (:file "org-mini")))               ; org -> sexp
   ))
