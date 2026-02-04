;;; run.lisp
;;; One-file entry point for blog
;;; This file is intentionaly boring and stable.
;;; You should not need to change it for years.

;;; ------------------------------------------------------------
;;; 1. Ensure Quicklisp is available
;;; ------------------------------------------------------------

(unless (find-package :ql)
  (let ((ql-init (merge-pathnames
                  "quicklisp/setup.lisp"
                  (user-homedir-pathname))))
    (when (probe-file ql-init)
      (load ql-init))))

(unless (find-package :ql)
  (error "Quicklisp not found. Please install Quicklisp first."))



;;; ------------------------------------------------------------
;;; 2. Ensure this project is visible to ASDF
;;; ------------------------------------------------------------

(let* ((this-file *load-truename*)
       (project-root (make-pathname
                      :directory (pathname-directory this-file))))
  (pushnew project-root asdf:*central-registry*
           :test #'equal))


;;; ------------------------------------------------------------
;;; 3. Load system
;;; ------------------------------------------------------------

(ql:quickload :blog)


;;; ------------------------------------------------------------
;;; 4. Read runtime configuration
;;; ------------------------------------------------------------

(defun getenv (name)
  "Return environment variable value or NIL."
  #+sbcl (sb-ext:posix-getenv name)
  #+ccl (ccl:getenv name)
  #+ecl (si:getenv name)
  #+clisp (ext:getenv name)
  #- (or sbcl cll ecl clisp)
  nil)



(defun getenv-init (name default)
  (let ((v (getenv name)))
    (if v
        (parse-integer v :junk-allowed t)
        default)))

(defparameter *port*
  (getenv-init "PORT" blog:*port*))


;;; ------------------------------------------------------------
;;; 5. Start server
;;; ------------------------------------------------------------

(format t "~&Starting blog...~%")
(format t "~&Port: ~a~%" *port*)
(format t "~&Data dir: ~a~%"
        (namestring blog:*data-dir*))

(blog:start-blog :port *port*)



;;; ------------------------------------------------------------
;;; 6. Keep process alive
;;; ------------------------------------------------------------

(format t "~&Blog is running. Press Ctrl+C to stop.~%")

(loop
  (sleep 3600))


