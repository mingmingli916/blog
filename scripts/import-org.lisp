;;;; scripts/import-org.lisp
;;;; Compile Org posts under ./content/org/ -> .sexp under blog:*posts-dir*
;;;; Uses Emacs in batch mode with scripts/org-to-sexp.el
;;;;
;;;; Usage:
;;;;   sbcl --script scripts/import-org.lisp            # all (incremental)
;;;;   sbcl --script scripts/import-org.lisp <slug>     # one
;;;; No static references to BLOG package symbols (avoids reader errors).

(defun exit-ok ()   (sb-ext:quit :unix-status 0))
(defun exit-fail () (sb-ext:quit :unix-status 1))

(defun die (fmt &rest args)
  (format *error-output* "~&ERROR: ~?~%" fmt args)
  (finish-output *error-output*)
  (exit-fail))

(defun info (fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output))

(defun getenv (name) (sb-ext:posix-getenv name))
(defun argv () (cdr sb-ext:*posix-argv*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require "asdf"))
  (ignore-errors (require "uiop")))

(defun maybe-load-quicklisp-setup ()
  (let ((ql-init (merge-pathnames "quicklisp/setup.lisp"
                                 (user-homedir-pathname))))
    (when (probe-file ql-init)
      (load ql-init))))

(maybe-load-quicklisp-setup)

(defun project-root ()
  (make-pathname :directory (butlast (pathname-directory *load-truename*) 1)))

(defun newest-p (src dst)
  (let ((dstp (probe-file dst)))
    (or (null dstp)
        (let ((s (file-write-date src))
              (d (file-write-date dstp)))
          (and s d (> s d))))))

(defun call-emacs-export (org-file out-file exporter-el)
  (let* ((emacs (or (getenv "EMACS") "emacs"))
         (cmd "/bin/sh")
         (expr (format nil "~a --batch -Q -l ~s --eval ~s"
                       emacs
                       (namestring exporter-el)
                       (format nil "(blog-export-file ~S ~S)"
                               (namestring org-file)
                               (namestring out-file))))
         (args (list "-lc" expr)))
    (let ((proc (sb-ext:run-program cmd args
                                    :output *standard-output*
                                    :error *error-output*
                                    :search t)))
      (let ((code (sb-ext:process-exit-code proc)))
        (unless (eql code 0)
          (die "Emacs export failed (exit ~a) for ~a" code (namestring org-file)))))))

(defun maybe-export-one (slug org-dir posts-dir exporter-el)
  (let* ((org (merge-pathnames (format nil "~a.org" slug) org-dir))
         (out (merge-pathnames (format nil "~a.sexp" slug) posts-dir)))
    (unless (probe-file org)
      (die "Org not found: ~A" (namestring org)))
    (when (newest-p org out)
      (info "Exporting: ~A -> ~A" (namestring org) (namestring out))
      (ensure-directories-exist out)
      (call-emacs-export org out exporter-el))
    t))

(defun symfun (pkg name)
  (let ((s (find-symbol name pkg)))
    (unless s (die "Missing function ~A::~A" pkg name))
    (symbol-function s)))

(defun symval (pkg name)
  (let ((s (find-symbol name pkg)))
    (unless s (die "Missing var ~A::~A" pkg name))
    (symbol-value s)))

(defun main ()
  (let* ((root (project-root))
         (org-dir (merge-pathnames "content/org/" root))
         (exporter-el (merge-pathnames "scripts/org-to-sexp.el" root)))

    (pushnew root asdf:*central-registry* :test #'equal)

    ;; Load blog system (defines package BLOG)
    (asdf:load-system "blog")

    ;; Dynamically access BLOG internals without reader needing BLOG package at read-time
    (funcall (symfun "BLOG" "ENSURE-POSTS-DIR"))
    (let* ((posts-dir (symval "BLOG" "*POSTS-DIR*"))
           (args (argv)))
      (info "Org dir:   ~A" (namestring org-dir))
      (info "Posts dir: ~A" (namestring posts-dir))

      (unless (probe-file exporter-el)
        (die "Missing exporter: ~A" (namestring exporter-el)))
      (unless (probe-file org-dir)
        (die "Missing org dir: ~A" (namestring org-dir)))

      (if (and args (>= (length args) 1))
          (maybe-export-one (first args) org-dir posts-dir exporter-el)
          (dolist (org (directory (merge-pathnames "*.org" org-dir)))
            (maybe-export-one (pathname-name org) org-dir posts-dir exporter-el)))

      (info "Done.")
      (exit-ok))))

(main)
