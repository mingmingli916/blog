;;;; scripts/import-org.lisp
;;;; Compile Org posts under ./content/org/ -> .sexp under blog:*posts-dir*
;;;; Pure Common Lisp (no Emacs). Parses a tiny Org subset.
;;;;
;;;; Usage:
;;;;   sbcl --script scripts/import-org.lisp            # all (incremental)
;;;;   sbcl --script scripts/import-org.lisp <slug>     # one

;; --- Quicklisp bootstrap (put at the very top of your script) ---
(let* ((home (user-homedir-pathname))
       (ql-init (merge-pathnames "quicklisp/setup.lisp" home)))
  (cond
    ((probe-file ql-init)
     (load ql-init)
     ;; optional: show where quicklisp was loaded from
     (format t "~&[import-org] Loaded Quicklisp: ~A~%" (namestring ql-init)))
    (t
     (format *error-output* "~&[import-org] Quicklisp not found at: ~A~%"
             (namestring ql-init)))))


(defun exit-ok ()   (sb-ext:quit :unix-status 0))
(defun exit-fail () (sb-ext:quit :unix-status 1))

(defun die (fmt &rest args)
  (format *error-output* "~&ERROR: ~?~%" fmt args)
  (finish-output *error-output*)
  (exit-fail))

(defun info (fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output))

(defun argv () (cdr sb-ext:*posix-argv*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require "asdf"))
  (ignore-errors (require "uiop")))

(defun project-root ()
  (make-pathname :directory (butlast (pathname-directory *load-truename*) 1)))

(defun newest-p (src dst)
  (let ((dstp (probe-file dst)))
    (or (null dstp)
        (let ((s (file-write-date src))
              (d (file-write-date dstp)))
          (and s d (> s d))))))

(defun symfun (pkg name)
  (let ((s (find-symbol name pkg)))
    (unless s (die "Missing function ~A::~A" pkg name))
    (symbol-function s)))

(defun symval (pkg name)
  (let ((s (find-symbol name pkg)))
    (unless s (die "Missing var ~A::~A" pkg name))
    (symbol-value s)))

(defun maybe-export-one (slug org-dir posts-dir export-fn)
  (let* ((org (merge-pathnames (format nil "~a.org" slug) org-dir))
         (out (merge-pathnames (format nil "~a.sexp" slug) posts-dir)))
    (unless (probe-file org)
      (die "Org not found: ~A" (namestring org)))
    (when (newest-p org out)
      (info "Exporting: ~A -> ~A" (namestring org) (namestring out))
      (ensure-directories-exist out)
      (funcall export-fn org out))
    t))

(defun main ()
  (let* ((root (project-root))
         (org-dir (merge-pathnames "content/org/" root)))

    (pushnew root asdf:*central-registry* :test #'equal)
    (asdf:load-system "blog")

    (funcall (symfun "BLOG" "ENSURE-POSTS-DIR"))
    (let* ((posts-dir (symval "BLOG" "*POSTS-DIR*"))
           (export-fn (symfun "BLOG" "EXPORT-ORG-FILE"))
           (args (argv)))
      (info "Org dir:   ~A" (namestring org-dir))
      (info "Posts dir: ~A" (namestring posts-dir))

      (unless (probe-file org-dir)
        (die "Missing org dir: ~A" (namestring org-dir)))

      (if (and args (>= (length args) 1))
          (maybe-export-one (first args) org-dir posts-dir export-fn)
          (dolist (org (directory (merge-pathnames "*.org" org-dir)))
            (maybe-export-one (pathname-name org) org-dir posts-dir export-fn)))

      (info "Done.")
      (exit-ok))))

(main)
