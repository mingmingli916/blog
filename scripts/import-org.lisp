;;;; scripts/import-org.lisp
;;;; Usage examples:
;;;;   sbcl --script scripts/import-org.lisp -- --file content/org/org-test.org
;;;;   sbcl --script scripts/import-org.lisp -- --dir content/org
;;;;   sbcl --script scripts/import-org.lisp -- --dir content/org --out posts
;;;;   sbcl --script scripts/import-org.lisp -- --dir content/org --out posts --recursive
;;;;   sbcl --script scripts/import-org.lisp -- --dir content/org --force

(require :asdf)
(require :uiop)

;; Load Quicklisp so dependencies like hunchentoot can be found
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql)
    (load ql)))
(ql:quickload :hunchentoot)


;; scripts/import-org.lisp 位于 blog/scripts/ 下
(defparameter *project-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

;; 让 ASDF 能找到 blog.asd
(pushnew *project-root* asdf:*central-registry* :test #'equal)

;; 加载你的系统（系统名以 blog.asd 里的 defsystem 为准）
(asdf:load-system :blog)

(in-package :blog)
;;;; ------------------------------------------------------------
;;;; Config (defaults)
;;;; ------------------------------------------------------------

(defparameter *default-org-dir* blog::*posts-source-dir*)
(defparameter *default-out-dir* *posts-dir*)
(defparameter *default-el-file*  (merge-pathnames "src/dsl/org-to-sexp.el" blog::*blog-root*))

;;;; ------------------------------------------------------------
;;;; Small utilities
;;;; ------------------------------------------------------------

(defun ensure-dir-path (p)
  "Ensure path is a directory pathname (ends with /)."
  (let ((pn (pathname p)))
    (if (pathname-name pn)
        (make-pathname :defaults pn :name nil :type nil)
        pn)))

(defun org->sexp-path (org-path out-dir)
  (merge-pathnames
   (make-pathname :name (pathname-name org-path) :type "sexp")
   (ensure-dir-path out-dir)))

(defun file-mtime (path)
  "Return mtime as universal-time integer or NIL if missing."
  (when (probe-file path)
    (file-write-date path)))

(defun up-to-date-p (sexp-file org-file)
  "Return true iff SEXP exists and its mtime >= ORG mtime."
  (let ((ms (file-mtime sexp-file))
        (mo (file-mtime org-file)))
    (and ms mo (>= ms mo))))

(defun list-org-files (dir &key recursive)
  (let* ((dir (ensure-dir-path dir))
         (files (directory (merge-pathnames "*.org" dir))))
    (if (not recursive)
        files
        (let ((subs (uiop:subdirectories dir)))
          (dolist (sd subs files)
            (setf files (nconc files (list-org-files sd :recursive t))))))))

;;;; ------------------------------------------------------------
;;;; Emacs invocation
;;;; ------------------------------------------------------------

(defun run-emacs-org-to-sexp (org-file sexp-file el-file)
  (ensure-directories-exist sexp-file)
  (let ((cmd (list "emacs" "--batch"
                   "-l" (namestring el-file)
                   "--eval"
                   (format nil "(blog-org-file->sexp-file ~S ~S)"
                           (namestring org-file)
                           (namestring sexp-file)))))
    (format t "~&[org->sexp] ~a -> ~a~%"
            (namestring org-file) (namestring sexp-file))
    (uiop:run-program cmd
                      :output *standard-output*
                      :error-output *error-output*
                      :ignore-error-status nil)))

(defun convert-one (org-file out-dir el-file &key force)
  (let* ((sexp-file (org->sexp-path org-file out-dir)))
    (cond
      ((and (not force)
            (probe-file sexp-file)
            (up-to-date-p sexp-file org-file))
       (format t "~&[skip] up-to-date: ~a~%" (namestring org-file))
       :skipped)
      (t
       (run-emacs-org-to-sexp org-file sexp-file el-file)
       :generated))))

(defun convert-dir (org-dir out-dir el-file &key recursive force)
  (let ((org-files (list-org-files org-dir :recursive recursive))
        (gen 0) (skip 0))
    (dolist (org org-files)
      (case (convert-one org out-dir el-file :force force)
        (:generated (incf gen))
        (:skipped   (incf skip))))
    (format t "~&[done] generated=~d skipped=~d total=~d~%"
            gen skip (+ gen skip))))

;;;; ------------------------------------------------------------
;;;; CLI
;;;; ------------------------------------------------------------

(defun usage ()
  (format t "~&Usage:
  sbcl --script scripts/import-org.lisp -- [--file PATH | --dir DIR]
       [--out OUTDIR] [--el org-to-sexp.el] [--recursive] [--force]

Skip rule:
  Skip when sexp.mtime >= org.mtime (unless --force)

Examples:
  sbcl --script scripts/import-org.lisp -- --file content/org/org-test.org
  sbcl --script scripts/import-org.lisp -- --dir content/org --out posts
  sbcl --script scripts/import-org.lisp -- --dir content/org --out posts --recursive
  sbcl --script scripts/import-org.lisp -- --dir content/org --force
~%"))

(defun parse-args (argv)
  "Return plist: :file :dir :out :el :recursive :force
Paths are resolved relative to project root if relative."
  (let ((file nil) (dir nil)
        (out *default-out-dir*)
        (el  *default-el-file*)
        (source *default-org-dir*)
        (recursive nil)
        (force nil))
    (labels ((pop1 () (prog1 (car argv) (setf argv (cdr argv)))))
      (loop while argv do
        (let ((a (pop1)))
          (cond
            ((string= a "--file") (setf file (pop1)))
            ((string= a "--dir")  (setf dir  (pop1)))
            ((string= a "--out")  (setf out  (pop1)))
            ((string= a "--el")   (setf el   (pop1)))
            ((string= a "--recursive") (setf recursive t))
            ((string= a "--force")     (setf force t))
            ((or (string= a "-h") (string= a "--help"))
             (usage) (uiop:quit 0))
            (t
             (format t "~&Unknown arg: ~a~%" a)
             (usage) (uiop:quit 2))))))
    (flet ((resolve (p &key dirp)
             (let* ((pn (merge-pathnames p *blog-root*))
                    (pn2 (if dirp (ensure-dir-path pn) pn)))
               (if (probe-file pn2) (truename pn2) pn2))))
      (list :file (and file (resolve file))
            :dir  (and dir  (resolve dir :dirp t))
            :out  (resolve out :dirp t)
            :el   (resolve el)
            :recursive recursive
            :force force))))

(defun main ()
  (let* ((argv (uiop:command-line-arguments))
         (opts (parse-args argv))
         (file (getf opts :file))
         (dir  (getf opts :dir))
         (out  (getf opts :out))
         (el   (getf opts :el))
         (recursive (getf opts :recursive))
         (force (getf opts :force)))
    (cond
      (file
       (convert-one file out el :force force))
      (dir
       (convert-dir dir out el :recursive recursive :force force))
      (t
       ;; default: convert default org dir
       (convert-dir *default-org-dir* out el :recursive recursive :force force)))))

(main)
