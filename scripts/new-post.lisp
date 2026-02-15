;;; scripts/new-post.lisp
;;; Create a new Org post template under ./content/org/
;;; Useage
;;;   sbcl --script scripts/new-post.lisp <slug> [title]

(defun exit-ok ()
  #+sbcl (sb-ext:quit :unix-status 0)
  #-sbcl (throw 'exit 0))

(defun exit-fail ()
  #+sbcl (sb-ext:quit :unix-status 1)
  #-sbcl (throw 'exit 1))

(defun die (fmt &rest args)
  (format *error-output* "~&ERROR: ~?~%" fmt args)
  (finish-output *error-output*)
  (exit-fail))

(defun info (fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output))





(defun argv ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl nil)


(defun sanitize-slug (s)
  (labels ((ok (ch)
             (or (and (char>= ch #\a) (char<= ch #\z))
                 (and (char>= ch #\0) (char<= ch #\9))
                 (char= ch #\-))))
    (let* ((lower (string-downcase (princ-to-string s)))
           (mapped (with-output-to-string (out)
                     (loop for ch across lower do
                           (cond
                             ((or (char= ch #\Space) (char= ch #\_) (char= ch #\/) (char= ch #\\))
                              (write-char #\- out))
                             ((ok ch) (write-char ch out))
                             (t)))))
           (collapsed (with-output-to-string (out)
                        (let ((prev-dash nil))
                          (loop for ch across mapped do
                                (cond
                                  ((char= ch #\-)
                                   (unless prev-dash (write-char ch out))
                                   (setf prev-dash t))
                                  (t
                                   (write-char ch out)
                                   (setf prev-dash nil))))))))
      (string-trim "-" collapsed))))


(defun iso-date ()
  (multiple-value-bind (_sec _min _hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore _sec _min _hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))


(defun project-root ()
  (make-pathname :directory (pathname-directory *load-truename*)))


(defun ensure-dir (path)
  (ensure-directories-exist path))


(defun main ()
  (let ((args (argv)))
    (when (or (null args) (< (length args) 1))
      (info "Usage: sbcl --script scritps/new-post.lisp <slug> [title]")
      (exit-ok))

    (let* ((slug (sanitize-slug (first args)))
           (title (first args))
           (root (make-pathname :directory (butlast (pathname-directory (project-root)) 1)))
           (org-dir (ensure-dir (merge-pathnames "content/org/" root)))
           (path (merge-pathnames (format nil "~a.org" slug) org-dir)))
      (when (or (null slug) (<= (length slug) 0))
        (die "Slug becomes empty after sanitization."))

      (when (probe-file path)
        (die "Already exists: ~A" (namestring path)))

      (with-open-file (out path :direction :output :if-does-not-exist :create)
        (format out "#+TITLE: ~a~%" title)
        (format out "#+SLUG: ~a~%" slug)
        (format out "#+DATE: ~a~%" (iso-date))
        (format out "#+CATEGORY: ~a~%" "")
        (format out "#+TAGS: ~a~%" "")
        (format out "#+OPTIONS: toc:nil num:nil~%~%"))

      (info "Created org: ~A" (namestring path))
      (info "Next: edit it, then compile:")
      (info "  sbcl --script scripts/import-org.lisp")
      (exit-ok))))

(main)

