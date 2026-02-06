(in-package :blog)



;;; ------------------------------
;;; HTTP response helpers
;;; ------------------------------

(defun respond-html (html &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/html; charset=utf-8")
  html)

(defun respond-text (text &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/plain; charset=utf-8")
  text)

;;; ------------------------------
;;; Pages (Level 2)
;;; ------------------------------

(defun all-posts-list ()
  (loop for p being the hash-values of *posts* collect p))

(defun sort-posts-by-updated-desc (posts)
  (sort posts #'> :key (lambda (p) (or (getf p :updated-at) 0))))

(hunchentoot:define-easy-handler (index-handler :uri "/") ()
  (let* ((posts (sort-posts-by-updated-desc (all-posts-list))))
    (respond-html
     (page "Blog"
           (with-output-to-string (out)
             (format out "<h2>Posts</h2>")
             (if (null posts)
                 (format out "<p class=\"muted\">No posts yet.</p>")
                 (progn
                   (write-string "<ol class=\"postlist\">" out)
                   (dolist (p posts)
                     (let* ((slug (getf p :slug))
                            (title (or (getf p :title) slug))
                            (updated (getf p :updated-at)))
                       (format out
                               "<li><a href=\"~a\">~a</a><div class=\"meta\">Updated: ~a · Slug: ~a</div></li>~%"
                               (url-for-post slug)
                               (html-escape title)
                               (html-escape (ut->ymdhm updated))
                               (html-escape slug))))
                   (write-string "</ol>" out))))
           :subtitle "A long-term technical notebook"))))

(defun group-posts-by-ym (posts)
  "Return alist (\"YYYY-MM\" . (posts...)) in reverse chronological order."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (p posts)
      (push p (gethash (ut->ym (or (getf p :created-at) 0)) ht)))
    (let ((alist '()))
      (maphash (lambda (k v)
                 (push (cons k (sort-posts-by-updated-desc v)) alist))
               ht)
      (sort alist #'string> :key #'car))))

(hunchentoot:define-easy-handler (archive-handler :uri "/archive") ()
  (let* ((posts (all-posts-list))
         (groups (group-posts-by-ym posts)))
    (respond-html
     (page "Archive"
           (with-output-to-string (out)
             (if (null groups)
                 (format out "<p class=\"muted\">No posts yet.</p>")
                 (dolist (g groups)
                   (format out "<h2>~a</h2>~%" (html-escape (car g)))
                   (write-string "<ul class=\"postlist\">" out)
                   (dolist (p (cdr g))
                     (let* ((slug (getf p :slug))
                            (title (or (getf p :title) slug))
                            (created (getf p :created-at)))
                       (format out
                               "<li><a href=\"~a\">~a</a> <span class=\"meta\">(Created: ~a)</span></li>~%"
                               (url-for-post slug)
                               (html-escape title)
                               (html-escape (ut->ymdhm created)))))
                   (write-string "</ul>" out))))))))



(hunchentoot:define-easy-handler (about-handler :uri "/about") ()
  (respond-html
   (page "About"
         (with-output-to-string (out)
           ;; Minimal, calm, English About page
           (write-string "<p>This site — <strong>mingmingli.site</strong> — is a long-term technical notebook.</p>" out)
           (write-string "<p>It exists for one reason: to turn understanding into something durable.</p>" out)

           (write-string "<h2>Focus</h2>" out)
           (write-string "<ul>" out)
           (write-string "<li><strong>AI</strong>: behavior, limits, verification, practical workflows.</li>" out)
           (write-string "<li><strong>Linux</strong>: systems and tools designed to remain usable for years.</li>" out)
           (write-string "<li><strong>Programming</strong>: structure, abstraction, and maintainability.</li>" out)
           (write-string "</ul>" out)

           (write-string "<h2>Principles</h2>" out)
           (write-string "<ul>" out)
           (write-string "<li>Simple system, inspectable content.</li>" out)
           (write-string "<li>Stable URLs; posts are versionable files.</li>" out)
           (write-string "<li>Clear assumptions and boundaries.</li>" out)
           (write-string "</ul>" out)

           (write-string "<p class=\"muted\">Not a tutorial site. A record of thinking.</p>" out)

           ;; Keep your existing note about posts directory (now not ./posts)
           ;; (format out "<p class=\"muted\">Posts live in: <code>~a</code></p>"
           ;;         (html-escape (namestring *posts-dir*)))
           ))))




(hunchentoot:define-easy-handler (post-query-handler :uri "/post") (slug)
  "Fallback: /post?slug=..."
  (if (and slug (safe-slug-p slug))
      (let ((p (find-post slug)))
        (if p
            (respond-html (post-page p))
            (respond-html (post-not-found-page slug) 404)))
      (respond-text "Bad slug" 400)))

(defun post-dispatcher (request)
  "Dispatcher for /post/<slug>."
  (let* ((path (hunchentoot:script-name request))
         (prefix "/post/"))
    (when (and (>= (length path) (length prefix))
               (string= prefix (subseq path 0 (length prefix))))
      (let ((slug (subseq path (length prefix))))
        (lambda ()
          (if (safe-slug-p slug)
              (let ((p (find-post slug)))
                (if p
                    (respond-html (post-page p))
                    (respond-html (post-not-found-page slug) 404)))
              (respond-text "Bad slug" 400)))))))
