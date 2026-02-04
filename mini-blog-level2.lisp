;;;; mini-blog-level2.lisp
;;;; Level 2: stable slug + created/updated + time-sorted index + archive + about
;;;; - No cl-who dependency (pure string HTML)
;;;; - Hunchentoot only




(defpackage :mini-blog
  (:use :cl :hunchentoot)
  (:export :start-blog :stop-blog :defpost))


(in-package :mini-blog)

;;; ------------------------------
;;; Config / Storage
;;; ------------------------------

;;; 解决使用systemd时，目录位置的问题。
(defparameter *posts-dir*
  (merge-pathnames
   "posts/"
   (or *load-truename* *default-pathname-defaults*)))
(defparameter *port* 8080)
(defvar *acceptor* nil)
(defvar *posts* (make-hash-table :test 'equal)) ; slug -> post plist

(defun ensure-posts-dir ()
  (ensure-directories-exist *posts-dir*))

(defun post-path (slug)
  (merge-pathnames (format nil "~a.sexp" slug) *posts-dir*))

(defun safe-slug-p (slug)
  "Minimal safety: non-empty, no slashes/backslashes, not dotfiles."
  (and (stringp slug)
       (> (length slug) 0)
       (not (find #\/ slug))
       (not (find #\\ slug))
       (not (string= slug "."))
       (not (string= slug ".."))
       (not (and (> (length slug) 0) (char= (char slug 0) #\.)))))

(defun url-for-post (slug)
  (format nil "/post/~a" slug))


;;; ------------------------------
;;; Time helper
;;; ------------------------------

(defun now-ut ()
  "Universal time (integer seconds)."
  (get-universal-time))

(defun ut->ymdhm (ut)
  "Format universal time -> YYYY-MM-DD HH:MM (local time)."
  (if (and (integerp ut) (<= 0 ut))
      (multiple-value-bind (sec min hour day month year) (decode-universal-time ut)
        (declare (ignore sec))
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D" year month day hour min))
      "N/A"))

(defun ut->ym (ut)
  "YYYY-MM for grouping."
  (if (and (integerp ut) (<= 0 ut))
      (multiple-value-bind (sec min hour day month year) (decode-universal-time ut)
        (declare (ignore sec min hour day))
        (format nil "~4,'0D-~2,'0D" year month))
      "0000-00"))

;;; ------------------------------
;;; HTML helpers (no cl-who)
;;; ------------------------------

(defun html-escape (s)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string (or s ""))
          do (write-string
              (case ch
                (#\< "&lt;")
                (#\> "&gt;")
                (#\& "&amp;")
                (#\" "&quot;")
                (t (string ch)))
              out))))

(defun page (title body-html &key (subtitle nil))
  (with-output-to-string (out)
    (format out "<!doctype html>~%<html><head><meta charset=\"utf-8\">~%")
    (format out "<meta name=\"viewport\" content=\"width=divice-width, initial-scale=1\">~%")
    (format out "<title>~a</title>~%" (html-escape title))
    (write-string
     "<style>
body{max-width:720px;margin:40px auto;padding:0 16px;line-height:1.6;font-family:ui-sans-serif,system-ui, -apple-system, Segoe UI, Roboto;}
header{margin-bottom:18px}
nav a{margin-right:12px}
.muted{opacity:.7}
pre{background:#f6f6f6;padding:12px;overflow:auto;border-radius:10px}
code{font-family:ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,monospace}
.postlist li{margin:10px 0}
.meta{font-size:.92em; opacity:.75}
hr{border:none;border-top:1px solid #eee;margin:18px 0}
</style>"
     out)
    (write-string "</head><body>" out)
    (format out "<header><h1 style=\"margin:0;\">~a</h1>" (html-escape title))
    (when subtitle
      (format out "<div class=\"muted\" style=\"margin-top:6px;\">~a</div>" (html-escape subtitle)))
    (write-string "<nav style=\"margin-top:10px;\">" out)
    (write-string "<a href=\"/\">Home</a>" out)
    (write-string "<a href=\"/archive\">Archive</a>" out)
    (write-string "<a href=\"/about\">About</a>" out)
    (write-string "</nav></header><hr/>" out)
    (write-string body-html out)
    (write-string "</body></html>" out)))


(defun render-node (node)
  "Render one DSL node to HTML string."
  (cond
    ((stringp node) (html-escape node))

    ((and (consp node) (symbolp (car node)))
     (let* ((tag (string-upcase (symbol-name (car node))))
            (arg (second node)) ;; Level0: single arg
            (txt (html-escape arg)))
       (cond
         ((string= tag "H1")   (format nil "<h1>~a</h1>~%" txt))
         ((string= tag "H2")   (format nil "<h2>~a</h2>~%" txt))
         ((string= tag "P")    (format nil "<p>~a</p>~%" txt))
         ((string= tag "CODE") (format nil "<pre><code>~a</code></pre>~%" txt))
         (t (format nil "<pre>Unknown tag: ~a</pre>~%" (html-escape (prin1-to-string (car node))))))))

    (t
     (format nil "<pre>~a</pre>~%" (html-escape (prin1-to-string node))))))

(defun render-post-body (post)
  (with-output-to-string (out)
    (let* ((created (getf post :created-at))
           (updated (getf post :updated-at)))
      (format out "<div class=\"meta\">Created: ~a"
              (html-escape (if created (ut->ymdhm created) "N/A")))
      (when (and updated created (/= updated created))
        (format out " · Updated: ~a" (html-escape (ut->ymdhm updated))))
      (format out "</div>~%"))
    (format out "<article>~%")
    (dolist (n (getf post :content))
      (write-string (render-node n) out))
    (format out "</article>~%")))







;;; ------------------------------
;;; Persistence
;;; ------------------------------

(defun valid-ut-p (x)
  (and (integerp x) (<= 0 x)))

(defun normalize-post (p)
  "Ensure required keys exist; migrate old level0/1 posts."
  (let* ((slug (getf p :slug))
         (title (or (getf p :title) slug))
         (content (or (getf p :content) '()))
         (created0 (getf p :created-at))
         (updated0 (getf p :updated-at))
         (created (if (valid-ut-p created0) created0 (now-ut)))
         (updated (cond
                    ((valid-ut-p updated0) updated0)
                    ((valid-ut-p created0) created0)
                    (t created))))
    (list :slug slug :title title :content content
          :created-at created :updated-at updated)))

(defun save-post-to-disk (post)
  (ensure-posts-dir)
  (let ((slug (getf post :slug)))
    (with-open-file (out (post-path slug)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write post :stream out :pretty t :readably t)))
  t)



(defun load-post-from-disk (slug)
  (let ((path (post-path slug)))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (let ((p (read in nil nil)))
          (when (and (listp p) (getf p :slug))
            (normalize-post p)))))))

(defun load-all-posts ()
  (ensure-posts-dir)
  (clrhash *posts*)
  (dolist (file (directory (merge-pathnames "*.sexp" *posts-dir*)))
    (with-open-file (in file :direction :input)
      (let ((p (read in nil nil)))
        (when (and (listp p) (getf p :slug))
          (let ((np (normalize-post p)))
            (setf (gethash (getf p :slug) *posts*) np))))))
  ;; Optional: persit migrated metadata back to disk.
  (maphash (lambda (_slug post)
             (declare (ignore _slug))
             (save-post-to-disk post))
           *posts*)
  (hash-table-count *posts*))





(defun find-post (slug)
  (or (gethash slug *posts*)
      (let ((p (load-post-from-disk slug)))
        (when p (setf (gethash slug *posts*) p))
        p)))



;;; ------------------------------
;;; Writing DSL
;;; ------------------------------

(defun post-exists-p (slug)
  (or (gethash slug *posts*)
      (probe-file (post-path slug))))


(defmacro defpost (slug &key title content)
  "Define a post.
Slug is permanent.
If the post exists, we udpate title/content and bump updated-at (created-at stays)."
  `(progn
     (unless (safe-slug-p ,slug)
       (error "Band slug: ~S" ,slug))
     (let* ((s ,slug)
            (existing (find-post s))
            (created (if existing (getf existing :created-at) (now-ut)))
            (updated (now-ut))
            (p (list :slug s
                     :title ,title
                     :content ',content
                     :created-at created
                     :udpated-at updated)))
       (setf (gethash s *posts*) p)
       (save-post-to-disk p)
       p)))

(defun touch-post (slug)
  "Bump updated-at without changing content (useful if you edit file manually)."
  (let ((p (find-post slug)))
    (unless p (error "No such post: ~A" slug))
    (setf (getf p :updated-at) (now-ut))
    (setf (gethash slug *posts*) p)
    (save-post-to-disk p)
    p))


;;; Example posts
(defpost "hello-lisp"
  :title "Write a blog with Common Lisp"
  :content
  ((h1 "Write a blog with Common Lisp")
   (p "This is Level 0: wirte + persist +view.")
   (h2 "Why Lisp DSL?")
   (p "Because your content is structured data.")
   (code "(defmacro blog () 'freedom)")))


(defpost "second-post"
  :title "Second Post"
  :content
  ((p "You now have a homepage listing posts.")))


(defpost "about-this-blog"
  :title "About this blog"
  :content
  ((p "This is a minimal long-term blog system.")
   (p "Level 2 focuses on stability: slug + metadata + archive.")))





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
  (sort posts #'> :key (lambda (p)
                         (or (getf p :updated-at) 0))))

(define-easy-handler (index-handler :uri "/") ()
  (let* ((posts (sort-posts-by-updated-desc (all-posts-list))))
    (respond-html
     (page "Mini Blog"
           (with-output-to-string (out)
             (format out "<h2>Posts</h2>")
             (if (null posts)
                 (format out "<p class=\"muted\">No posts yet.</p>")
                 (progn
                   (write-string "<ol class=\"postlist\">" out)
                   (dolist (p posts)
                     (let* ((slug (getf p :slug))
                            (title (or (getf p :title) slug))
                            (updated (getf p :udpated-at)))
                       (format out
                               "<li><a href=\"~a\">~a</a><div class=\"meta\">Updated: ~a · Slug: ~a</div></li>~%"
                               (url-for-post slug)
                               (html-escape title)
                               (html-escape (ut->ymdhm updated))
                               (html-escape slug))))
                   (write-string "</ol>" out))))
           :subtitle "Level 2: stable slug + created/updated + archive"))))


(defun group-posts-by-ym (posts)
  "Return alist (\"YYYY-MM\" . (posts...)) in reverse chronological order."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (p posts)
      (push p (gethash (ut->ym (or (getf p :created-at) 0)) ht)))
    ;; convert to alist and sort keys describe
    (let ((alist '()))
      (maphash (lambda (k v)
                 (setf (gethash k ht) (sort-posts-by-updated-desc v))
                 (push (cons k (gethash k ht)) alist))
               ht)
      (sort alist #'string> :key #'car))))


(define-easy-handler (archive-handler :uri "/archive") ()
  (let* ((posts (all-posts-list))
         (groups (group-posts-by-ym posts)))
    (respond-html
     (page "Archive"
           (with-output-to-string  (out)
             (if (null groups)
                 (format out "<p class=\"muted\">No Posts yet.</p>")
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

(define-easy-handler (about-handler :uri "/about") ()
  (respond-html
   (page "About"
         (with-output-to-string (out)
           (write-string "<p>This is a minimal Common Lisp blog.</p>" out)
           (write-string "<p> Level 2 focuses on long-term stability:</p>" out)
           (write-string "<ul>" out)
           (write-string "<li>Stable Slugs (URLs never change)</li>" out)
           (write-string "<li>created-at / updated-at metadata</li>" out)
           (write-string "<li>Archive page</li>" out)
           (write-string "</ul>" out)
           (write-string "<p class=\"muted\">Data lives in ./posts/*.sexp</p>" out)))))

(defun post-not-found-page (slug)
  (page "404"
        (format nil "<p>Not found: <code>~a</code></p>" (html-escape slug))
        :subtitle "The post does not exist."))

(defun post-page (post)
  (page (or (getf post :title) (getf post :slug))
        (render-post-body post)
        :subtitle (format nil "/post/~a" (getf post :slug))))


(define-easy-handler (post-query-handler :uri "/post") (slug)
  "Fallback: /post?slug=..."
  (if (and slug (safe-slug-p slug))
      (let ((p (find-post slug)))
        (if p
            (respond-html (post-page p))
            (respond-html (post-not-found-page slug) 404)))
      (respond-text "Bad slug" 404)))

(defun post-dispatcher (request)
  "Dispatcher for /post/<slug>."
  (let* ((path (hunchentoot:script-name request)) ; no query string
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



;;; ------------------------------
;;; Start/Stop
;;; ------------------------------
(defun start-blog (&key (port *port*))
  (ensure-posts-dir)
  (load-all-posts)
  (setf *acceptor* (make-instance 'easy-acceptor :port port))
  ;; Add /post/<slug> support
  (push #'post-dispatcher *dispatch-table*)
  (start *acceptor*)
  (format t "~&Mini blog Level2 running: http://127.0.0.1:~a/~%" port)
  (format t "~&Archive: http://127.0.0.1:~a/archive~%" port)
  (format t "~&Example: http://127.0.0.1:~a/post/hello-lisp~%" port)  
  t)

(defun stop-blog ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  t)




