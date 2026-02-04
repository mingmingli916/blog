;;;; mini-blog-level1.lisp
;;;; Level 1: Level 0 + homepage index list
;;;; - Write posts via (defpost ...)
;;;; - Persist to posts/<slug>.sexp
;;;; - Load posts at startup
;;;; - View single post via /post/<slug>
;;;; - Index page: /



(defpackage :mini-blog
  (:use :cl :hunchentoot)
  (:export :start-blog :stop-blog :defpost))


(in-package :mini-blog)

;;; ------------------------------
;;; Config
;;; ------------------------------

;; (defparameter *posts-dir* (merge-pathnames "posts/" (truename "./")))
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

(defun page (title body-html)
  (with-output-to-string (out)
    (format out "<!doctype html>~%<html><head><meta charset=\"utf-8\">~%")
    (format out "<meta name=\"viewport\" content=\"width=divice-width, initial-scale=1\">~%")
    (format out "<title>~a</title>~%" (html-escape title))
    ;; tiny inline style (optional)
    (write-string
     "<style>
body{max-width:720px;margin:40px auto;padding:0 16px;line-height:1.6;font-family:system-ui, -apple-system, Segoe UI, Roboto;}
a{color:inherit}
.muted{opacity:.7}
pre{background:#f6f6f6;padding:12px;overflow:auto;border-radius:10px}
.postlist li{margin:10px 0}
</style>"
     out)
    (write-string "</head><body>" out)
    (format out "<header><h1 style=\"margin-bottom:8px;\">~a</h1>" (html-escape title))
    (write-string "<div class=\"muted\"><a href=\"/\">Home</a></div></header><hr/>" out)
    (write-string body-html out)
    (write-string "</body></html>" out)))


(defun render-node (node)
  "Render one DSL node to HTML string."
  (cond
    ((stringp node)
     (html-escape node))

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


(defun render-post (post)
  (with-output-to-string (out)
    (format out "<article>")
    ;; 标题在body内也渲染一次
    (format out "<h2>~a</h2>~%" (html-escape (or (getf post :title) "Untitled")))
    (dolist (n (getf post :content))
      (write-string (render-node n) out))
    (format out "</article>")))

(defun safe-slug-p (slug)
  "Very small safety: forbid empty and slashes."
  (and (stringp slug)
       (> (length slug) 0)
       (not (find #\/ slug))
       (not (find #\\ slug))))

(defun url-for-post (slug)
  (format nil "/post/~a" slug))


;;; ------------------------------
;;; Persistence
;;; ------------------------------
(defun save-post-to-disk (slug title content-nodes)
  "Persist a post as a readable S-expression file."
  (ensure-posts-dir)
  (with-open-file (out (post-path slug)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    ;; Store as a property list to keep it dead-simple.
    (write (list :slug slug :title title :content content-nodes)
           :stream out
           :pretty t
           :readably t))
  t)

(defun load-post-from-disk (slug)
  (let ((path (post-path slug)))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (read in nil nil)))))


(defun load-all-posts ()
  "Load all *.sexp posts into memory."
  (ensure-posts-dir)
  (clrhash *posts*)
  (dolist (file (directory (merge-pathnames "*.sexp" *posts-dir*)))
    (with-open-file (in file :direction :input)
      (let ((p (read in nil nil)))
        (when (and (listp p) (getf p :slug))
          (setf (gethash (getf p :slug) *posts*) p)))))
  (hash-table-count *posts*))


;;; ------------------------------
;;; Writing DSL
;;; ------------------------------
(defmacro defpost (slug &key title content)
  "Define a post and persit it immediately.
CONTENT is a list of nodes, each node like (p \"...\") (code \"...\")"
  `(progn
     (unless (safe-slug-p ,slug)
       (error "Band slug: ~S" ,slug))
     (let* ((s ,slug)
            (ti ,title)
            (c ',content)
            (p (list :slug s :title ti :content c)))
       (setf (gethash s *posts*) p)
       (save-post-to-disk s ti c)
       p)))

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







;;; ------------------------------
;;; HTTP routes
;;; ------------------------------

(defun respond-html (html &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/html; charset=utf-8")
  html)

(defun respond-text (text &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/plain; charset=utf-8")
  text)


(define-easy-handler (index-handler :uri "/") ()
  (let ((posts (loop for p being the hash-values of *posts* collect p)))
    ;; sort by title (level 1 没有日期，所以只做一个稳定排序)
    (setf posts (sort posts #'string<
                      :key (lambda (p) (string-downcase (or (getf p :title) "")))))
    (respond-html
     (page "Mini Blog"
           (with-output-to-string (out)
             (format out "<h2>Posts</h2>")
             (if (null posts)
                 (format out "<p class=\"muted\">No posts yet.</p>")
                 (progn
                   (write-string "<ol class=\"postlist\">" out)
                   (dolist (p posts)
                     (let ((slug (getf p :slug))
                           (title (or (getf p :title) (getf p :slug))))
                       (format out "<li><a href=\"~a\">~a></a> <span class=\"muted\">(/post/~a)</span></li>~%"
                               (url-for-post slug)
                               (html-escape title)
                               (html-escape slug))))
                   (write-string "</ol>" out))))))))

(defun find-post (slug)
  (or (gethash slug *posts*)
      (load-post-from-disk slug)))

(define-easy-handler (post-query-handler :uri "/post") (slug)
  "GET /post?slug=hello-lisp (fallback)
We also support /post/<slug> via a dispatcher below."
  (if (and slug (safe-slug-p slug))
      (let ((p (find-post slug)))
        (if p
            (respond-html (page (or (getf p :title) slug) (render-post p)))
            (respond-html (page "404" (format nil "<p>Not found: ~a</p>" (html-escape slug))) 404)))
      (respond-text "Bad slug" 400)))

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
                    (respond-html (page (or (getf p :title) slug) (render-post p)))
                    (respond-html (page "404" (format nil "<p>Not found: ~a</p>" (html-escape slug))) 404)))
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
  (format t "~&Mini Blog Level 1 running on http://127.0.0.1:~a/~%" port)
  (format t "~&Example: http://127.0.0.1:~a/post/hello-lisp~%" port)
  t)

(defun stop-blog ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  t)




