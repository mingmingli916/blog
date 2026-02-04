;;;; mini-blog-level0.lisp
;;;; Level 0: write + persist + view single post
;;;; - Write posts via (defpost ...)
;;;; - Persist to posts/<slug>.sexp
;;;; - View via /post/<slug>


(ql:quickload '(:hunchentoot))

(defpackage :mini-blog
  (:use :cl :hunchentoot)
  (:export :start-blog :stop-blog :defpost))


(in-package :mini-blog)

;;; ------------------------------
;;; Config
;;; ------------------------------

(defparameter *posts-dir* (merge-pathnames "posts/" (truename "./")))

(defparameter *port* 8000)

(defvar *acceptor* nil)
(defvar *posts* (make-hash-table :test 'equal)) ; slug -> post plist

(defun ensure-posts-dir ()
  (ensure-directories-exist *posts-dir*))

(defun post-path (slug)
  (merge-pathnames (format nil "~a.sexp" slug) *posts-dir*))


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



;;; ------------------------------
;;; Rendering
;;; ------------------------------
;; (defun render-node (node)
;;   "Render one node into HTML using CL-WHO."
;;   (cond
;;     ((stringp node) (str node))
;;     ((and (consp node) (symbolp (car node)))
;;      (let ((tag (car node))
;;            (args (cdr node)))
;;        ;; Minimal tag set. Add more when you need.
;;        (ecase tag
;;          (h1 (htm (:h1 (str (first args)))))
;;          (h2 (htm (:h2 (str (first args)))))
;;          (p (htm (:p (str (first args)))))
;;          (code (htm (:pre (:code (str (first args)))))))))
;;     (t (htm (:pre (str (prin1-to-string node)))))))


;; (defun render-node (node)
;;   "Render one node into HTML using CL-WHO.
;; Match tags by symbol-name so tags can come from any package."
;;   (cond
;;     ((stringp node) (str node))
;;     ((and (consp node) (symbolp (car node)))
;;      (let* ((tag-sym (car node))
;;             (tag (string-upcase (symbol-name tag-sym)))
;;             (args (cdr node)))
;;        (cond
;;          ((string= tag "H1")   (htm (:h1 (str (first args)))))
;;          ((string= tag "H2")   (htm (:h2 (str (first args)))))
;;          ((string= tag "P")    (htm (:p  (str (first args)))))
;;          ((string= tag "CODE") (htm (:pre (:code (str (first args))))))

;;          (t (htm (:pre (str (format nil "Unknown tag: ~A" tag-sym))))))))
;;     (t (htm (:pre (str (prin1-to-string node)))))))


;; (defun render-post-page (post)
;;   (with-html-output-to-string (out nil :prologue t :indent t)
;;     (htm
;;      (:html
;;       (:head
;;        (:meta :charset "utf-8")
;;        (:title (str (or (getf post :title) "Untitled"))))
;;       (:body
;;        (dolist (n (getf post :content))
;;          (render-node n)))))))

;; (defun render-node (node)
;;   "Render one node into HTML using CL-WHO.
;; Match tags by symbol-name so tags can come from any package.
;; Avoid CL-WHO:STR; use ESC/FMT instead."
;;   (cond
;;     ((stringp node)
;;      (cl-who:esc node))

;;     ((and (consp node) (symbolp (car node)))
;;      (let* ((tag-sym (car node))
;;             (tag (string-upcase (symbol-name tag-sym)))
;;             (arg (first (cdr node))))
;;        (cond
;;          ((string= tag "H1")   (htm (:h1 (cl-who:esc (or arg "")))))
;;          ((string= tag "H2")   (htm (:h2 (cl-who:esc (or arg "")))))
;;          ((string= tag "P")    (htm (:p  (cl-who:esc (or arg "")))))
;;          ((string= tag "CODE") (htm (:pre (:code (cl-who:esc (or arg ""))))))

;;          (t (htm (:pre (cl-who:fmt "Unknown tag: ~A" tag-sym)))))))

;;     (t
;;      (htm (:pre (cl-who:esc (prin1-to-string node)))))))

;; (defun render-post-page (post)
;;   (with-html-output-to-string (out nil :prologue t :indent t)
;;     (htm
;;       (:html
;;        (:head
;;         (:meta :charset "utf-8")
;;         (:title (cl-who:esc (or (getf post :title) "Untitled"))))
;;        (:body
;;         (dolist (n (getf post :content))
;;           (render-node n)))))))


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

(defun render-post-page (post)
  (with-output-to-string (out)
    (format out "<!doctype html>~%<html><head><meta charset=\"utf-8\">~%")
    (format out "<title>~a</title></head><body>~%"
            (html-escape (or (getf post :title) "Untitled")))
    (dolist (n (getf post :content))
      (write-string (render-node n) out))
    (write-string "</body></html>" out)))


;;; ------------------------------
;;; HTTP routes
;;; ------------------------------
(define-easy-handler (post-handler :uri "/post") (slug)
  "GET /post?slug=hello-lisp (fallback)
We also support /post/<slug> via a dispatcher below."
  (let* ((p (or (gethash slug *posts*)
                (load-post-from-disk slug))))
    (if p
        (progn
          (setf (content-type*) "text/html; charset=utf-8")
          (render-post-page p))
        (progn
          (setf (return-code*) 404
                (content-type*) "text/plain; charset=utf-8")
          (format nil "Post not found: ~a" slug)))))

(defun post-dispatcher (request)
  "Dispatcher for /post/<slug>."
  (let* ((path (hunchentoot:request-uri request)) ; e.g. /post/hello-lisp
         (prefix "/post/"))
    (when (and (>= (length path) (length prefix))
               (string= prefix (subseq path 0 (length prefix))))
      (let ((slug (subseq path (length prefix))))
        (lambda ()
          (post-handler :slug slug))))))



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
  (format t "~&Mini Blog Level 0 running on http://127.0.0.1:~a/post/hello-lisp~%" port)
  t)

(defun stop-blog ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  t)
