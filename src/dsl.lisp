

;;; Example posts 
;; (defpost "hello-lisp"
;;   :title "Write a blog with Common Lisp"
;;   :content
;;   ((h1 "Write a blog with Common Lisp")
;;    (p "This is Level 0: write + persist + view.")
;;    (h2 "Why Lisp DSL?")
;;    (p "Because your content is structured data.")
;;    (code "(defmacro blog () 'freedom)")))

;; (defpost "second-post"
;;   :title "Second Post"
;;   :content
;;   ((p "You now have a homepage listing posts.")))

;; (defpost "about-this-blog"
;;   :title "About this blog"
;;   :content
;;   ((p "This is a minimal long-term blog system.")
;;    (p "Level 2 focuses on stability: slug + metadata + archive.")))


;;; ------------------------------
;;; Sexp -> HTML
;;; ------------------------------
(defun plistp (x)
  (and (listp x)
       (evenp (length x))
       (loop for (k v) on x by #'cddr always (keywordp k))))


(defun escape-html (s)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string s) do
      (write-string
       (case ch
         (#\< "&lt;")
         (#\> "&gt;")
         (#\& "&amp;")
         (#\" "&quot;")
         (t (string ch)))
       out))))


(defun tag->name (sym)
  ;; BLOG::H1 -> "h1"
  (string-downcase (symbol-name sym)))


(defun kw->attr-name (k)
  "Convert :CLASS -> \"class\" etc."
  (string-downcase (subseq (symbol-name k) 1)))


(defun render-attrs (attrs out)
  (loop for (k v) on attrs by #'cddr do
    (format out " ~a=\"~a\""
            (string-downcase (subseq (symbol-name k) 1)) ; :CLASS -> "class"
            (escape-html v))))


(defun render-node (node out)
  (cond
    ((null node) nil)
    ((stringp node) (write-string (escape-html node) out))
    ((numberp node) (write-string (escape-html node) out))
    ((and (consp node) (symbolp (car node)))
     (let* ((tag (car node))
            (rest (cdr node))
            (attrs (when (and rest (plistp (car rest)))))
            (kids (if attrs (cdr rest) rest)))
       (format out "<~a" (tag->name tag))
       (when attrs (render-attrs attrs out))
       (write-string ">" out)
       (dolist (k kids) (render-node k out))
       (format out "</~a>" (tag->name tag))))
    (t (write-string (escape-html node) out))))

(defun render (ast)
  (with-output-to-string (out)
    (dolist (n ast) (render-node n out))))




(defun h (tag &rest xs)
  ;; (h 'BLOG::P "hi")  -> (BLOG::P "hi")
  ;; (h 'BLOG::A :href "/x" "x") -> (BLOG::A (:HREF "/x") "x")
  (labels ((kw? (x) (and (keywordp x) (not (null x)))))
    (if (and xs (kw? (first xs)))
        (let ((attrs '())
              (kids '()))
          (loop while (and xs (kw? (first xs))) do
            (let ((k (pop xs)) (v (pop xs)))
              (setf attrs (nconc attrs (list k v)))))
          (setf kids xs)
          (cons tag (cons attrs kids)))
        (cons tag xs))))

