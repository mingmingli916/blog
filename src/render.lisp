;;;; src/render.lisp
(in-package :blog)


(defun %attrs->string (plist)
  (with-output-to-string (out)
    (loop for (k v) on plist by #'cddr do
      (when (and k v)
        (format out " ~a=\"~a\""
                ;; keyword like :src -> "src"
                (string-downcase (symbol-name k)) 
                (html-escape v))))))

(defun render-node (node)
  (cond
    ((stringp node) (html-escape node))

    ((and (consp node) (symbolp (car node)))
     (let* ((tag (string-downcase (symbol-name (car node))))
            (rest (cdr node))
            (has-attrs (and rest (consp (car rest)) (keywordp (caar rest))))
            (attrs (if has-attrs (car rest) nil))
            (children (if has-attrs (cdr rest) rest)))
       (flet ((render-children ()
                (with-output-to-string (o)
                  (dolist (c children)
                    (write-string (render-node c) o)))))
         (cond
           ((string= tag "h1") (format nil "<h1~a>~a</h1>~%"
                                       (%attrs->string attrs) (render-children)))
           ((string= tag "h2") (format nil "<h2~a>~a</h2>~%"
                                       (%attrs->string attrs) (render-children)))
           ((string= tag "h3") (format nil "<h3~a>~a</h3>~%"
                                       (%attrs->string attrs) (render-children)))
           ((string= tag "p")  (format nil "<p~a>~a</p>~%"
                                       (%attrs->string attrs) (render-children)))

           ;; code blocks
           ((string= tag "code")
            (format nil "<pre><code~a>~a</code></pre>~%"
                    (%attrs->string attrs) (render-children)))
           ((string= tag "src")
            (let* ((lang (or (getf attrs :lang) "text")))
              (format nil "<pre><code class=\"language-~a\">~a</code></pre>~%"
                      (html-escape lang)
                      (render-children))))

           ;; lists
           ((string= tag "ul") (format nil "<ul~a>~a</ul>~%"
                                       (%attrs->string attrs) (render-children)))
           ((string= tag "ol") (format nil "<ol~a>~a</ol>~%"
                                       (%attrs->string attrs) (render-children)))
           ((string= tag "li") (format nil "<li~a>~a</li>~%"
                                       (%attrs->string attrs) (render-children)))

           ;; image
           ((string= tag "img")
            (let ((src (getf attrs :src)))
              ;; 让 "images/xxx.png" 变成 "/images/xxx.png"，避免在 /post/... 下变相对路径
              (when (and (stringp src) (uiop:string-prefix-p "images/" src))
                (setf (getf attrs :src) (concatenate 'string "/" src)))
              (format nil "<img~a/>~%" (%attrs->string attrs))))


           ;; math (KaTeX placeholder)
           ((string= tag "math")
            (let* ((mode (or (getf attrs :mode) "inline"))
                   (tex (with-output-to-string (o)
                          (dolist (c children)
                            (etypecase c
                              (string (write-string c o))
                              (t (write-string (prin1-to-string c) o)))))))
              (if (string-equal mode "block")
                  (format nil "<div class=\"katex-block\" data-tex=\"~a\"></div>~%"
                          (html-escape tex))
                  (format nil "<span class=\"katex-inline\" data-tex=\"~a\"></span>"
                          (html-escape tex)))))

           ;; links
           ((string= tag "a")  (format nil "<a~a>~a</a>"
                                       (%attrs->string attrs) (render-children)))
           ((string= tag "quote")
            (format nil "<blockquote~a>~a</blockquote>~%"
                    (%attrs->string attrs) (render-children)))

           (t
            (format nil "<pre>Unknown tag: ~a</pre>~%"
                    (html-escape (prin1-to-string (car node)))))))))

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

(defun post-not-found-page (slug)
  (page "404"
        (format nil "<p>Not found: <code>~a</code></p>" (html-escape slug))
        :subtitle "The post does not exist."))

(defun post-page (post)
  (page (or (getf post :title) (getf post :slug))
        (render-post-body post)
        :subtitle (format nil "/post/~a" (getf post :slug))))
