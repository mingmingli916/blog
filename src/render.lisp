
(defun respond-html (html &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/html; charset=utf-8")
  html)

(defun respond-text (text &optional (code 200))
  (setf (return-code*) code
        (content-type*) "text/plain; charset=utf-8")
  text)




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




(defun post-not-found-page (slug)
  (page "404"
        (format nil "<p>Not found: <code>~a</code></p>" (html-escape slug))
        :subtitle "The post does not exist."))

(defun post-page (post)
  (page (or (getf post :title) (getf post :slug))
        (render-post-body post)
        :subtitle (format nil "/post/~a" (getf post :slug))))




