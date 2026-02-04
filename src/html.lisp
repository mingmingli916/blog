(in-package :blog)

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
    (format out "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
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



(defun %attrs->string (plist)
  (with-output-to-string (out)
    (loop for (k v) on plist by #'cddr do
      (when (and k v)
        (format out " ~a=\"~a\""
                (string-downcase (subseq (symbol-name k) 1)) ; :href -> href
                (html-escape v))))))


(defun render-node (node)
  "Tree renderer:
- string => escaped
- (tag ...) where ... may include optional attribute plist as second element when it's a plist starting with keyword."
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
           ((string= tag "h1")    (format nil "<h1~a>~a</h1>~%"
                                          (%attrs->string attrs) (render-children)))
           ((string= tag "h2")    (format nil "<h2~a>~a</h2>~%"
                                          (%attrs->string attrs) (render-children)))
           ((string= tag "p")     (format nil "<p~a>~a</p>~%"
                                          (%attrs->string attrs) (render-children)))

           ;; 原先 code 节点仍支持：(code "...")
           ((string= tag "code")  (format nil "<pre><code~a>~a</code></pre>~%"
                                          (%attrs->string attrs) (render-children)))

           ;; 新增：src 代码块带语言
           ((string= tag "src")
            (let* ((lang (or (getf attrs :lang) "text")))
              (format nil "<pre><code class=\"language-~a\">~a</code></pre>~%"
                      (html-escape lang)
                      (render-children))))

           ;; 新增：ul/li
           ((string= tag "ul")    (format nil "<ul~a>~a</ul>~%"
                                          (%attrs->string attrs) (render-children)))
           ((string= tag "li")    (format nil "<li~a>~a</li>~%"
                                          (%attrs->string attrs) (render-children)))

           ;; 新增：链接
           ((string= tag "a")     (format nil "<a~a>~a</a>"
                                          (%attrs->string attrs) (render-children)))

           ;; 新增：引用
           ((string= tag "quote") (format nil "<blockquote~a>~a</blockquote>~%"
                                          (%attrs->string attrs) (render-children)))

           (t (format nil "<pre>Unknown tag: ~a</pre>~%"
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
