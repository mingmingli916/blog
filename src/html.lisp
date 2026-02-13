;;;; src/html.lisp
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

    ;; KaTeX (pinned version via jsDelivr)
    (format out
     "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css\">~%")
    (format out
     "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js\"></script>~%")

    (format out
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
img{max-width:100%;height:auto;border-radius:10px}
.katex-block{margin:12px 0}
</style>~%")

    (write-string "</head><body>" out)
    (format out "<header><h1 style=\"margin:0;\">~a</h1>" (html-escape title))
    (when subtitle
      (format out "<div class=\"muted\" style=\"margin-top:6px;\">~a</div>"
              (html-escape subtitle)))
    (write-string "<nav style=\"margin-top:10px;\">" out)
    (write-string "<a href=\"/\">Home</a>" out)
    (write-string "<a href=\"/archive\">Archive</a>" out)
    (write-string "<a href=\"/search\">Search</a>" out)
    (write-string "<a href=\"/graph\">Graph</a>" out)
    (write-string "<a href=\"/about\">About</a>" out)



    ;; small search box
    (write-string "<form action=\"/search\" method=\"get\" style=\"display:inline-block;margin-left:10px;\">" out)
    (write-string "<input name=\"q\" placeholder=\"Search...\" style=\"padding:6px 8px;border:1px solid #ddd;border-radius:10px;\"/>" out)
    (write-string "<button type=\"submit\" style=\"padding:6px 10px;border:1px solid #ddd;border-radius:10px;background:white;\">Go</button>" out)
    (write-string "</form>" out)

    (write-string "</nav></header><hr/>" out)
    
    (write-string body-html out)

    ;; Render KaTeX placeholders
    (write-string
     "<script>
(function(){
  function renderAll(sel, displayMode){
    var els=document.querySelectorAll(sel);
    for(var i=0;i<els.length;i++){
      var el=els[i];
      var tex=el.getAttribute('data-tex')||'';
      try{
        katex.render(tex, el, {displayMode:displayMode, throwOnError:false});
      }catch(e){}
    }
  }
  function go(){
    if(!window.katex) return;
    renderAll('.katex-inline', false);
    renderAll('.katex-block', true);
  }
  if(document.readyState === 'loading'){
    document.addEventListener('DOMContentLoaded', go);
  } else {
    go();
  }
})();
</script></br>"
     out)

    (write-string "</body></html>" out)))

(defun %attrs->string (plist)
  (with-output-to-string (out)
    (loop for (k v) on plist by #'cddr do
      (when (and k v)
        ;; keyword :href -> href
        (format out " ~a=\"~a\""
                (string-downcase (subseq (symbol-name k) 1))
                (html-escape v))))))




