;;; scripts/org-to-sexp.el
;;; Org -> Common Lisp .sexp exporter for blog
;;;
;;; Output format:
;;; (:slug "..." :title "..." :content ( (h1 "...") (p "...") (src (:lang "lisp") "...") ... )
;;;  :created-at <ut-int> :updated-at <ut-int>)
;;;
;;; IMPORTANT: This exporter prints STRICT Common Lisp-readable S-expressions:
;;; only lists, strings, integers. No #<...> objects.

(require 'org)
(require 'org-element)

;; --------------------------
;; Metadata helpers
;; --------------------------

(defun blog--kw (key)
  "Get #+KEY: value in current buffer, string or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" (upcase key)) nil t)
      (string-trim (match-string 1)))))

(defun blog--now-ut ()
  "Export universal time (seconds since 1900-01-01)."
  ;; UT = unix + 2208988800
  (+ (floor (float-time)) 2208988800))

;; --------------------------
;; Common Lisp string/int printer (strict)
;; --------------------------

(defun blog--escape-cl-string (s)
  "Escape for Common Lisp string literal."
  (let ((s (or s "")))
    (setq s (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
    (setq s (replace-regexp-in-string "\"" "\\\\\"" s))
    (setq s (replace-regexp-in-string "\r" "" s))
    s))

(defun blog--cl-str (s)
  (format "\"%s\"" (blog--escape-cl-string s)))

(defun blog--cl-int (n)
  (format "%d" (truncate n)))

(defun blog--cl-print-plist (plist)
  "Print keyword plist as CL list: (:lang \"lisp\" :href \"...\")"
  (let ((parts '()))
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (push (format "%s %s"
                      (symbol-name k) ; :lang
                      (blog--cl-str (format "%s" v)))
              parts)))
    (format "(%s)" (mapconcat #'identity (nreverse parts) " "))))

;; Forward declare for mutual recursion
(declare-function blog--cl-print-node "org-to-sexp.el")
(declare-function blog--cl-print-children "org-to-sexp.el")

(defun blog--cl-print-children (lst)
  "Print children as expanded args: a b c (no outer parens)."
  (mapconcat #'blog--cl-print-node lst " "))

(defun blog--cl-print-node (node)
  "Serialize our DSL node to Common Lisp syntax string."
  (cond
   ;; leaf string
   ((stringp node) (blog--cl-str node))

   ;; (tag ...)
   ((and (consp node) (symbolp (car node)))
    (let* ((tag (symbol-name (car node)))
           (rest (cdr node)))

      ;; Optional attrs form: (tag (:k v ...) child...)
      (cond
       ((and rest (consp (car rest)) (keywordp (caar rest)))
        (let ((attrs (car rest))
              (children (cdr rest)))
          (format "(%s %s %s)"
                  tag
                  (blog--cl-print-plist attrs)
                  (blog--cl-print-children children))))

       (t
        ;; Normal: expand children directly
        (format "(%s %s)" tag (blog--cl-print-children rest))))))

   ;; fallback: stringify
   (t (blog--cl-str (format "%S" node)))))

(defun blog--cl-print-list (lst)
  "Print list as (a b c)."
  (if (null lst)
      "NIL"
    (format "(%s)" (blog--cl-print-children lst))))

;; --------------------------
;; Org AST -> DSL nodes
;; --------------------------

(defun blog--node-text (node)
  "Plain text content of NODE."
  (string-trim (org-element-interpret-data (org-element-contents node))))

(defun blog--export-headline (h)
  (let* ((level (org-element-property :level h))
         (title (or (org-element-property :raw-value h) "")))
    (cond
     ((= level 1) (list 'h1 title))
     ((= level 2) (list 'h2 title))
     (t           (list 'h2 title)))))

(defun blog--export-paragraph (p)
  (let ((txt (blog--node-text p)))
    (when (> (length txt) 0)
      (list 'p txt))))

(defun blog--export-src-block (b)
  (let* ((lang (or (org-element-property :language b) "text"))
         (value (or (org-element-property :value b) "")))
    (list 'src (list :lang lang) value)))

(defun blog--export-quote-block (b)
  (let ((txt (string-trim (or (org-element-property :value b) ""))))
    (when (> (length txt) 0)
      (list 'quote txt))))

(defun blog--export-plain-list (lst)
  (let ((items '()))
    (org-element-map (org-element-contents lst) 'item
      (lambda (it)
        (let ((txt (string-trim (org-element-interpret-data (org-element-contents it)))))
          (when (> (length txt) 0)
            (push (list 'li txt) items)))))
    ;; (ul (li ...) (li ...))
    (cons 'ul (nreverse items))))

(defun blog--export-element (el)
  "Export EL if it is a meaningful node; otherwise return nil.
Container nodes (section, etc.) return nil and are handled by recursion."
  (pcase (org-element-type el)
    ('headline    (blog--export-headline el))
    ('paragraph   (blog--export-paragraph el))
    ('src-block   (blog--export-src-block el))
    ('plain-list  (blog--export-plain-list el))
    ('quote-block (blog--export-quote-block el))
    ('section     nil)
    (_            nil)))

(defun blog--walk (node out)
  "Depth-first walk collecting exported nodes in OUT (reversed)."
  (let ((x (blog--export-element node)))
    (when x (push x out)))
  (dolist (child (org-element-contents node))
    (setq out (blog--walk child out)))
  out)

(defun blog--collect-content ()
  "Collect all exported DSL nodes in document order."
  (let* ((ast (org-element-parse-buffer))
         (out (nreverse (blog--walk ast '()))))
    (delq nil out)))

;; --------------------------
;; Main entry
;; --------------------------

(defun blog-export-file (in-org out-sexp)
  "Export IN-ORG to OUT-SEXP as Common Lisp readable S-expression."
  (with-temp-buffer
    (insert-file-contents in-org)
    (delay-mode-hooks (org-mode))

    (let* ((slug (or (blog--kw "SLUG") (file-name-base in-org)))
           (title (or (blog--kw "TITLE") slug))
           (created (blog--now-ut))
           (updated created)
           (content (blog--collect-content)))

      (with-temp-file out-sexp
        (insert (format "(:slug %s :title %s :content %s :created-at %s :updated-at %s)\n"
                        (blog--cl-str slug)
                        (blog--cl-str title)
                        (blog--cl-print-list content)
                        (blog--cl-int created)
                        (blog--cl-int updated))))
      (message "Exported %s -> %s" in-org out-sexp))))

(provide 'org-to-sexp)
