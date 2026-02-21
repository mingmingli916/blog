;;; 目前支持的节点:
;;; ------------------------------
;;; H1 H2 H3 H4 H5 H6
;;; P
;;; UL OL LI
;;; BLOCKQUOTE
;;; HR
;;; SRC
;;; MATH
;;; IMG
;;; 
;;; 行内：
;;; A
;;; STRONG
;;; EM
;;; CODE
;;; 
;;; 其余全本降级为文本

;;; org-to-sexp.el --- Org -> Blog SEXP (MVP + time) -*- lexical-binding: t; -*-

;;; org-to-sexp.el --- Org -> Blog SEXP (MVP + time) -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)

;;;; ============================================================
;;;; Helpers: strings + SEXP printer
;;;; ============================================================

(defun blog--clean-string (s)
  "Return S without any text properties (avoid #(\"...\" ...))."
  (if (stringp s) (substring-no-properties s) s))

(defun blog--blank-string-p (s)
  "True if S is nil or only whitespace/newlines."
  (or (null s)
      (and (stringp s)
           (string-match-p "\\`[[:space:]\n\r\t]*\\'" s))))

(defun blog--sexp-atom (x)
  "Return Common Lisp-readable literal for string/number/nil.
Important: strip Emacs text properties from strings."
  (cond
   ((stringp x) (prin1-to-string (blog--clean-string x)))
   ((numberp x) (number-to-string x))
   ((null x) "NIL")
   (t (prin1-to-string (format "%s" x)))))

(defun blog--sexp-list (&rest parts)
  (concat "(" (mapconcat #'identity parts " ") ")"))

(defun blog--tag (name)
  (concat "BLOG::" name))

(defun blog--attrs (plist)
  "Convert elisp plist to Common Lisp plist literal."
  (let ((xs '()))
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (push (format "%s %s" k (blog--sexp-atom v)) xs)))
    (blog--sexp-list (mapconcat #'identity (nreverse xs) " "))))

(defun blog--image-url-p (s)
  (let ((p (downcase (or s ""))))
    (or (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\|webp\\)\\'" p)
        (string-prefix-p "data:image/" p))))

(defun blog--rewrite-href (raw)
  "Rewrite custom schemes to site URLs."
  (cond
   ((string-prefix-p "post:" raw) (concat "/post/" (substring raw 5)))
   ((string-prefix-p "tag:" raw)  (concat "/tag/" (substring raw 4)))
   ((string-prefix-p "category:" raw) (concat "/category/" (substring raw 9)))
   (t raw)))

(defun blog--slug-fallback (title)
  (when title
    (let ((s (downcase (blog--clean-string title))))
      (setq s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
      (setq s (replace-regexp-in-string "^-+" "" s))
      (setq s (replace-regexp-in-string "-+$" "" s))
      s)))

;;;; ============================================================
;;;; Time handling
;;;; ============================================================

(defun blog--file-mtime (file)
  "Return FILE modification time as unix timestamp (integer)."
  (truncate
   (float-time
    (file-attribute-modification-time
     (file-attributes file)))))

(defun blog--date-to-unix (date-str)
  "Parse org #+DATE string to unix timestamp. Return nil if fails."
  (when (and date-str (not (string-empty-p (string-trim date-str))))
    (condition-case nil
        (truncate (float-time (date-to-time (blog--clean-string date-str))))
      (error nil))))

;;;; ============================================================
;;;; Metadata
;;;; ============================================================

(defun blog--kw-first (info key)
  "Return the first keyword value for KEY from org-collect-keywords result INFO.
Compatible with both (KEY . \"value\") and (KEY . (\"value\" ...)) formats."
  (let ((v (cdr (assoc key info))))
    (cond
     ((null v) nil)
     ((listp v) (car v))
     (t v))))

(defun blog--meta ()
  "Extract TITLE SLUG DATE CATEGORY TAGS from buffer."
  (let* ((info (org-collect-keywords '("TITLE" "SLUG" "DATE" "CATEGORY" "TAGS")))
         (title (blog--kw-first info "TITLE"))
         (slug  (blog--kw-first info "SLUG"))
         (date  (blog--kw-first info "DATE"))
         (cat   (blog--kw-first info "CATEGORY"))
         (tags-raw (blog--kw-first info "TAGS"))
         (tags (when (and tags-raw (not (string-empty-p (string-trim tags-raw))))
                 (mapcar #'string-trim (split-string tags-raw "[, ]+" t)))))
    (list :title (and title (string-trim (blog--clean-string title)))
          :slug (and slug (string-trim (blog--clean-string slug)))
          :date (and date (string-trim (blog--clean-string date)))
          :category (and cat (string-trim (blog--clean-string cat)))
          :tags tags)))

;;;; ============================================================
;;;; Inline emitter (MVP)
;;;; ============================================================

(defun blog--emit-inline (obj)
  "Return a list of SEXP fragments (strings). OBJ may be a string or org-element."
  (cond
   ;; Org often gives plain strings (not a 'plain-text element).

   ;; Org element objects:
   ((stringp obj)
    (let* ((s (blog--clean-string obj))
           ;; 去掉段落尾部换行
           (s2 (replace-regexp-in-string "\n\\'" "" s)))
      (if (blog--blank-string-p s2)
	  nil
	(list (blog--sexp-atom s2)))))

   (t
    (pcase (org-element-type obj)

      ('plain-text
       (let ((s (blog--clean-string (or (org-element-property :value obj) ""))))
         (if (blog--blank-string-p s)
             nil
           (list (blog--sexp-atom s)))))

      ('bold
       (list (blog--sexp-list
              (blog--tag "STRONG")
              (mapconcat #'identity
                         (blog--emit-inlines (org-element-contents obj))
                         " "))))

      ('italic
       (list (blog--sexp-list
              (blog--tag "EM")
              (mapconcat #'identity
                         (blog--emit-inlines (org-element-contents obj))
                         " "))))

      ('code
       (let ((s (blog--clean-string (or (org-element-property :value obj) ""))))
         (list (blog--sexp-list (blog--tag "CODE") (blog--sexp-atom s)))))

      ('verbatim
       (let ((s (blog--clean-string (or (org-element-property :value obj) ""))))
         (list (blog--sexp-list (blog--tag "CODE") (blog--sexp-atom s)))))

      ('link
       (let* ((type (org-element-property :type obj))
              (raw  (or (org-element-property :raw-link obj) ""))
              (path (or (org-element-property :path obj) raw))
              (desc-objs (org-element-contents obj))
              (desc (string-trim
                     (mapconcat (lambda (x) (org-element-interpret-data x))
                                desc-objs "")))
              (href (if (string= type "file") path raw))
              (href2 (blog--rewrite-href href)))
         (cond
          ;; Only file links that are images become IMG
          ((and (string= type "file") (blog--image-url-p href2))
           (list (blog--sexp-list
                  (blog--tag "IMG")
                  (blog--attrs (list :SRC href2 :ALT desc)))))
          (t
           (list (blog--sexp-list
                  (blog--tag "A")
                  (blog--attrs (list :HREF href2))
                  (blog--sexp-atom (if (string-empty-p desc) href2 desc))))))))

      ('latex-fragment
       (let ((v (blog--clean-string (or (org-element-property :value obj) ""))))
         (if (blog--blank-string-p v)
             nil
           (list (blog--sexp-list
                  (blog--tag "MATH")
                  (blog--attrs '(:MODE "inline"))
                  (blog--sexp-atom v))))))

      (_
       ;; Fallback: interpret as text
       (let ((s (blog--clean-string (org-element-interpret-data obj))))
         (if (blog--blank-string-p s)
             nil
           (list (blog--sexp-atom s)))))))))

(defun blog--emit-inlines (objs)
  (apply #'append (delq nil (mapcar #'blog--emit-inline objs))))

;;;; ============================================================
;;;; Block emitter (MVP)
;;;; ============================================================

(defun blog--emit-block (el)
  (pcase (org-element-type el)

    ;; Containers / metadata we don't want in CONTENT:
    ('org-data (blog--emit-blocks (org-element-contents el)))
    ('section  (blog--emit-blocks (org-element-contents el)))
    ('keyword  nil)
    ('planning nil)

    ;; Headings
    ('headline
     (let* ((level (org-element-property :level el))
            (title (or (org-element-property :raw-value el) ""))
            (h (format "H%d" (min 6 (max 1 level))))
            (kids (blog--emit-blocks (org-element-contents el))))
       (append
        (list (blog--sexp-list (blog--tag h) (blog--sexp-atom title)))
        kids)))

    ;; Paragraph
    ('paragraph
     (let ((inl (blog--emit-inlines (org-element-contents el))))
       ;; If paragraph is empty/whitespace, drop it
       (when inl
         (list (blog--sexp-list (blog--tag "P")
                                (mapconcat #'identity inl " "))))))

    ;; Code block
    ('src-block
     (let ((lang (or (org-element-property :language el) "text"))
           (code (or (org-element-property :value el) "")))
       (list (blog--sexp-list
              (blog--tag "SRC")
              (blog--attrs (list :LANG lang))
              (blog--sexp-atom code)))))

    ;; Quote block
    ('quote-block
     (let ((kids (blog--emit-blocks (org-element-contents el))))
       (when kids
         (list (blog--sexp-list (blog--tag "BLOCKQUOTE")
                                (mapconcat #'identity kids " "))))))

    ;; Lists
    ('plain-list
     (let* ((type (org-element-property :type el))
            (tag (if (eq type 'ordered) "OL" "UL"))
            (items
             (org-element-map el 'item
               (lambda (it)
                 (let* ((kids (blog--emit-blocks (org-element-contents it))))
                   ;; LI should not end up empty
                   (when kids
                     (blog--sexp-list (blog--tag "LI")
                                      (mapconcat #'identity kids " ")))))
               nil nil 'item)))
       (setq items (delq nil items))
       (when items
         (list (blog--sexp-list (blog--tag tag)
                                (mapconcat #'identity items " "))))))

    ;; Horizontal rule
    ('horizontal-rule
     (list (blog--sexp-list (blog--tag "HR"))))

    ;; Block math (if org parses it that way)
    ('latex-environment
     (let ((v (or (org-element-property :value el) "")))
       (list (blog--sexp-list
              (blog--tag "MATH")
              (blog--attrs '(:MODE "block"))
              (blog--sexp-atom v)))))

    ;; Fallback: keep readable text
    (_
     (let ((txt (string-trim (blog--clean-string (org-element-interpret-data el)))))
       (if (string-empty-p txt)
           nil
         (list (blog--sexp-list (blog--tag "P") (blog--sexp-atom txt))))))))

(defun blog--emit-blocks (elements)
  (apply #'append (delq nil (mapcar #'blog--emit-block elements))))

;;;; ============================================================
;;;; Public API
;;;; ============================================================

(defun blog-org-buffer->sexp ()
  "Convert current Org buffer to top-level plist sexp string."
  (let* ((meta (blog--meta))
         (title (plist-get meta :title))
         (slug (or (plist-get meta :slug)
                   (blog--slug-fallback title)
                   ""))

         ;; Time
         (date-str (plist-get meta :date))
         (file (or (buffer-file-name)
                   (error "buffer-file-name is nil; cannot compute mtime for CREATED/UPDATED")))
         (mtime (blog--file-mtime file))
         (created (or (blog--date-to-unix date-str) mtime))
         (updated mtime)

         (cat (or (plist-get meta :category) ""))
         (tags (or (plist-get meta :tags) '()))
         (ast (org-element-parse-buffer))
         (content (blog--emit-blocks (org-element-contents ast))))

    (blog--sexp-list
     (format ":SLUG %s" (blog--sexp-atom slug))
     (format ":TITLE %s" (blog--sexp-atom (or title "")))
     (format ":CREATED-AT %s" (blog--sexp-atom created))
     (format ":UPDATED-AT %s" (blog--sexp-atom updated))
     (format ":CATEGORY %s" (blog--sexp-atom cat))
     (format ":TAGS %s"
             (blog--sexp-list (mapconcat #'blog--sexp-atom tags " ")))
     (format ":CONTENT %s"
             (blog--sexp-list (mapconcat #'identity content " "))))))

(defun blog-org-file->sexp-file (org-file out-sexp-file)
  "Convert ORG-FILE to SEXP and write to OUT-SEXP-FILE."
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-mode)
    ;; Ensure time logic can read buffer-file-name (mtime)
    (set-visited-file-name org-file)
    (let ((s (blog-org-buffer->sexp)))
      (with-temp-file out-sexp-file
        (insert s)))))

(provide 'org-to-sexp)
;;; org-to-sexp.el ends here
