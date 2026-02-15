(in-package :blog)

(defvar *posts* (make-hash-table :test 'equal)) ; slug -> post plist



(defun all-posts-list ()
  "Return all posts currently loaded in memory."
  (maybe-reload-posts :min-interval-sec 1)
  (loop for p being the hash-values of *posts* collect p))

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


;;; taxonomy (category/tags)------------------------------
(defun blank-string-p (s)
  (and (stringp s)
       (every (lambda (ch)
                (or (char= ch #\Space)
                    (char= ch #\Tab)
                    (char= ch #\Newline)
                    (char= ch #\Return)))
              s)))

(defun trim-string (s)
  (if (stringp s)
      (string-trim '(#\Space #\Tab #\Newline #\Return) s)
      s))


(defun sanitize-token (s)
  "Lowercase; keep [a-z0-9-]; map space/_ to '-'; collapse dashes; trim dashes."
  (let* ((lower (string-downcase (princ-to-string (or s ""))))
         (mapped (with-output-to-string (out)
                   (loop for ch across lower do
                         (cond
                           ((or (char= ch #\Space) (char= ch #\Tab) (char= ch #\_)
                                (char= ch #\/) (char= ch #\\))
                            (write-char #\- out))
                           ((or (and (char>= ch #\a) (char<= ch #\z))
                                (and (char>= ch #\0) (char<= ch #\9))
                                (char= ch #\-))
                            (write-char ch out))
                           (t nil)))))
         (collapsed (with-output-to-string (out)
                      (let ((prev-dash nil))
                        (loop for ch across mapped do
                              (cond
                                ((char= ch #\-)
                                 (unless prev-dash (write-char ch out))
                                 (setf prev-dash t))
                                (t
                                 (write-char ch out)
                                 (setf prev-dash nil))))))))
    (string-trim "-" collapsed)))

(defun normalize-category (cat)
  "Return a string (single category) or NIL if uncategorized."
  (cond
    ((null cat) nil)
    ((and (stringp cat)
          (or (string= (trim-string cat) "")
              (blank-string-p cat)))
     nil)
    (t (trim-string (princ-to-string cat)))))


(defun split-tags-string (s)
  "Split by comma and/or whitespace; return list of raw tokens."
  (let ((s (trim-string (or s ""))))
    (when (and s (> (length s) 0))
      (let ((buf "")
            (out '()))
        (labels ((flush ()
                   (let ((tkn (trim-string buf)))
                     (when (> (length tkn) 0)
                       (push tkn out)))
                   (setf buf "")))
          (loop for ch across s do
                (cond
                  ((or (char= ch #\,) (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline) (char= ch #\Return))
                   (flush))
                  (t (setf buf (concatenate 'string buf (string ch))))))
          (flush)
          (nreverse out))))))

(defun normalize-tags (tags)
  "Accept NIL / list / string. Return list of santized unique tokens."
  (let ((raw
          (cond
            ((null tags) '())
            ((stringp tags) (split-tags-string tags))
            ((listp tags) (mapcar (lambda (x) (princ-to-string x)) tags))
            (t '()))))
    (let ((seen (make-hash-table :test 'equal))
          (out '()))
      (dolist (x raw (nreverse out))
          (let ((tok (sanitize-token x)))
            (when (> (length tok) 0)
              (unless (gethash tok seen)
                (setf (gethash tok seen) t)
                (push tok out))))))))


(defun url-for-category (cat)
  (format nil "/category/~a" (sanitize-token cat)))

(defun url-for-tag (tag)
  (format nil "/tag/~a" (sanitize-token tag)))


(defun url-for-post (slug)
  (format nil "/post/~a" slug))



(defun valid-ut-p (x)
  (and (integerp x) (<= 0 x)))



;;; ------------------------------
;;; Search helpers
;;; ------------------------------

(defun %write-space (out)
  (when out (write-char #\Space out)))

(defun %node->text (node out)
  "Best effort extraction of visible text from the post SEXP tree.
Used for simple full-text search."
  (cond
    ((null node) nil)
    ((stringp node)
     (write-string node out)
     (%write-space out))
    ((and (consp node) (symbolp (car node)))
     (let* ((rest (cdr node))
            (has-attrs (and rest (consp (car rest)) (keywordp (caar rest))))
            (attrs (if has-attrs (car rest) nil))
            (children (if has-attrs (cdr rest) rest))
            (tag (string-downcase (symbol-name (car node)))))
       ;; include useful attributes for some nodes
       (when (and (string= tag "img") attrs)
         (let ((alt (getf attrs :alt)))
           (when (and alt (stringp alt) (> (length alt) 0))
             (write-string alt out)
             (%write-space out))))
       (dolist (c children) (%node->text c out))))
         
    
    ((consp node)
     ;; list of nodes
     (dolist (c node) (%node->text c out)))
    (t
     (write-string (princ-to-string node) out)
     (%write-space out))))

(defun post->search-text (post)
  "Return (and cache) a plain-text representation for searching."
  (or (getf post :search-text)
      (let* ((txt (with-output-to-string (out)
                    (when (getf post :title)
                      (write-string (princ-to-string (getf post :title)) out)
                      (%write-space out))
                    (when (getf post :slug)
                      (write-string (princ-to-string (getf post :slug)) out)
                      (%write-space out))
                    (when (getf post :category)
                      (write-string (princ-to-string (getf post :category)) out)
                      (%write-space out))
                    (dolist (tag (or (getf post :tags) '()))
                      (write-string (princ-to-string tag) out)
                      (%write-space out))
                    (%node->text (getf post :content) out)))
             (norm (string-downcase txt)))
        (setf (getf post :search-text-raw) txt)
        (setf (getf post :search-text) norm)
        norm)))

(defun post->search-text-raw (post)
  "Return (and cache) raw (not downcased) search text."
  (or (getf post :search-text-raw)
      (progn (post->search-text post) (getf post :search-text-raw))))

(defun %query-tokens (q)
  "Tokenize query for search. Keeps unicode; splists on whitespace.
We also keep the raw trimmed query for substring matching."
  (let ((q (trim-string (or q ""))))
    (values
     q
     (let ((buf "")
           (out '()))
       (labels ((flush ()
                  (let ((tkn (trim-string buf)))
                    (when (> (length tkn) 0)
                      (push tkn out)))
                  (setf buf "")))
         (loop for ch across q do
               (if (or (char= ch #\Space)
                       (char= ch #\Tab)
                       (char= ch #\Newline)
                       (char= ch #\Return))
                   (flush)
                   (setf buf (concatenate 'string buf (string ch)))))
         (flush)
         (nreverse out))))))





(defun search-posts (query &key (limit 50))
  (multiple-value-bind (raw tokens) (%query-tokens query)
    (let* ((scored '()))
      (when (and raw (> (length raw) 0))
        (dolist (p (all-posts-list))
          (let* ((text (post->search-text p))  ;; 确保这是字符串
                 (score 0))
            ;; 大小写不敏感
            (when (search raw text :test #'char-equal)
              (incf score 1))
            (dolist (toks tokens)
              (let ((tok (princ-to-string toks)))
                (when (and (> (length tok) 0)
                           (search tok text :test #'char-equal))
                  (incf score 1))))
            (let ((title (princ-to-string (or (getf p :title) "")))
                  (slug  (princ-to-string (or (getf p :slug) ""))))
              (when (search raw title :test #'char-equal)
                (incf score 3))
              (when (search raw slug :test #'char-equal)
                (incf score 2)))
            (when (> score 0)
              (push (cons p score) scored)))))

      ;; 注意：排序/截断必须在循环外
      (setf scored (sort scored #'> :key #'cdr))
      (if (and limit (< limit (length scored)))
          (subseq scored 0 limit)
          scored))))


(defun normalize-post (p)
  "Ensure required keys exist; migrate old level0/1 posts."
  (let* ((slug (getf p :slug))
         (title (or (getf p :title) slug))
         (content (or (getf p :content) '()))
         ;; New:
         (category (normalize-category (getf p :category)))
         (tags (normalize-tags (getf p :tags)))
         (created0 (getf p :created-at))
         (updated0 (getf p :updated-at))
         (created (if (valid-ut-p created0) created0 (now-ut)))
         (updated (cond
                    ((valid-ut-p updated0) updated0)
                    ((valid-ut-p created0) created0)
                    (t created))))
    ;; include category/tags (backward-compatible)
    (list :slug slug :title title
          :category category
          :tags tags
          :content content
          :created-at created :updated-at updated)))

(defun save-post-to-disk (post)
  (ensure-posts-dir)
  (let ((slug (getf post :slug)))
    (with-open-file (out (post-path slug)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write post :stream out :pretty t :readably t)))
  (setf (gethash (getf post :slug) *posts*) (normalize-post post))
  (setf *posts-last-mtime* (max *posts-last-mtime*
                                (or (file-write-date (post-path (getf post :slug))) 0)))
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
  (setf *posts-last-mtime* (posts-max-mtime))
  (hash-table-count *posts*))


(defun find-post (slug)
  (or (gethash slug *posts*)
      (let ((p (load-post-from-disk slug)))
        (when p (setf (gethash slug *posts*) p))
        p)))



(defun post-exists-p (slug)
  (or (gethash slug *posts*)
      (probe-file (post-path slug))))


;;; ------------------------------
;;; Reload posts when needed
;;; ------------------------------
(defvar *posts-last-mtime* 0)           ; 最近一次已加载的 posts 文件最大 mtime
(defvar *posts-last-check-ut* 0)        ; 上次检查时间（避免每个请求都扫目录）

(defun posts-max-mtime ()
  "Return max file-write-date among *.sexp under *posts-dir* (or 0)."
  (ensure-posts-dir)
  (let ((mx 0))
    (dolist (file (directory (merge-pathnames "*.sexp" *posts-dir*)) mx)
      (let ((date (or (file-write-date file) 0)))
        (when (> date mx) (setf mx date))))))

(defun maybe-reload-posts (&key (min-interval-sec 1))
  "Reload posts if disk changed. Throttle directory scan by MIN-INTERVAL-SEC."
  (let ((now (get-universal-time)))
    (when (>= (- now *posts-last-check-ut*) min-interval-sec)
      (setf *posts-last-check-ut* now)
      (let ((mx (posts-max-mtime)))
        (when (> mx *posts-last-mtime*)
          (load-all-posts)
          (setf *posts-last-mtime* mx))))))
