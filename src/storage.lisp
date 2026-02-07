(in-package :blog)

(defvar *posts* (make-hash-table :test 'equal)) ; slug -> post plist

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
  (hash-table-count *posts*))


(defun find-post (slug)
  (or (gethash slug *posts*)
      (let ((p (load-post-from-disk slug)))
        (when p (setf (gethash slug *posts*) p))
        p)))



(defun post-exists-p (slug)
  (or (gethash slug *posts*)
      (probe-file (post-path slug))))

