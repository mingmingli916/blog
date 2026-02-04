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



(defun url-for-post (slug)
  (format nil "/post/~a" slug))



(defun valid-ut-p (x)
  (and (integerp x) (<= 0 x)))

(defun normalize-post (p)
  "Ensure required keys exist; migrate old level0/1 posts."
  (let* ((slug (getf p :slug))
         (title (or (getf p :title) slug))
         (content (or (getf p :content) '()))
         (created0 (getf p :created-at))
         (updated0 (getf p :updated-at))
         (created (if (valid-ut-p created0) created0 (now-ut)))
         (updated (cond
                    ((valid-ut-p updated0) updated0)
                    ((valid-ut-p created0) created0)
                    (t created))))
    (list :slug slug :title title :content content
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

