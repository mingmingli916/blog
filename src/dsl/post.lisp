(in-package :blog)



(defmacro defpost (slug &key title category tags content)
  "Define a post.
Slug is permanent.
If the post exists, we update title/content and bump updated-at (created-at stays)."
  `(progn
     (unless (safe-slug-p ,slug)
       (error "Bad slug: ~S" ,slug))
     (let* ((s ,slug)
            (existing (find-post s))
            (created (if existing (getf existing :created-at) (now-ut)))
            (updated (now-ut))
            (p (list :slug s
                     :title ,title
                     :category (normalize-category ,category)
                     :tags (normalize-tags ,tags)
                     :content ',content
                     :created-at created
                     :updated-at updated)))
       (setf (gethash s *posts*) p)
       (save-post-to-disk p)
       p)))

(defun touch-post (slug)
  "Bump updated-at without changing content (useful if you edit file manually)."
  (let ((p (find-post slug)))
    (unless p (error "No such post: ~A" slug))
    (setf (getf p :updated-at) (now-ut))
    (setf (gethash slug *posts*) p)
    (save-post-to-disk p)
    p))

