(in-package :blog)

(defmacro defpost (slug &key title content)
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

;;; Example posts 
(defpost "hello-lisp"
  :title "Write a blog with Common Lisp"
  :content
  ((h1 "Write a blog with Common Lisp")
   (p "This is Level 0: write + persist + view.")
   (h2 "Why Lisp DSL?")
   (p "Because your content is structured data.")
   (code "(defmacro blog () 'freedom)")))

(defpost "second-post"
  :title "Second Post"
  :content
  ((p "You now have a homepage listing posts.")))

(defpost "about-this-blog"
  :title "About this blog"
  :content
  ((p "This is a minimal long-term blog system.")
   (p "Level 2 focuses on stability: slug + metadata + archive.")))
