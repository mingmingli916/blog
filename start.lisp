(load "/home/ming/quicklisp/setup.lisp")
(ql:quickload :hunchentoot)
(load "/home/ming/cl-projects/blog/mini-blog-level2.lisp")
(mini-blog:start-blog)

(loop
      (sleep 3600))
