(in-package :blog)


(defvar *acceptor* nil)
(defvar *post-dispatcher-installed* nil)

(defun start-blog (&key (port *port*))
  (ensure-posts-dir)
  (load-all-posts)
  (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))

  ;; Add /post/<slug> support (avoid pushing twice across restarts)
  (unless *post-dispatcher-installed*
    (push #'post-dispatcher hunchentoot:*dispatch-table*)
    (setf *post-dispatcher-installed* t))

  (let* ((root (asdf:system-source-directory "blog"))
         (img-dir (merge-pathnames "content/org/images/" root)))
    (when (probe-file img-dir)
      (push (hunchentoot:create-folder-dispatcher-and-handler "/images/" img-dir)
            hunchentoot:*dispatch-table*)))

  
  (hunchentoot:start *acceptor*)
  (format t "~&Mini blog Level2 running: http://127.0.0.1:~a/~%" port)
  (format t "~&Archive: http://127.0.0.1:~a/archive~%" port)
  (format t "~&Example: http://127.0.0.1:~a/post/hello-lisp~%" port)  
  t)


(defun stop-blog ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  t)



(defun restart-blog ()
  (stop-blog)
  (start-blog))

