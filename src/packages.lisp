;;; 只放defpackage/in-package（让包定义永远固定）
(defpackage :blog
  (:use :cl :hunchentoot)
  (:export :start-blog
           :stop-blog
           :restart-blog
           :defpost
           :*port*
           :*data-dir*
           :*posts-dir*))
