;;; 项目的配置文件
(in-package :blog)

(defparameter *port* 8080)

(defun getenv (name)
  "Return environment variable value or NIL."
  #+sbcl (sb-ext:posix-getenv name)
  #+ccl (ccl:getenv name)
  #+ecl (si:getenv name)
  #+clisp (ext:getenv name)
  #- (or sbcl cll ecl clisp)
  nil)


(defun ensure-dir (path)
  (ensure-directories-exist path))

(defun xdg-data-home ()
  "Best-effort XDG_DATA_HOME with facllback to ~/.local/share/ ."
  (or (getenv "XDG_DATA_HOME")
      (namestring (merge-pathnames ".local/share/" (user-homedir-pathname)))))


(defun default-data-dir ()
  ;; 项目在运行时的可写根目录
  (merge-pathnames "blog/" (pathname (xdg-data-home))))

(defparameter *data-dir*
  (let ((env (getenv "BLOG_DATA_DIR")))
    (ensure-dir
     (if (and env (> (length env) 0))
         ;; allow both absolute and relative; merge-pathnames makes relative
         (merge-pathnames "" (pathname  env))
         (default-data-dir))))
  "Root directory for runtime data (writable). Can override via BLOG_DATA_DIR.")

(defparameter *posts-dir*
  (ensure-dir (merge-pathnames "posts/" *data-dir*))
  "Where posts are stored as *.sexp files.")
