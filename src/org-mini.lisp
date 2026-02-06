;;;; src/org-mini.lisp
;;;; Minimal Org (subset) -> blog DSL SEXP
;;;; - No Emacs required
;;;; - Only supports:
;;;;   1) headlines (* .. ***)
;;;;   2) math (inline $..$; block $$..$$ and \[..\])
;;;;   3) lists (-/+ and 1.), with indentation nesting
;;;;   4) images ([[file:..][alt]] / [[..]]), inline or alone
;;;;   5) code blocks (#+begin_src LANG .. #+end_src)
;;;;
;;;; Metadata:
;;;;   #+TITLE: -> :title
;;;;   #+SLUG:  -> :slug
;;;;   #+DATE:  -> :created-at (YYYY-MM-DD, 00:00 local time)
;;;; updated-at:
;;;;   - Always equals (file-write-date org-file)

(in-package :blog)

;;; ------------------------------------------------------------
;;; Utilities (no extra deps)
;;; ------------------------------------------------------------

(defun %trim (s)
  (string-trim '(#\Space #\Tab #\Return #\Newline) (or s "")))

(defun %blank-line-p (s)
  (every (lambda (ch) (member ch '(#\Space #\Tab #\Return))) (or s "")))

(defun %starts-with-ci-p (s prefix)
  (let* ((s (or s ""))
         (p (or prefix ""))
         (n (length p)))
    (and (<= n (length s))
         (string-equal p s :end2 n))))

(defun %line-indent (s)
  (loop for ch across (or s "")
        for i from 0
        while (member ch '(#\Space #\Tab))
        finally (return i)))

(defun %join-lines (lines)
  (with-output-to-string (out)
    (dolist (l lines)
      (write-string l out)
      (write-char #\Newline out))))

(defun %read-file-lines (path)
  (with-open-file (in path :direction :input)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun %parse-ymd->ut (ymd)
  "Parse YYYY-MM-DD to universal time at 00:00:00 local time. Return NIL if invalid."
  (handler-case
      (let* ((s (%trim ymd)))
        (when (< (length s) 10) (error "short date"))
        (let ((y (parse-integer s :start 0 :end 4))
              (m (parse-integer s :start 5 :end 7))
              (d (parse-integer s :start 8 :end 10)))
          (encode-universal-time 0 0 0 d m y)))
    (error () nil)))

;;; ------------------------------------------------------------
;;; Metadata: #+TITLE / #+SLUG / #+DATE
;;; ------------------------------------------------------------

(defun %read-org-metadata (lines)
  "Return (values slug title date-ut)."
  (let ((slug nil)
        (title nil)
        (date-ut nil))
    (dolist (l lines)
      (let ((tline (%trim l)))
        (cond
          ((and (null slug) (%starts-with-ci-p tline "#+slug:"))
           (setf slug (%trim (subseq tline (length "#+slug:")))))

          ((and (null title) (%starts-with-ci-p tline "#+title:"))
           (setf title (%trim (subseq tline (length "#+title:")))))

          ((and (null date-ut) (%starts-with-ci-p tline "#+date:"))
           (let ((ds (%trim (subseq tline (length "#+date:")))))
             (setf date-ut (%parse-ymd->ut ds)))))))
    (values slug title date-ut)))

;;; ------------------------------------------------------------
;;; Headline: * / ** / ***
;;; ------------------------------------------------------------

(defun %parse-headline (line)
  "Return node or NIL. Only levels 1..3."
  (let* ((s (or line ""))
         (n (length s))
         (i 0))
    (loop while (and (< i n) (char= (char s i) #\*)) do (incf i))
    (when (and (> i 0) (<= i 3) (< i n) (char= (char s i) #\Space))
      (let ((title (%trim (subseq s (1+ i)))))
        (case i
          (1 (list 'h1 title))
          (2 (list 'h2 title))
          (3 (list 'h3 title)))))))

;;; ------------------------------------------------------------
;;; Org link [[...]] -> (img (:src ... :alt ...))
;;; ------------------------------------------------------------

(defun %normalize-org-url (url)
  (let ((u (%trim url)))
    (if (%starts-with-ci-p u "file:")
        (subseq u 5)
        u)))

(defun %parse-org-link (s start)
  "Parse org link beginning at START (at '[[').
Return (values node end-index) where end-index is index after closing ']]',
or NIL if not valid."
  (let ((n (length s)))
    (when (and (<= (+ start 2) n)
               (char= (char s start) #\[)
               (char= (char s (1+ start)) #\[))
      (let ((close (search "]]" s :start2 (+ start 2))))
        (when close
          (let* ((inner (subseq s (+ start 2) close))
                 ;; inner can be: "file:a.png" or "file:a.png][alt text"
                 (sep (search "][" inner))
                 (raw-url (if sep (subseq inner 0 sep) inner))
                 (alt (if sep (subseq inner (+ sep 2)) ""))
                 (src (%normalize-org-url raw-url)))
            (values (list 'img (list :src src :alt alt))
                    (+ close 2))))))))

;;; ------------------------------------------------------------
;;; Inline parsing:
;;; - images: [[...]]
;;; - inline math: $...$
;;; ------------------------------------------------------------

(defun %parse-inline (text)
  "Return list of inline children: strings / (math (:mode \"inline\") \"...\") / (img ...)."
  (let* ((s (or text ""))
         (n (length s))
         (i 0)
         (chunk-start 0)
         (acc '()))
    (labels ((emit-chunk (end)
               (when (< chunk-start end)
                 (push (subseq s chunk-start end) acc)))
             (emit-node (node)
               (push node acc)))
      (loop while (< i n) do
        (cond
          ;; org link [[...]] -> img
          ((and (< (1+ i) n)
                (char= (char s i) #\[)
                (char= (char s (1+ i)) #\[))
           (multiple-value-bind (node next) (%parse-org-link s i)
             (if node
                 (progn
                   (emit-chunk i)
                   (emit-node node)
                   (setf i next)
                   (setf chunk-start i))
                 (incf i))))

          ;; inline math $...$
          ((char= (char s i) #\$)
           (let ((j (position #\$ s :start (1+ i))))
             (if j
                 (progn
                   (emit-chunk i)
                   (emit-node (list 'math (list :mode "inline") (subseq s (1+ i) j)))
                   (setf i (1+ j))
                   (setf chunk-start i))
                 (incf i))))

          (t
           (incf i))))

      (emit-chunk n)
      ;; Keep empty strings out
      (nreverse (remove-if (lambda (x) (and (stringp x) (zerop (length x)))) acc)))))

;;; ------------------------------------------------------------
;;; Lists (indent-based):
;;; - ul: - item / + item
;;; - ol: 1. item
;;; Nesting: greater indentation starts nested list
;;; ------------------------------------------------------------

(defun %list-item-info (line)
  "Return (values kind indent text) where kind is 'ul or 'ol, or NIL."
  (let* ((ind (%line-indent line))
         (s (subseq (or line "") ind)))
    (cond
      ((or (%starts-with-ci-p s "- ") (%starts-with-ci-p s "+ "))
       (values 'ul ind (%trim (subseq s 2))))
      ((and (>= (length s) 3)
            (digit-char-p (char s 0))
            (char= (char s 1) #\.)
            (char= (char s 2) #\Space))
       (values 'ol ind (%trim (subseq s 3))))
      (t nil))))

(defun %parse-list-block (lines start)
  "Parse contiguous list block starting at START.
Return (values node next-index)."
  (labels
      ((parse-at (i base-indent kind)
         (let ((items '())
               (k i))
           (loop while (< k (length lines)) do
             (multiple-value-bind (knd ind text) (%list-item-info (nth k lines))
               (cond
                 ;; not a list item -> end block
                 ((null knd) (return))
                 ;; less indent than base -> end block
                 ((< ind base-indent) (return))
                 ;; deeper indent -> nested list attaches to last li
                 ((> ind base-indent)
                  (multiple-value-bind (subnode nk) (%parse-list-block lines k)
                    (when items
                      ;; (li <children...>) so we append nested list node into children
                      (setf (cdr (car items))
                            (append (cdr (car items)) (list subnode))))
                    (setf k nk)))
                 ;; same indent but different list kind -> end block
                 ((and (= ind base-indent) (not (eql knd kind)))
                  (return))
                 ;; normal item
                 (t
                  (push (cons 'li (%parse-inline text)) items)
                  (incf k)))))
           (values (cons kind (nreverse items)) k))))
    (multiple-value-bind (kind0 ind0 _t0) (%list-item-info (nth start lines))
      (declare (ignore _t0))
      (parse-at start ind0 kind0))))

;;; ------------------------------------------------------------
;;; Block-level parsing:
;;; - headlines
;;; - code blocks
;;; - math blocks
;;; - lists
;;; - paragraphs
;;; ------------------------------------------------------------

(defun org-lines->content (lines)
  "Parse org LINES and return DSL content nodes list."
  (let ((i 0)
        (out '())
        (pbuf '()))
    (labels
        ((flush-paragraph ()
           (when pbuf
             ;; join paragraph lines with spaces
             (let* ((joined (%trim (with-output-to-string (o)
                                    (loop for s in (nreverse pbuf)
                                          do (write-string (%trim s) o)
                                             (write-char #\Space o))))))
               (when (> (length joined) 0)
                 (push (cons 'p (%parse-inline joined)) out)))
             (setf pbuf '())))

         (collect-until (start end-pred)
           (let ((acc '())
                 (k start))
             (loop while (and (< k (length lines))
                              (not (funcall end-pred (nth k lines))))
                   do (push (nth k lines) acc)
                      (incf k))
             (values (nreverse acc) k))))

      (loop while (< i (length lines)) do
        (let* ((line (nth i lines))
               (tline (%trim line)))
          (cond
            ;; blank line ends paragraph
            ((%blank-line-p line)
             (flush-paragraph)
             (incf i))

            ;; ignore metadata lines in body
            ((or (%starts-with-ci-p tline "#+title:")
                 (%starts-with-ci-p tline "#+slug:")
                 (%starts-with-ci-p tline "#+date:")
                 (%starts-with-ci-p tline "#+options:"))
             (incf i))

            ;; code block: #+begin_src LANG ... #+end_src
            ((%starts-with-ci-p tline "#+begin_src")
             (flush-paragraph)
             (let ((lang (%trim (subseq tline (length "#+begin_src")))))
               (multiple-value-bind (body k)
                   (collect-until (1+ i)
                                  (lambda (l) (%starts-with-ci-p (%trim l) "#+end_src")))
                 (push (list 'src (list :lang (if (> (length lang) 0) lang "text"))
                             (%join-lines body))
                       out)
                 ;; skip end marker line
                 (setf i (min (length lines) (+ k 1))))))

            ;; math block: $$ ... $$
            ((string= tline "$$")
             (flush-paragraph)
             (multiple-value-bind (body k)
                 (collect-until (1+ i) (lambda (l) (string= (%trim l) "$$")))
               (push (list 'math (list :mode "block") (%trim (%join-lines body))) out)
               (setf i (min (length lines) (+ k 1)))))

            ;; math block: \[ ... \]
            ((string= tline "\\[")
             (flush-paragraph)
             (multiple-value-bind (body k)
                 (collect-until (1+ i) (lambda (l) (string= (%trim l) "\\]")))
               (push (list 'math (list :mode "block") (%trim (%join-lines body))) out)
               (setf i (min (length lines) (+ k 1)))))

            ;; headline
            ((%parse-headline line)
             (flush-paragraph)
             (push (%parse-headline line) out)
             (incf i))

            ;; list block
            ((multiple-value-bind (k _ind _txt) (%list-item-info line)
               (declare (ignore _ind _txt))
               k)
             (flush-paragraph)
             (multiple-value-bind (node next) (%parse-list-block lines i)
               (when node (push node out))
               (setf i next)))

            ;; paragraph line
            (t
             (push line pbuf)
             (incf i)))))

      (flush-paragraph)
      (nreverse out))))

;;; ------------------------------------------------------------
;;; Export: org file -> .sexp plist
;;; created-at:
;;;   - from #+DATE if present/valid
;;;   - else keep existing :created-at if .sexp exists
;;;   - else (now-ut)
;;; updated-at:
;;;   - file-write-date(org)
;;; ------------------------------------------------------------

(defun export-org-file (in-org out-sexp)
  "Export IN-ORG (org subset) to OUT-SEXP (blog plist)."
  (let* ((lines (%read-file-lines in-org))
         (slug0 (pathname-name in-org))
         (org-mtime (or (file-write-date in-org) (now-ut))))
    (multiple-value-bind (slug title date-ut) (%read-org-metadata lines)
      (let* ((slug (or slug slug0))
             (title (or title slug))
             (existing (and (probe-file out-sexp)
                            (with-open-file (in out-sexp :direction :input)
                              (read in nil nil))))
             (created (or date-ut
                          (and (listp existing) (getf existing :created-at))
                          (now-ut)))
             (updated org-mtime)
             (content (org-lines->content lines))
             (plist (list :slug slug
                          :title title
                          :created-at created
                          :updated-at updated
                          :content content)))
        (ensure-directories-exist out-sexp)
        (with-open-file (out out-sexp
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (write plist :stream out :pretty t :readably t)
          (terpri out))
        plist))))
