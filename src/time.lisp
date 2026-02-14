(in-package :blog)

(defun now-ut ()
  "Universal time (integer seconds)."
  (get-universal-time))

(defun ut->ymdhm (ut)
  "Format universal time -> YYYY-MM-DD HH:MM (local time)."
  (if (and (integerp ut) (<= 0 ut))
      (multiple-value-bind (sec min hour day month year) (decode-universal-time ut)
        (declare (ignore sec))
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D" year month day hour min))
      "N/A"))

(defun ut->ym (ut)
  "YYYY-MM for grouping."
  (if (and (integerp ut) (<= 0 ut))
      (multiple-value-bind (sec min hour day month year) (decode-universal-time ut)
        (declare (ignore sec min hour day))
        (format nil "~4,'0D-~2,'0D" year month))
      "0000-00"))



;;; ------------------------------
;;; RSS date (RFC 822)
;;; ------------------------------
(defparameter *rfc822-dow* #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(defparameter *rfc822-month* #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun ut->rfc822 (ut)
  "RFC 822 date string for RSS/Atom (local time)."
  (let ((ut (if (and (integerp ut) (<= 0 ut)) ut 0)))
    (multiple-value-bind (sec min hour day month year dow _dst tz)
        (decode-universal-time ut)
      (declare (ignore _dst))
      ;; Defensive: some environments may yield NIL timezone; treat as UTC.
      (let* ((tz (if (realp tz) tz 0))
             ;; tz is hours west of GMT. RFC822 wants +/-HHMM.
             (sign (if (plusp tz) "-" "+"))
             (hh (truncate (abs tz)))
             (offset (format nil "~a~2,'0D00" sign hh)))
        (format nil "~a, ~2,'0D ~a ~4,'0D ~2,'0D:~2,'0D:~2,'0D ~a"
                (aref *rfc822-dow* dow)
                day
                (aref *rfc822-month* (1- month))
                year hour min sec offset)))))
