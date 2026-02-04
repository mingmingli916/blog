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
