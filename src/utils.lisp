(in-package :common-social.utils)

(defun timestamp-to-relative-string (timestamp)
  "Convert timestamp to relative time string (e.g., '2h ago')"
  (let* ((now (local-time:now))
         (diff (local-time:timestamp-difference now timestamp))
         (seconds (floor diff))
         (minutes (floor seconds 60))
         (hours (floor minutes 60))
         (days (floor hours 24)))
    (cond
      ((< seconds 60) "now")
      ((< minutes 60) (format nil "~dm" minutes))
      ((< hours 24) (format nil "~dh" hours))
      ((< days 7) (format nil "~dd" days))
      (t (local-time:format-timestring nil timestamp :format '(:short-month " " :day))))))

(defun escape-html (string)
  "Basic HTML escaping for user content"
  (when (stringp string)
    (with-output-to-string (out)
      (loop for char across string do
        (case char
          (#\< (write-string "&lt;" out))
          (#\> (write-string "&gt;" out))
          (#\& (write-string "&amp;" out))
          (#\" (write-string "&quot;" out))
          (#\' (write-string "&#39;" out))
          (otherwise (write-char char out)))))))

(defun string-trim-whitespace (string)
  "Trim whitespace from string"
  (when (stringp string)
    (string-trim '(#\Space #\Tab #\Newline #\Return) string)))