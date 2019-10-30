(require 'url)

(defun url-utils--get-url-content (url)
  "Doesn't handle networking issues or special conditions"
  (let ((url-utils--get-url-content))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (forward-line)
      (buffer-substring-no-properties (point) (point-max)))))

(defun url-utils-get-json-url-content (url)
  (json-read-from-string (url-utils--get-url-content url)))

(defun url-utils-download-image (url path)
  (with-current-buffer (url-retrieve-synchronously url t)
    (write-region (point-min) (point-max) path))
  path)


(provide 'url-utils)
