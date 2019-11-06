(require 'url)

(defun url-utils--retrieve-url (url content-consumer)
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char url-http-end-of-headers)
    (forward-line)
    (apply content-consumer (list (point) (point-max)))))

(defun url-utils--get-url-content (url)
  "Doesn't handle networking issues or special conditions"
  (let ((url-utils--get-url-content))
    (url-utils--retrieve-url url 'buffer-substring-no-properties)))

(defun url-utils-get-json-url-content (url)
  (json-read-from-string (url-utils--get-url-content url)))

(defun url-utils-download-image (url path)
  (url-utils--retrieve-url url (lambda (beginning end)
                                 (write-region beginning end path)))
  path)

(provide 'url-utils)
