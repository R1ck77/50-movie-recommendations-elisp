(setq load-path (cons "." load-path))
(require 'url-utils)

(defun url-utils-cached-get-json-url-content (url)
  (url-utils-get-json-url-content url))

(provide 'cached-url-utils)
