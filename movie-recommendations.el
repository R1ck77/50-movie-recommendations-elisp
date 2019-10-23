(require 'url-utils)

(defvar movie-recommendations-debug-server nil
  "A port to use for debugging. Set it to a cons list with host and port of the mock server")

(defun movie-recommendations--search-movie (api-key search-string)
  (url-utils-get-json-url-content "http://localhost:8080/foobar"))


(provide 'movie-recommendations)
