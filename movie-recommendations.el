(require 'url-utils)

(defvar movie-recommendations-server '("www.omdbapi.com" . 80)
  "Set it to a cons list with host and port of the server")

(defun movie-recommendations--get-base-url ()
  (format "http://%s:%d/" (car movie-recommendations-server) (cdr movie-recommendations-server)))

(defun movie-recommendations--format-search-string (search-string)
  (replace-regexp-in-string "[ ]+" "+" search-string))

(defun movie-recommendations--search-movie (api-key search-string)
  (let ((url (format "%s?apikey=%s&t=%s"
                     (movie-recommendations--get-base-url)
                     api-key
                     (movie-recommendations--format-search-string search-string))))
    (url-utils-get-json-url-content url)))


(provide 'movie-recommendations)
