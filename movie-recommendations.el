(setq load-path (cons "." load-path))
(require 'url-utils)

(defconst movie-recommendations-buffer-name "IMDb movies recommendations")
(defconst movie-recommendations-mode-name "*IMDb*")

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

(defun movie-recommendations--mode ()
  (kill-all-local-variables)
  (setq major-mode 'movie-recommendations-mode)
  (setq mode-name movie-recommendations-mode-name)
  )

(defun movie-recommendations ()
  (interactive)
  (switch-to-buffer (get-buffer-create movie-recommendations-buffer-name))
  (movie-recommendations--mode)
  (read-string "Enter the name of a movie: "))


(provide 'movie-recommendations)
