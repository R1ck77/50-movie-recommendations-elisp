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
  (erase-buffer)
  (setq major-mode 'movie-recommendations-mode)
  (setq mode-name movie-recommendations-mode-name))

(defun movie-recommendations--errorp (answer)
  (equal "False" (alist-get 'Response answer)))

(defun movie-recommendations--error-reason (answer)
  (alist-get 'Error answer))

(defun movie-recommendations--handle-error (answer)
  (movie-recommendations--error-reason answer))

(defun movie-recommendations--route-answer (answer)
  (if (movie-recommendations--errorp answer)
      (movie-recommendations--handle-error answer)
    "I didn't think it this through"))

(defun movie-recommendations-read-api-key ()
  (interactive)
  "Read the API key from the \"api-key.txt\" file"
  (condition-case nil
   (with-temp-buffer
     (insert-file-contents "api-key.txt")
     (buffer-substring (point-min)
                       (point-max)))
   (error nil)))

(defun movie-recommendations--no-key ()
  (insert "No API key provided!"))

(defun movie-recommendations--for-key (api-key)
  (let* ((movie-name (read-string "Enter the name of a movie: "))
         (imdb-answer (movie-recommendations--search-movie api-key movie-name)))
    (insert (movie-recommendations--route-answer imdb-answer))))

(defun movie-recommendations--procedure ()
  (let ((api-key (movie-recommendations-read-api-key)))
    (if (not api-key)
        (movie-recommendations--no-key)
      (movie-recomendations--for-key api-key))))

(defun movie-recommendations ()
  (interactive)
  (switch-to-buffer (get-buffer-create movie-recommendations-buffer-name))
  (movie-recommendations--mode)
  (movie-recommendations--procedure))

(provide 'movie-recommendations)
