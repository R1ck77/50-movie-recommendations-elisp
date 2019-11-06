(require 's)
(setq load-path (cons "." load-path))
(require 'cached-url-utils)
(require 'json)
(require 'movie-recommendations-formatter)

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
    (url-utils-cached-get-json-url-content url)))

(defun movie-recommendations--mode ()
  (kill-all-local-variables)
  (erase-buffer)
  (setq major-mode 'movie-recommendations-mode)
  (setq mode-name movie-recommendations-mode-name)
  (visual-line-mode))

(defun movie-recommendations--errorp (answer)
  (equal "False" (alist-get 'Response answer)))

(defun movie-recommendations--error-reason (answer)
  (alist-get 'Error answer))

(defun movie-recommendations--handle-error (answer)
  (insert (movie-recommendations--error-reason answer)))

(defun movie-recommendations--handle-movie-content (answer)
  (insert (movie-recommendations-format-data answer)))

(defun movie-recommendations--handle-answer (answer)
  (if (movie-recommendations--errorp answer)
      (movie-recommendations--handle-error answer)
    (movie-recommendations--handle-movie-content answer)))

(defun movie-recommendations-read-api-key ()
  "Read the API key from the \"api-key.txt\" file"
  (interactive)
  (condition-case nil
   (with-temp-buffer
     (insert-file-contents "api-key.txt")
     (s-trim
      (buffer-substring (point-min)
                        (point-max))))
   (error nil)))

(defun movie-recommendations--no-key ()
  (insert "No API key provided!"))

(defun movie-recommendations--for-key (api-key)
  (let* ((movie-name (read-string "Enter the name of a movie: "))
         (imdb-answer (movie-recommendations--search-movie api-key movie-name)))
    (movie-recommendations--handle-answer imdb-answer)))

(defun movie-recommendations--procedure ()
  (let ((api-key (movie-recommendations-read-api-key)))
    (if (not api-key)
        (movie-recommendations--no-key)
      (movie-recommendations--for-key api-key))))

(defun movie-recommendations ()
  (interactive)
  (switch-to-buffer (get-buffer-create movie-recommendations-buffer-name))
  (erase-buffer)
  (movie-recommendations--mode)
  (movie-recommendations--procedure)
  (goto-char (point-min)))

(defun movie-recommendations-clear-cache ()
  (url-utils-cached-clear))

(provide 'movie-recommendations)
