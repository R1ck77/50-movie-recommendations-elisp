
(defun movie-recommendations--evaluation (rating)
  "A very basic evaluation based on the rating.

A less manichean function would be of course more realistic"
  (if (> rating 7.0)
      "You should watch this movie right now!"
    "This is a bad movie, avoid it if you can!"))

(defun get-image (id url downloader)
  (let ((image-path (concat (file-name-as-directory (concat (file-name-as-directory (file-truename ".")) "cache/images")) (concat id ".jpg"))))
    (if (not (file-exists-p image-path))
        (apply downloader (list url image-path)))
    image-path))

;;; TODO/FIXME wrong place for the caching!
(defun movie-recommendations--image (id url title downloader)
  (let ((image-path (get-image id url downloader))
        (image-alt (format "\[poster for \"%s\"\]" title)))
    (put-text-property 0 (length image-alt) 'display (list 'image :file image-path :type 'jpeg) image-alt)
    image-alt))

;;; TODO/FIXME too many arguments
(defun movie-recommendations-format-data (json image-downloader)
  "Format the json appropriately.

image-downloader is a function that takes a url and a path"
  (let ((title (alist-get 'Title json))
        (year (alist-get 'Year json))
        (rating (alist-get 'Rated json))
        (length (alist-get 'Runtime json))
        (plot (alist-get 'Plot json))
        (imdb-rating (string-to-number (alist-get 'imdbRating json))))
    (let ((poster (movie-recommendations--image (alist-get 'imdbID json)
                                                (alist-get 'Poster json)
                                                title
                                                image-downloader)))
      (format "Title: %s

%s

Year: %s
Rated: %s
Running Time: %s
Plot: %s

%s" title poster year rating
length plot
(movie-recommendations--evaluation imdb-rating)))))


(provide 'movie-recommendations-formatter)
