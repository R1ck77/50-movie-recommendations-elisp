
(defun movie-recommendations--evaluation (rating)
  "A very basic evaluation based on the rating.

A less manichean function would be of course more realistic"
  (if (> rating 7.0)
      "You should watch this movie right now!"
    "This is a bad movie, avoid it if you can!"))

(defun get-image (id url)
  (let ((image-path (concat (file-name-as-directory (concat (file-name-as-directory (file-truename ".")) "cache/images")) (concat id ".jpg"))))
    (if (not (file-exists-p image-path))
        (url-utils-download-image url image-path))
    image-path))

(defun movie-recommendations--image (id url title)
  (let ((image-path (get-image id url))
        (image-alt (format "\[poster for %s\]" title)))
    (put-text-property 0 (length image-alt) 'display (list 'image :file image-path :type 'jpeg) image-alt)
    image-alt))

(defun movie-recommendations-format-data (json)
  (let ((title (alist-get 'Title json))
        (year (alist-get 'Year json))
        (rating (alist-get 'Rated json))
        (length (alist-get 'Runtime json))
        (plot (alist-get 'Plot json))
        (imdb-rating (string-to-number (alist-get 'imdbRating json))))
    (let ((poster (movie-recommendations--image (alist-get 'imdbID json)
                                                (alist-get 'Poster json)
                                                title)))
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
