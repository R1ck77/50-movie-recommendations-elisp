
(defun movie-recommendations--evaluation (rating)
  "A very basic evaluation based on the rating.

A less manichean function would be of course more realistic"
  (if (> rating 7.0)
      "You should watch this movie right now!"
    "This is a bad movie, avoid it if you can!"))

(defun movie-recommendations--image (url title image-downloader)
  (let ((image-path (url-utils-cached-get-image url image-downloader))
        (image-alt (format "\[poster for \"%s\"\]" title)))
    (put-text-property 0 (length image-alt) 'display (list 'image :file image-path :type 'jpeg) image-alt)
    image-alt))

(defun movie-recommendations-format-data (json &optional image-downloader)
  "Format the json appropriately.

image-downloader is a function that takes a url and a path"
  (let ((title (alist-get 'Title json))
        (year (alist-get 'Year json))
        (rating (alist-get 'Rated json))
        (length (alist-get 'Runtime json))
        (plot (alist-get 'Plot json))
        (imdb-rating (string-to-number (alist-get 'imdbRating json))))
    (let ((poster (movie-recommendations--image (alist-get 'Poster json)
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
