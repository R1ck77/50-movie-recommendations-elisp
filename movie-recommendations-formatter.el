
(defun movie-recommendations--evaluation (rating)
  "A very basic evaluation based on the rating.

A less manichean function would be of course more realistic"
  (if (> rating 7.0)
      "You should watch this movie right now!"
    "This is a bad movie, avoid it if you can!"))

(defun movie-recommendations-format-data (json)
  (let ((title (alist-get 'Title json))
        (year (alist-get 'Year json))
        (rating (alist-get 'Rated json))
        (length (alist-get 'Runtime json))
        (plot (alist-get 'Plot json))
        (imdb-rating (string-to-number (alist-get 'imdbRating json))))
    (format "Title: %s
Year: %s
Rated: %s
Running Time: %s
Plot: %s

%s"
            title year rating
            length plot
            (movie-recommendations--evaluation imdb-rating))))


(provide 'movie-recommendations-formatter)
