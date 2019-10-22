(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'movie-recommendations)

(describe "movie-recomendations"
  (it "exists and load"
    (expect t :to-be t)))
