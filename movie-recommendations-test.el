(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'movie-recommendations)
(require 'mock-server)

(defun stop-servers ()
  (dolist (port (elnode-ports))
    (elnode-stop port)))

(describe "movie-recomendations"
  (before-each
    (start-mock-server))
  (after-each
    (stop-servers))
  (it "exists and load (1)"
    (expect t :to-be t))
  (it "exists and load (2)"
    (expect t :to-be t)))
