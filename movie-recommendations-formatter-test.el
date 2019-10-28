(require 'buttercup)
(require 'json)
(setq load-path (cons "." load-path))
(require 'movie-recommendations-formatter)

(defun read-json (name)
  (json-read-from-string
   (format (with-temp-buffer
             (insert-file-contents (concat "sample_results/" name))
             (buffer-substring (point-min) (point-max)))
           "somewhere" 80)))

(defconst jurassic-json (read-json "jurassic_park_example.json"))
(defconst alone-json (read-json "alone_in_the_dark_example.json"))

(describe "movie-recommendations-formatter"
  (describe "movie-recommendations-format-data"
    (it "formats the json data for a positive review"
      (expect (movie-recommendations-format-data jurassic-json)
              :to-equal "Title: Jurassic Park

[http://somewhere:80/jurassic.jpg]

Year: 1993
Rated: PG-13
Running Time: 127 min
Plot: A pragmatic Paleontologist visiting an almost complete theme park is tasked with protecting a couple of kids after a power failure causes the park's cloned dinosaurs to run loose.

You should watch this movie right now!"))
    (it "formats the json data for a negative review"
      (expect (movie-recommendations-format-data alone-json)
              :to-equal "Title: Alone in the Dark

[http://somewhere:80/alone.jpg]

Year: 2005
Rated: R
Running Time: 96 min
Plot: A detective of the paranormal slowly unravels mysterious events with deadly results.

This is a bad movie, avoid it if you can!"))))
