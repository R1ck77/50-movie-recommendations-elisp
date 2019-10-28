(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'movie-recommendations)
(require 'mock-server)

(defun stop-servers ()
  (dolist (port (elnode-ports))
    (elnode-stop port)))

(defmacro with-debug-server (&rest forms)
  (declare (indent 0))
  `(let ((movie-recommendations-server '("localhost" . 8080)))
     ,@forms))

(defun save-to-file (file content)
  (with-temp-buffer
    (insert content)
    (write-file file)))

(defmacro with-api-file (content &rest forms)
  (declare (indent 1))
  `(unwind-protect 
       (progn
         (save-to-file "api-key.txt" ,content)
         ,@forms)     
     (delete-file "api-key.txt")))

(defun remove-api-key ()
  (condition-case nil
      (delete-fil "api-key.txt")
    (error nil)))

(describe "movie-recommendations"
  (before-all
    (start-mock-server))
  (after-all
    (stop-servers))
  (describe "movie-recommendations--search-movie"
    (it "returns the \"invalid API key\" content"
      (expect (with-debug-server
                  (movie-recommendations--search-movie "INVALID_KEY" "jurassic park"))
              :to-equal '((Response . "False")
                          (Error . "Invalid API key!"))))
    (it "returns the correct movie data if the movie is present"
      (expect (with-debug-server
                  (movie-recommendations--search-movie "API-KEY" "jurassic park"))
              :to-equal '((Title . "Jurassic Park")
                          (Year . "1993")
                          (Rated . "PG-13")
                          (Released . "11 Jun 1993")
                          (Runtime . "127 min")
                          (Genre . "Action, Adventure, Sci-Fi, Thriller")
                          (Director . "Steven Spielberg")
                          (Writer . "Michael Crichton (novel), Michael Crichton (screenplay), David Koepp (screenplay)")
                          (Actors . "Sam Neill, Laura Dern, Jeff Goldblum, Richard Attenborough")
                          (Plot . "A pragmatic Paleontologist visiting an almost complete theme park is tasked with protecting a couple of kids after a power failure causes the park's cloned dinosaurs to run loose.")
                          (Language . "English, Spanish")
                          (Country . "USA")
                          (Awards . "Won 3 Oscars. Another 32 wins & 25 nominations.")
                          (Poster . "http://localhost:8080/jurassic.jpg")
                          (Ratings .
                                   [((Source . "Internet Movie Database")
                                     (Value . "8.1/10"))
                                    ((Source . "Rotten Tomatoes")
                                     (Value . "91%"))
                                    ((Source . "Metacritic")
                                     (Value . "68/100"))])
                          (Metascore . "68")
                          (imdbRating . "8.1")
                          (imdbVotes . "796,497")
                          (imdbID . "tt0107290")
                          (Type . "movie")
                          (DVD . "10 Oct 2000")
                          (BoxOffice . "$45,299,680")
                          (Production . "Universal City Studios")
                          (Website . "N/A")
                          (Response . "True"))))
    (it "returns the \"movie not found\" data if no movie is present"
      (expect (with-debug-server
                  (movie-recommendations--search-movie "API-KEY" "invalid-movie"))
              :to-equal '((Response . "False")
                          (Error . "Movie not found!")))))
  (describe "movie-recommendation"
    (it "puts the user in a buffer with the correct name"
      (with-api-file "API-KEY"
        (with-debug-server
          (spy-on 'read-string :and-return-value "42")
          (movie-recommendations)))
      (expect (buffer-name) :to-equal "IMDb movies recommendations"))
    (it "creates a buffer in visual line mode"
      (remove-api-key)
      (movie-recommendations)
      (expect visual-line-mode :to-be t))
    (it "requests the title of a movie"
      (with-api-file "API-KEY"
        (with-debug-server
          (spy-on 'read-string :and-return-value "movie title")          
          (movie-recommendations)))
      (expect 'read-string :to-have-been-called-with "Enter the name of a movie: "))
    (it "puts the user in a buffer with the correct mode"
      (with-api-file "API-KEY"
        (with-debug-server
            (spy-on 'read-string :and-return-value "42")
            (movie-recommendations)))
      (expect mode-name :to-equal "*IMDb*"))
    (it "writes an error message if the movie is not found"
      (with-api-file "API-KEY"
        (with-debug-server
          (spy-on 'read-string :and-return-value "missing movie")
          (movie-recommendations)))
          (expect (buffer-substring (point-min) (point-max))
                  :to-equal "Movie not found!"))
    (it "writes an error message if no key is provided"
      (remove-api-key)
      (spy-on 'read-string :and-return-value "")
      (movie-recommendations)
      (expect (buffer-substring (point-min) (point-max))
              :to-equal "No API key provided!"))
    (it "writes an error message if the key provided is wrong"
      (with-api-file "wrong API-KEY"
        (with-debug-server
            (spy-on 'read-string :and-return-value "")
            (movie-recommendations)))
      (expect (buffer-substring (point-min) (point-max))
              :to-equal "Invalid API key!"))
    (it "writes the movie details and the rating, with a positive review"
      (spy-on 'read-string :and-return-value "jurassic park")
      (with-api-file "API-KEY"
        (with-debug-server
          (movie-recommendations)))
      (expect (buffer-substring (point-min) (point-max))
                  :to-equal "Title: Jurassic Park

[http://localhost:8080/jurassic.jpg]

Year: 1993
Rated: PG-13
Running Time: 127 min
Plot: A pragmatic Paleontologist visiting an almost complete theme park is tasked with protecting a couple of kids after a power failure causes the park's cloned dinosaurs to run loose.

You should watch this movie right now!"))
    (it "writes the movie details and the rating, with a negative review"
      (spy-on 'read-string :and-return-value "alone in the dark")
      (with-api-file "API-KEY"
        (with-debug-server
          (movie-recommendations)))
      (expect (buffer-substring (point-min) (point-max))
                  :to-equal "Title: Alone in the Dark

[http://localhost:8080/alone.jpg]

Year: 2005
Rated: R
Running Time: 96 min
Plot: A detective of the paranormal slowly unravels mysterious events with deadly results.

This is a bad movie, avoid it if you can!"))))
