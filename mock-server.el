;;; -*- lexical-binding: t -*-
(require 'cl)
(require 'web-server)
(require 'json)

(defconst valid-api-key "API-KEY")
(defconst movie-title-1 "alone+in+the+dark")
(defconst movie-title-2 "jurassic+park")
(defconst movie-present (list movie-title-1 movie-title-2))

(defmacro my-lexical-binding-q ()
  "Copied from here: 'https://yoo2080.wordpress.com/2011/12/30/how-to-check-dynamically-if-lexical-scoping-is-active-in-emacs-lisp/'"
  (let ((tempvar (make-symbol "x")))
    `(let ((,tempvar nil)
           (f (let ((,tempvar t)) (lambda () ,tempvar))))
       (funcall f))))
(assert (my-lexical-binding-q))

(defmacro comment (&rest args))

(defvar samples-dir "sample_results")

(defun replace-server (text)
  (format text "localhost" "8080"))

(defconst mock-server--result (list 'jurassic-park  '("jurassic_park_example.json" . replace-server)
                                    'alone-in-the-dark '("alone_in_the_dark_example.json" . replace-server)
                                    'search-not-found  '("movie_not_found.json" . identity)
                                    'invalid-key '("invalid_api_key.json" . identity)
                                    'no-key  '("no_api_key.json" . identity)
                                    'jurassic-image '("jurassic.jpg" . identity)
                                    'alone-image '("alone.jpg" . identity)))

(defun mock-server--get-sample (response-type)
  (let* ((example-data (plist-get mock-server--result response-type))
         (file-to-load (concat (file-name-as-directory "sample_results")
                              (car example-data))))
    (with-temp-buffer
      (insert-file-contents file-to-load)
      (funcall (cdr example-data)
               (buffer-substring-no-properties (point-min) (point-max))))))

(defun mock-server--switch-page (path api-key search)
  (comment
   (with-current-buffer (find-file "/tmp/visited.txt")
     (goto-char (point-max))
     (insert (format "%s %s\n" api-key search))
     (basic-save-buffer)
     (kill-buffer)))
  (cond
   ((equal path "/jurassic.jpg") 'jurassic-image)
   ((equal path "/alone.jpg") 'alone-image)
   ((not api-key) 'no-key)
   ((not (equal valid-api-key api-key)) 'invalid-key)
   ((not (member search movie-present)) 'search-not-found)
   ((equal movie-title-1 search) 'alone-in-the-dark)
   ((equal movie-title-2 search) 'jurassic-park)
   (t "unexpected condition")))

(defun param-value (headers key)
  (cdr (assoc key headers)))

(defun path (headers)
  (cdr (assoc :GET headers)))

(defun mock-server--select-content (headers)
  (mock-server--get-sample
   (let ((api-key (param-value headers "apikey"))
         (search (param-value headers "t"))
         (path (path headers)))
     (mock-server--switch-page path api-key search))))

(defun mock-imdb-handler (response)
  (with-slots (process headers) response
      (let ((content (mock-server--select-content headers)))
        (ws-response-header process 200
                            '("Content Type" . "application/json")
                            (cons "Content-Length" (number-to-string (length content))))
        (process-send-string process content))))

(defun stop-mock-server ()
  (interactive)
  (ws-stop-all))

(defun start-mock-server (&optional cycle)
  (interactive)
  (when cycle
    (stop-mock-server))
  (ws-start 'mock-imdb-handler 8080))

(defmacro with-debug-server (&rest forms)
  (let ((result (make-symbol "result")))
    `(progn
       (start-mock-server t)
       (let ((,result (progn ,@forms)))
         (stop-mock-server)
         ,result))))

(provide 'mock-server)
