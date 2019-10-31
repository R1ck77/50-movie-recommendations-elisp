;;; -*- lexical-binding: t -*-
(require 'elnode)
(require 'json)

(defconst valid-api-key "API-KEY")
(defconst movie-title-1 "alone in the dark")
(defconst movie-title-2 "jurassic park")
(defconst movie-present (list movie-title-1 movie-title-2))

(defun elnode-error (msg &rest arg)
  "Temporary workaround for elnode errors showing during the test.")

(defmacro my-lexical-binding-q ()
  "Copied from here: 'https://yoo2080.wordpress.com/2011/12/30/how-to-check-dynamically-if-lexical-scoping-is-active-in-emacs-lisp/'"
  (let ((tempvar (make-symbol "x")))
    `(let ((,tempvar nil)
           (f (let ((,tempvar t)) (lambda () ,tempvar))))
       (funcall f))))
(assert (my-lexical-binding-q))

(defmacro comment (&rest args))

(defvar samples-dir "sample_results")

(defun info-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content Type" . "application/json"))
  (elnode-http-return httpcon (format "path: %s\nparameters: %s" (elnode-http-pathinfo httpcon) (elnode-http-params httpcon))))

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

(defun mock-server--select-content (httpcon)
  (mock-server--get-sample
   (let ((api-key (elnode-http-param httpcon "apikey"))
         (search (elnode-http-param httpcon "t"))
         (path (elnode-http-pathinfo httpcon)))
     (mock-server--switch-page path api-key search))))

(defun mock-imdb-handler (httpcon)
  (let ((content (mock-server--select-content httpcon)))
    (elnode-http-start httpcon 200
                             '("Content Type" . "application/json")
                             (cons "Content-Length"
                                   (number-to-string (length content))))
    (elnode-http-return httpcon content)))

(defun stop-mock-server ()
  (interactive)
  (condition-case nil
      (elnode-stop 8080)
    (error nil)))

(defun start-mock-server (&optional cycle)
  (interactive)
  (when cycle
    (stop-mock-server))
  (elnode-start 'mock-imdb-handler :port 8080))

(defun stop-servers ()
  (dolist (port (elnode-ports))
    (elnode-stop port)))

(defmacro with-debug-server (&rest forms)
  (let ((result (make-symbol "result")))
    `(progn
       (start-mock-server t)
       (let ((,result (progn ,@forms)))
         (stop-servers)
         ,result))))

(provide 'mock-server)
