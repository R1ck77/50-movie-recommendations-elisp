;;; -*- lexical-binding: t -*-
(require 'elnode)

(defconst valid-api-key "API-KEY")
(defconst movie-present-search "jurassic park")

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

(defconst mock-server--result '(search-result  "jurassic_park_example.json"
                                search-not-found  "movie_not_found.json"
                                invalid-key "invalid_api_key.json"
                                no-key  "no_api_key.json"))

(defun mock-server--get-sample (response-type)
  (let ((file-to-load (concat (file-name-as-directory "sample_results")
                              (plist-get mock-server--result response-type))))
    (with-temp-buffer
      (insert-file-contents file-to-load)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mock-server--switch-page (api-key search)
  (comment
   (with-current-buffer (find-file "/tmp/visited.txt")
     (goto-char (point-max))
     (insert (format "%s %s\n" api-key search))
     (basic-save-buffer)
     (kill-buffer)))
     (cond
      ((not api-key) 'no-key)
      ((not (equal valid-api-key api-key)) 'invalid-key)
      ((not (equal movie-present-search search)) 'search-not-found)
      ((equal movie-present-search search) 'search-result)
      (t "unexpected condition")))

(defun mock-server--select-content (httpcon)
  (mock-server--get-sample
   (let ((api-key (elnode-http-param httpcon "apikey"))
         (search (elnode-http-param httpcon "t")))
     (mock-server--switch-page api-key search))))

(defun mock-imdb-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content Type" . "application/json"))
  (elnode-http-return httpcon (mock-server--select-content httpcon)))

(defun stop-mock-server ()
  (condition-case nil
      (elnode-stop 8080)
    (error nil)))

(defun start-mock-server (&optional cycle)
  (interactive)
  (when cycle
    (stop-mock-server))
  (elnode-start 'mock-imdb-handler :port 8080))

(provide 'mock-server)
