;;; -*- lexical-binding: t -*-

(defmacro my-lexical-binding-q ()
  "Copied from here: 'https://yoo2080.wordpress.com/2011/12/30/how-to-check-dynamically-if-lexical-scoping-is-active-in-emacs-lisp/'"
  (let ((tempvar (make-symbol "x")))
    `(let ((,tempvar nil)
           (f (let ((,tempvar t)) (lambda () ,tempvar))))
       (funcall f))))

(assert (my-lexical-binding-q))

(defvar samples-dir "sample_results")

 (defun mock-imdb-handler (httpcon)
   (elnode-http-start httpcon 200 '("Content Type" . "application/json"))
   (elnode-http-return httpcon "not really a json…"))

(defun start-mock-server ()
  (elnode-start 'mock-imdb-handler :port 8080))

(defun stop-mock-server ()
  (condition-case nil
      (elnode-stop 8080)
    (error nil)))
