(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'test-utils)
(require 'cached-url-utils)
(require 'mock-server)

(defun create-empty-file (name)
  (with-temp-buffer
    (write-file (concat (file-name-as-directory "cache") name))))

(describe "cached-url-utils.el"
  :var (temp-file)
  (before-each
    (setq temp-file (make-temp-name "test_saved_image_")))
  (after-each
    (safe-delete-file temp-file))
  (after-all
    (url-utils-cached-clear))
  (describe "url-utils-cached-clear"
    (it "removes all files from the \"cached\" folder"
      (create-empty-file "foo")
      (create-empty-file "bar")
      (create-empty-file "baz")      
      (expect (file-exists-p (concat (file-name-as-directory "cache") "foo")))
      (expect (file-exists-p (concat (file-name-as-directory "cache") "bar")))
      (expect (file-exists-p (concat (file-name-as-directory "cache") "baz")))
      (url-utils-cached-clear)
      (expect (not (file-exists-p (concat (file-name-as-directory "cache") "foo"))))
      (expect (not (file-exists-p (concat (file-name-as-directory "cache") "bar"))))
      (expect (not (file-exists-p (concat (file-name-as-directory "cache") "baz"))))))
  (describe "url-utils-cached-get-json-url-content"
    (it "downloads a json correctly"
      (url-utils-cached-clear)      
      (expect (with-debug-server
               (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey"))
              :to-equal '((Response . "False") (Error . "Invalid API key!"))))
    (it "does not download the same request twice"
      (url-utils-cached-clear)
      (with-debug-server
       (spy-on 'url-retrieve-synchronously :and-call-through)
       (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=API-KEY&t=jurassic+park")
       (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=API-KEY&t=jurassic+park")
       (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=API-KEY&t=jurassic+park"))
      (expect  'url-retrieve-synchronously
               :to-have-been-called-times 1))
    (it "returns the same json after subsequent calls"
      (with-debug-server 
       (url-utils-cached-clear)
       (expect (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=API-KEY&t=jurassic+park")
               :to-equal (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=API-KEY&t=jurassic+park"))))))
