(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'test-utils)
(require 'cached-url-utils)
(require 'mock-server)

(describe "cached-url-utils.el"
  :var (temp-file)
  (before-each
    (setq temp-file (make-temp-name "test_saved_image_")))
  (after-each
    (safe-delete-file temp-file))
  (describe "url-utils-cached-clear"
    (it "removes all files from the \"cached\" folder"
      (with-temp-buffer        
        (write-file (concat (file-name-as-directory "cache") "foo"))
        (write-file (concat (file-name-as-directory "cache") "bar"))
        (write-file (concat (file-name-as-directory "cache") "baz")))
      (expect (file-exists-p (concat (file-name-as-directory "cache") "foo")))
      (url-utils-cached-clear)
      (expect (not (file-exists-p (concat (file-name-as-directory "cache") "foo"))))
      (expect (not (file-exists-p (concat (file-name-as-directory "cache") "bar"))))
      (expect (not (file-exists-p (concat (file-name-as-directory "cache") "baz"))))))
  (describe "url-utils-cached-get-json-url-content"
    (it "downloads a json correctly"
      (expect (with-debug-server
                (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey"))
              :to-equal '((Response . "False") (Error . "Invalid API key!")))))
  (describe "url-utils-cached-get-json-url-content"
    (it "does not download the same request twice"
      (with-debug-server
       (spy-on 'url-retrieve-synchronously :and-call-through)
       (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey")
       (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey")       
       (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey"))
      (expect  'url-retrieve-synchronously
               :to-have-been-called-times 1))))
