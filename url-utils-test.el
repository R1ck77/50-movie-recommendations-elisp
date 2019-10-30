(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'url-utils)
(require 'mock-server)

(defmacro comment (&rest forms))

(defun safe-delete-file (path)
  (condition-case nil
      (delete-file path)
    (error nil)))

(defun get-file-size (path)
  (elt (file-attributes path) 7))

(describe "url-utils.el"
  :var (temp-file)
  (before-each
    (setq temp-file (make-temp-name "test_saved_image_")))
  (after-each
    (safe-delete-file temp-file))    
  (describe "url-utils-get-json-url-content"
    (it "downloads a json correctly"
      (expect (with-debug-server
                (url-utils-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey"))
              :to-equal '((Response . "False") (Error . "Invalid API key!")))))
  (describe "url-utils-download-image"
    (it "downloads an image in the selected file"
      (with-debug-server
        (url-utils-download-image "http://localhost:8080/jurassic.jpg" temp-file))
      (expect (get-file-size temp-file)
              :to-be 20309)))
  (describe "url-utils-download-image returns the path of the file on success"
    (it "downloads an image in the selected file"
      (with-debug-server       
        (expect (url-utils-download-image "http://localhost:8080/jurassic.jpg" temp-file)
                :to-equal temp-file)))))
