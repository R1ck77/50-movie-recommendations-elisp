(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'cached-url-utils)
(require 'mock-server)

;;; TODO/FIXME duplicated
(defmacro comment (&rest forms))

;;; TODO/FIXME duplicated
(defun safe-delete-file (path)
  (condition-case nil
      (delete-file path)
    (error nil)))

;;; TODO/FIXME duplicated
(defun get-file-size (path)
  (elt (file-attributes path) 7))

(describe "cached-url-utils.el"
  :var (temp-file)
  (before-each
    (setq temp-file (make-temp-name "test_saved_image_")))
  (after-each
    (safe-delete-file temp-file))    
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
       (url-utils-cached-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey"))
      (expect  'url-retrieve-synchronously
               :to-be-called- 1 ))))
