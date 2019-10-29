(require 'buttercup)
(setq load-path (cons "." load-path))
(require 'url-utils)
(require 'mock-server)

(describe "url-utils.el"
  (describe "url-utils-get-json-url-content"
    (it "downloads a json correctly"
      (expect (with-debug-server
                (url-utils-get-json-url-content "http://localhost:8080/anything/?apikey=wrongkey"))
             :to-equal '((Response . "False") (Error . "Invalid API key!"))))))
