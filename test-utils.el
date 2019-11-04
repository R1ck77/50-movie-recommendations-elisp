
(defmacro comment (&rest forms))

(defun safe-delete-file (path)
  (condition-case nil
      (delete-file path)
    (error nil)))

(defun get-file-size (path)
  (elt (file-attributes path) 7))

(provide 'test-utils)
