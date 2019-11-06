(require 's)
(setq load-path (cons "." load-path))
(require 'url-utils)

(defconst cached-url-utils--cache-folder "cache")
(defconst cached-url-utils--images-folder (concat (file-name-as-directory cached-url-utils--cache-folder) "images"))

(defun cached-url-utils--dir-contents (directory)
  (seq-filter 'file-regular-p
              (mapcar (lambda (x)
                        (concat (file-name-as-directory directory) x))
                      (directory-files directory))))

(defun cached-url-utils--cache-files ()
  (cached-url-utils--dir-contents cached-url-utils--cache-folder))

(defun cached-url-utils--cached-image-files ()
  (seq-filter (lambda (x) (s-ends-with? ".jpg" x))
              (cached-url-utils--dir-contents cached-url-utils--images-folder)))

(defun url-utils-cached-clear ()
  (interactive)
  (mapc (lambda (x)
          (delete-file x))
        (append (cached-url-utils--cache-files)
                (cached-url-utils--cached-image-files))))

(defun cached-url-utils--url-to-hash (url)
  (md5 url))

(defun cached-url-utils--read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cached-url-utils--get-path (url)
  (concat (file-name-as-directory cached-url-utils--cache-folder)
          (cached-url-utils--url-to-hash url)))

(defun cached-url-utils--get-cached (url)
  (let ((cached-file-path (cached-url-utils--get-path url)))
    (if (file-exists-p cached-file-path)
        (json-read-from-string (cached-url-utils--read-file cached-file-path)))))

(defun cached-url-utils--write-file (path content)
  (with-temp-buffer
    (insert content)
    (write-file path)))

(defun cached-url-utils--cache-url (url content)
  (let ((cached-file-path (cached-url-utils--get-path url)))
    (unless (file-exists-p cached-file-path)
      (cached-url-utils--write-file cached-file-path (json-encode content)))
    content))

(defun url-utils-cached-get-json-url-content (url)
  (cached-url-utils--cache-url url
                               (or (cached-url-utils--get-cached url)
                                   (url-utils-get-json-url-content url))))

(defun url-utils-cached-get-image (url &optional image-downloader)
  (let ((image-downloader (or image-downloader 'url-utils-download-image))
        (image-path (concat (file-name-as-directory cached-url-utils--images-folder) (cached-url-utils--url-to-hash url) ".jpg")))
    (if (not (file-exists-p image-path))
        (apply image-downloader (list url image-path)))
    image-path))

(provide 'cached-url-utils)
