(require 'em-glob)

(defun zoo.path/normalize-path (filepath)
  (if (string-match "/$" filepath)
    (replace-regexp-in-string "/$" "" filepath)
    filepath))

(defun zoo.path/split-path (filepath)
  (let ((path-tokens (split-string filepath "/")))
    (if (string= (car path-tokens) "")
      (cdr path-tokens)
      path-tokens)))

(defun zoo.path/join-path (path-tokens)
  (concat "/"
          (mapconcat 'identity
                     (mapcar #'zoo.path/normalize-path path-tokens)
                     "/")))

(defun zoo.path/parent-folder (filepath)
  (let ((path-tokens (zoo.path/split-path (expand-file-name filepath))))
    (if (null path-tokens)
      "/"
      (zoo.path/join-path (butlast path-tokens)))))

(defun zoo.path/rfind-file (glob dir)
  (let ((file-found (directory-files dir
                                     nil
                                     (eshell-glob-regexp glob))))
    (cond
     (file-found (car file-found))
     ((not (string= dir "/"))
      (zoo.path/rfind-file glob (zoo.path/parent-folder dir)))
     (t nil))))

(defun zoo.path/rfind-dir (glob dir)
  (let ((file-found (directory-files dir
                                     nil
                                     (eshell-glob-regexp glob))))
    (cond
     (file-found
      (if (string-match "/$" dir)
        dir
        (concat dir "/")))
     ((not (string= dir "/"))
      (zoo.path/rfind-dir glob (zoo.path/parent-folder dir)))
     (t nil))))

(defun zoo.path/pwd ()
  (file-name-directory (or load-file-name
                           buffer-file-name)))

(provide 'zoo.path)