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
  (let ((path-tokens (zoo.path/split-path filepath)))
    (if (null path-tokens)
      "/"
      (zoo.path/join-path (butlast path-tokens)))))

(provide 'zoo.path)