(require 'flymake)

(fset 'flymake-create-temp-inplace
      'flymake-create-temp-with-folder-structure)

(provide 'zoo-flymake)
