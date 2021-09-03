(require 'builtins)
(require 'packages)

(defun config/entry ()
  "Load config"
  (builtins/entry)
  (packages/entry))

(provide 'config)
