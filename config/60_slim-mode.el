(el-get 'sync '(slim-mode))

(require 'slim-mode)

(defun slim-custom ()
  "slim-mode-hook"
  (local-set-key "\t" 'insert-two-spaces)
  (set (make-local-variable 'tab-width) 2)
  (setq evil-shift-width tab-width))

(add-hook 'slim-mode-hook
  '(lambda() (slim-custom)))
