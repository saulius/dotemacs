(el-get 'sync '(coffee-mode))

(require 'coffee-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2)
 (setq evil-shift-width tab-width))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))
