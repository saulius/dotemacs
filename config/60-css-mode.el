(el-get 'sync '(css-mode))

(require 'css-mode)

(setq css-indent-offset 2)
(setq css-indent-level 2)

;; adjust evil shift width per language
(add-hook 'css-mode-hook
  (function (lambda ()
          (setq evil-shift-width css-indent-level))))
