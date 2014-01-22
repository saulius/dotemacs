(el-get 'sync '(scss-mode))

(require 'scss-mode)

(setq scss-compile-at-save nil) ;; do not compile scss on save
(setq css-indent-offset 2)
(setq css-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.css\\'" . scss-mode))

(add-hook 'css-mode-hook 'rainbow-mode)
