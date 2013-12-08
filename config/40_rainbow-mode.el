(el-get 'sync '(rainbow-mode))

(require 'rainbow-mode)

(define-globalized-minor-mode global-rainbow-mode rainbow-mode (lambda () (rainbow-mode t)))
(add-hook 'prog-mode-hook 'rainbow-mode)
