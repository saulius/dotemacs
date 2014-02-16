(el-get 'sync '(auto-complete))

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu t) ;; always show menu
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)

(add-hook 'prog-mode-hook 'auto-complete-mode)
