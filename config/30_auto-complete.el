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

(dolist (mode '(prog-mode cider-repl-mode org-mode))
  (add-to-list 'ac-modes mode))

(global-auto-complete-mode t)
