(el-get 'sync '(emux))

(setq emux-term-program "/bin/zsh")
(setq emux-default-session "emux")

(require 'emux-session)

(add-hook 'term-mode-hook
          '(lambda ()
             (interactive)
             (set-window-dedicated-p (selected-window) 1)
             (linum-mode 0)))
