(el-get 'sync '(emux))

(setq emux-term-program "/bin/zsh")

(require 'emux-session)

(add-hook 'term-mode-hook
          '(lambda ()
             (linum-mode 0)))
