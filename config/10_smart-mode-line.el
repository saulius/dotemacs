(el-get 'sync '(smart-mode-line))

(require 'smart-mode-line)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 40)
(setq sml/mode-width 'full)
(sml/setup)
(sml/apply-theme 'light)
