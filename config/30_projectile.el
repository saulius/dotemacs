(el-get 'sync '(projectile))
(el-get 'sync '(grizzl))

(require 'projectile)
(require 'grizzl)

(projectile-global-mode)

(setq projectile-completion-system 'grizzl)
