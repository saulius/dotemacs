(el-get 'sync '(projectile))
(el-get 'sync '(grizzl))
(el-get 'sync '(projectile-rails))

(require 'projectile)
(require 'grizzl)

(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)

(add-hook 'projectile-mode-hook 'projectile-rails-on)
