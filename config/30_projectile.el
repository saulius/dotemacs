(el-get 'sync '(projectile))
(el-get 'sync '(projectile-rails))

(require 'projectile)

(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-ignored-file-extensions '("class" "o" "so" "elc" "jar" "png" "jpg" "jpeg" "gif"))

(add-hook 'projectile-mode-hook 'projectile-rails-on)
