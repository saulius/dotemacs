(el-get 'sync '(projectile))
(el-get 'sync '(projectile-rails))

(require 'projectile)

(projectile-global-mode)

(setq projectile-enable-caching t)
(setq projectile-project-ignored-files '("*.class" "*.o" "*.so" "*.elc" "*.jar" "*.png" "*.jpg" "*.jpeg" "*.gif"))
(setq projectile-globally-ignored-directories (append '("target") projectile-globally-ignored-directories))
(setq projectile-require-project-root nil) ;; any directory can be a project
(setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)

(add-hook 'projectile-mode-hook 'projectile-rails-on)
