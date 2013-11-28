(el-get 'sync '(powerline))

(require 'powerline)

;; Disables 3d style
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(powerline-center-evil-theme)
