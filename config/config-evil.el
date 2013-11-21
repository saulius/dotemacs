(require 'evil)
(require 'evil-leader)
(require 'surround)

(evil-mode 1)
(global-surround-mode 1)
(global-evil-leader-mode 1)

;; keyboard shortcuts
(evil-leader/set-key
  "j" 'projectile-find-file)
