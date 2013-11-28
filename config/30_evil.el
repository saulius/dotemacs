(el-get 'sync '(evil))
(el-get 'sync '(evil-leader))
(el-get 'sync '(evil-surround))
(el-get 'sync '(evil-nerd-commenter))

(require 'evil)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'surround)

(evil-mode 1)
(global-surround-mode 1)
(global-evil-leader-mode 1)

;; default leader key is ,
(setq evil-leader/leader "," evil-leader/in-all-states t)

;; keyboard shortcuts
(evil-leader/set-key
  "." 'my-switch-to-other-buffer
  "a" 'ag-project
  "A" 'ag
  "cc" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "ci" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "b" 'projectile-switch-to-buffer
  "f" 'ido-find-file
  "n" 'rename-this-buffer-and-file
  "k" 'kill-this-buffer
  "j" 'projectile-find-file
  "T" 'eshell
  "w" 'save-buffer
  "W" 'save-buffer-no-whitespace)

(eval-after-load 'evil
  '(progn
    (setq
     ;; this stops evil from overwriting the cursor color
     evil-default-cursor t
     evil-default-state 'normal
     )))
