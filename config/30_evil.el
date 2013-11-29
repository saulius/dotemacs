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
  "."  'my-switch-to-other-buffer
  "a"  'ag-project
  "A"  'ag
  "cc" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "ci" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "b"  'projectile-switch-to-buffer
  "f"  'ido-find-file
  "n"  'rename-this-buffer-and-file
  "k"  'kill-this-buffer
  "j"  'projectile-find-file
  "T"  'eshell
  "xx" 'smex
  "xd" 'drag-stuff-mode
  "w"  'save-buffer
  "W"  'save-buffer-no-whitespace)

(eval-after-load 'evil
  '(progn
    (setq
     ;; this stops evil from overwriting the cursor color
     evil-default-cursor t
     evil-default-state 'normal
     )))

;; taken from https://github.com/davvil/.emacs.d/blob/64367f20a542f806b6313aa702faac3fe642ae38/init.el
;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)
