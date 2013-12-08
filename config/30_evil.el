(el-get 'sync '(evil))
(el-get 'sync '(evil-leader))
(el-get 'sync '(evil-surround))
(el-get 'sync '(evil-nerd-commenter))

(require 'evil)
(require 'evil-leader)

(setq evilnc-hotkey-comment-operator "\\")
(require 'evil-nerd-commenter)
(require 'surround)

(global-evil-leader-mode 1) ;; enable before evil-mode to work in all modes
(evil-mode 1)
(global-surround-mode 1)

;; default leader key is ,
(setq evil-leader/leader "," evil-leader/in-all-states t)

;; keyboard shortcuts
(evil-leader/set-key
  ","  'evil-buffer
  "a"  'ag-project
  "A"  'ag
  "cc" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "ci" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "b"  'projectile-switch-to-buffer
  "e"  'ido-find-file
  "n"  'rename-this-buffer-and-file
  "k"  'kill-this-buffer-and-window
  "K"  'kill-this-buffer
  "j"  'projectile-find-file
  "r"  'rename-buffer
  "t"  'emux-term-create
  "T"  'eshell
  "xx" 'smex
  "xd" 'drag-stuff-mode
  "xl" 'linum-mode
  "xw" 'whitespace-mode
  "w"  'save-buffer
  "W"  'save-buffer-no-whitespace)

(eval-after-load 'evil
  '(progn
    (setq
     ;; this stops evil from overwriting the cursor color
     evil-default-cursor t
     evil-default-state 'normal
     ;; Don't wait for any other keys after escape is pressed.
     evil-esc-delay 0
     )))

;; I comment with \\ in visual mode
(define-key evil-motion-state-map "\\" nil)
(define-key evil-normal-state-map "\\" nil)


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

;; evil moving around windows
(define-key evil-normal-state-map (kbd "C-w <left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w <right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w <up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w <down>") 'evil-window-down)

;; additional evil commands
(evil-ex-define-cmd "Ex[plore]" 'dired-jump)
