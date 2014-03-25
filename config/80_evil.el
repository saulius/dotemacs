(setq evilnc-hotkey-comment-operator "\\")

(el-get 'sync '(evil))
(el-get 'sync '(evil-leader))
(el-get 'sync '(evil-surround))
(el-get 'sync '(evil-nerd-commenter))
(el-get 'sync '(evil-org))
(el-get 'sync '(lispy))
(el-get 'sync '(evil-lispy))

(require 'evil)
(require 'evil-leader)
(require 'evil-org)

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
  "b"  'switch-to-buffer
  "e"  'ido-find-file
  "g"  'magit-status
  "n"  'rename-this-buffer-and-file
  "k"  'kill-this-buffer-and-window
  "K"  'kill-this-buffer
  "f"  'projectile-find-file
  "j" 'ace-jump-mode
  "J" 'ace-jump-word-mode
  "oa" 'visit-org-agenda-files
  "ot" 'open-org-global-todo
  "oi" 'org-clock-in
  "oo" 'org-clock-out
  "od" 'org-clock-display
  "oe" 'org-set-effort
  "om" 'org-clock-modify-effort-estimate
  "op" 'org-pomodoro
  "r"  'rename-buffer
  "t"  'emux-term-create
  "T"  'eshell
  "xx" 'smex
  "xd" 'drag-stuff-mode
  "xl" 'linum-mode
  "xw" 'whitespace-mode
  "xp" 'projectile-switch-project
  "xi" 'projectile-invalidate-cache
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

;; from https://gist.github.com/cofi/4963125
;; AceJump integration is now included in evil, this gist is only preserved for historical reasons.
;; Please use the provided integration (it's far more advanced)

;; AceJump is a nice addition to evil's standard motions.

;; The following definitions are necessary to define evil motions for ace-jump-mode (version 2).

;; ace-jump is actually a series of commands which makes handling by evil
;; difficult (and with some other things as well), using this macro we let it
;; appear as one.

(defmacro evil-enclose-ace-jump (&rest body)
  `(let ((old-mark (mark))
         (ace-jump-mode-scope 'window))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (progn
           ,@body
           (recursive-edit))
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (evil-enclose-ace-jump
   (ace-jump-mode 9)))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 1)))

(evil-define-motion evil-ace-jump-char-to-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)
   (forward-char -1)))

(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

;; some proposals for binding:

(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)

(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-char-mode)      ; similar to f
(define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;; expand-region integration
(define-key evil-normal-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "_") 'er/contract-region)

; (add-to-list 'evil-emacs-state-modes 'cider-repl-mode)

(evil-leader/set-key-for-mode 'clojure-mode
                              "mrf" 'cljr-rename-file
                              "mru" 'cljr-replace-use
                              "mau" 'cljr-add-use-to-ns
                              "mar" 'cljr-add-require-to-ns
                              "mai" 'cljr-add-import-to-ns
                              "msn" 'cljr-sort-ns
                              "msr" 'cljr-stop-referring
                              "mth" 'cljr-thread
                              "muw" 'cljr-unwind
                              "mua" 'cljr-unwind-all
                              "mil" 'cljr-introduce-let
                              "mel" 'cljr-expand-let
                              "mml" 'cljr-move-to-let
                              "mmf" 'cljr-move-form
                              "mtf" 'cljr-thread-first-all
                              "mtl" 'cljr-thread-last-all
                              "mcp" 'cljr-cycle-privacy
                              "mcc" 'cljr-cycle-coll
                              "mcs" 'cljr-cycle-stringlike
                              "mad" 'cljr-add-declaration
                              "mdk" 'cljr-destructure-keys)

;; Enter evil-lispy state with Meta-Enter
(require 'evil-lispy)

(global-set-key (kbd "M-<RET>") 'evil-lispy-state)

;; make % jump between ruby block items
(evil-define-motion evil-ruby-jump-item (count)
  :jump t
  :type inclusive
  (cond ((string-match ruby-block-beg-re (current-word))
         (ruby-end-of-block count))
        ((string-match ruby-block-end-re (current-word))
         (ruby-beginning-of-block count))
        (t
         (evil-jump-item count))))
