(el-get 'sync '(evil))
(el-get 'sync '(evil-leader))
(el-get 'sync '(evil-surround))
(el-get 'sync '(evil-nerd-commenter))

(require 'evil)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'surround)

;; TODO move to better place
(defun my-switch-to-other-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun save-buffer-no-whitespace ()
  "Writes buffer with trimmed whitespace"
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer)
  )

;; from http://tuxicity.se/emacs/elisp/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(evil-mode 1)
(global-surround-mode 1)
(global-evil-leader-mode 1)

;; default leader key is ,
(setq evil-leader/leader ",")

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
