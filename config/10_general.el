(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;; set default font
(set-default-font "Inconsolata 16")
;;; set default font for emacs --daemon / emacsclient
(setq default-frame-alist '((font . "Inconsolata 16")))

(add-to-list 'default-frame-alist '(background-color . "#FFFFFF"))
(add-to-list 'default-frame-alist '(foreground-color . "#000000"))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; UTF-8 all the things
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Time is useful to see when coding
(display-time)

;; from http://whattheemacsd.com/init.el-02.html
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; delete auto save file
(setq delete-auto-save-files t)

;; disable auto save
(setq auto-save-default nil)

;; yes/no => y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; truncate lines
(setq-default truncate-lines t)

;; [Home] & [End] key should take you to beginning and end of lines..
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;; from http://whattheemacsd.com/init.el-03.html
;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; don't use tabs to indent
(setq-default indent-tabs-mode nil)

;; only in emacs 24.4+
(electric-indent-mode +1)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)

;; visual, some spacing between lines
(setq-default line-spacing 2)

;; Disable fringes
(fringe-mode 0)

;; Enable line numbers
(setq linum-format "%4d ")

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Tab width defaults to 2 (may differ per language)
(setq tab-width 2)
