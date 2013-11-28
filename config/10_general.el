(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;; set default font
(set-default-font "Bitstream Vera Sans Mono for Powerline 16")
;;; set default font for emacs --daemon / emacsclient
(setq default-frame-alist '((font . "Bitstream Vera Sans Mono for Powerline 16")))

(add-to-list 'default-frame-alist '(background-color . "#FFFFFF"))
(add-to-list 'default-frame-alist '(foreground-color . "#000000"))

;; Disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable scroll bar
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; UTF-8 all the things
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Time is useful to see when coding
(display-time)

;; disable backup file
(setq make-backup-files nil)

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
