(add-to-list 'load-path "~/.emacs.d/")

;; Turn off graphical interface early on, console person
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off splash screen
(setq inhibit-startup-screen t)

;; Initialize Cask, the package manager
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/config")

(load "config-general.el")
(load "config-evil.el")
(load "config-theme.el")
(load "config-projectile.el")
