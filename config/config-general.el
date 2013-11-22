;; TODO move to better place
;; Syntactic sugar for eval-after-load dance.
;; https://github.com/purcell/emacs.d/blob/aa789c9745b13612c4fea6e638d81d8ebbfecdf8/init-utils.el#L1-L5
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(require 'whitespace)
(after-load "whitespace" '(setq whitespace-style (delq 'spaces whitespace-style)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; (require 'powerline)
; (powerline-default-theme)

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
