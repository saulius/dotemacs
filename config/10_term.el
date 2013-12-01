;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

(require 'term)

;; from emacs-live https://github.com/overtone/emacs-live/blob/ffb8f52f20203692c75ee53fe0b319a636a7b9a7/packs/dev/foundation-pack/config/shell-conf.el
;;make sure ansi colour character escapes are honoured
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

;; kill buffer and delete window when terminal process is killed
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer)
        (delete-window) )
    ad-do-it))
(ad-activate 'term-sentinel)

(defun term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'term-use-utf8)

;; Adapted from prelude https://github.com/bbatsov/prelude/blob/b9000702b2ac8216a8bfeea645fde6bb0c1fc7bc/core/prelude-core.el
(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (start-or-switch-to (lambda ()
                         (ansi-term (getenv "SHELL")))
                      "*ansi-term*"))
