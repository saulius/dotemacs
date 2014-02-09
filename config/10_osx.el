;; adapted from https://github.com/bradleywright/emacs.d/blob/0e9739f8c445c2b57fb59ddfdd56a4910d910b6a/init.el
;; OSX specific overrides

(when (eq system-type 'darwin)
  (when (display-graphic-p)
    ;; This makes left-option do M-
    (setq ns-alternate-modifier 'meta)
    (setq ns-right-alternate-modifier nil)
    (setq ns-command-modifier 'super)
    (setq ns-function-modifier 'hyper)
    (setq ns-use-srgb-colorspace t)

    (when (fboundp 'toggle-frame-fullscreen)
      (global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)))

  (unless (display-graphic-p)
    ;; Make sure cut/paste works properly. Gotten from:
    ;; http://mindlev.wordpress.com/2011/06/13/emacs-in-a-terminal-on-osx/#comment-20
    (defun copy-from-osx ()
      "Copies the current clipboard content using the `pbcopy` command"
      (shell-command-to-string "pbpaste"))

    (defun paste-to-osx (text &optional push)
      "Copies the top of the kill ring stack to the OSX clipboard"
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    ;; Override defaults to use the mac copy and paste
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))

