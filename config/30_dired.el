(el-get 'sync '(dired+))

; do not hide dired details by default
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)

(add-hook 'dired-load-hook
           (lambda ()
             ;; Bind dired-x-find-file.
             (setq dired-x-hands-off-my-keys nil)
             (load "dired-x")
             ))
