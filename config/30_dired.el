(el-get 'sync '(dired+))

(require 'diredp)

(add-hook 'dired-load-hook
           (lambda ()
             ;; Bind dired-x-find-file.
             (setq dired-x-hands-off-my-keys nil)
             (load "dired-x")
             ))
