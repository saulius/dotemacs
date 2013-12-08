(el-get 'sync '(dired+))

(add-hook 'dired-load-hook
           (lambda ()
             ;; Bind dired-x-find-file.
             (setq dired-x-hands-off-my-keys nil)
             (load "dired-x")
             ))
