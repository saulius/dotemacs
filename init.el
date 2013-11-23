(setq init-dir "~/.emacs.d/")

(add-to-list 'load-path (concat init-dir "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync '(el-get))
(add-to-list 'el-get-recipe-path (concat init-dir "elget-recipes"))

;; init
(el-get 'sync '(init-loader))
(require 'init-loader)
(init-loader-load (concat init-dir "config"))
