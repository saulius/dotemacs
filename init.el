;; get rid of menu, tool and scrollbars early on
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; hide startup splash screen
(setq inhibit-startup-screen t)

;;debug
(setq debug-on-error t)

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; shallow clone saves bandwidth
(setq el-get-git-shallow-clone t)

(el-get 'sync '(el-get org-mode))

;; store custom recipes in k-elget-recipes
(add-to-list 'el-get-recipe-path (concat user-emacs-directory "tao-elget-recipes"))

(org-babel-load-file (expand-file-name "tao-boot.org" user-emacs-directory))
