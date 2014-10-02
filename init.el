;; get rid of menu, tool and scrollbars early on
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; hide startup splash screen
(setq inhibit-startup-screen t)

;; debug
; (setq debug-on-error t)

(when (version< emacs-version "24.0")
  (error "GNU Emacs 24 is needed, current version %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

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

(org-babel-load-file (expand-file-name "tao-boot.org" user-emacs-directory))
