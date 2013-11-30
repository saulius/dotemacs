(el-get 'sync '(smartparens))

(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; idea and code borrowed from oh my emacs
;; https://github.com/xiaohanyu/oh-my-emacs/blob/a43b52875802a964a6d95c5f04d81536cc541885/core/ome-miscs.org

(defun create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))

(dolist (symbol '("{" "["))
  (dolist (mode '(coffee-mode js2-mode css-mode scss-mode))
      (sp-local-pair mode
                     symbol
                     nil
                     :post-handlers
                     '((create-newline-and-enter-sexp "RET")))))
