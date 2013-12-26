(el-get 'sync '(clojure-mode))
(el-get 'sync '(cider))

(require 'clojure-mode)
(require 'cider)

; As seen in prelude
; https://github.com/bbatsov/prelude/blob/master/modules/prelude-clojure.el

(eval-after-load 'clojure-mode
  '(progn
     (defun clojure-mode-defaults ()
       (smartparens-strict-mode +1)
       (subword-mode +1)
       (clojure-test-mode +1))

     (setq clojure-mode-hook 'clojure-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
                                  (run-hooks 'clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (defun cider-repl-mode-defaults ()
       (subword-mode +1)
       (smartparens-strict-mode +1))

     (setq cider-repl-mode-hook 'cider-repl-mode-defaults)

     (add-hook 'repl-mode-hook (lambda ()
                               (run-hooks 'cider-repl-mode-hook)))))
