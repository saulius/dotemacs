(el-get 'sync '(clojure-mode))
(el-get 'sync '(cider))
(el-get 'sync '(ac-nrepl))

(require 'clojure-mode)
(require 'cider)

;; auto-complete
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

; As seen in prelude
; https://github.com/bbatsov/prelude/blob/master/modules/prelude-clojure.el

(eval-after-load 'clojure-mode
  '(progn
     (defun clojure-mode-defaults ()
       (smartparens-strict-mode +1)
       (subword-mode +1)
       (clojure-test-mode +1))

     (setq my-clojure-mode-hook 'clojure-mode-defaults)

     (add-hook 'my-clojure-mode-hook (lambda ()
                                  (run-hooks 'my-clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (defun cider-repl-mode-defaults ()
       (subword-mode +1)
       (smartparens-strict-mode +1))

     (setq cider-repl-mode-hook 'cider-repl-mode-defaults)

     (add-hook 'repl-mode-hook (lambda ()
                               (run-hooks 'cider-repl-mode-hook)))))

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
