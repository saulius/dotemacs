(el-get 'sync '(clojure-mode))
(el-get 'sync '(cider))
(el-get 'sync '(ac-nrepl))
(el-get 'sync '(align-cljlet))
(el-get 'sync '(clj-refactor))
(el-get 'sync '(paredit))
(el-get 'sync '(multiple-cursors))

(require 'clojure-mode)
(require 'cider)
(require 'clj-refactor)

(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces nil)
(setq cider-repl-pop-to-buffer-on-connect t)

;; auto-complete
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)

; As seen in prelude
; https://github.com/bbatsov/prelude/blob/master/modules/prelude-clojure.el

(eval-after-load 'clojure-mode
  '(progn
     (defun clojure-mode-defaults ()
       (smartparens-strict-mode +1)
       (subword-mode +1)
       (clj-refactor-mode 1)
       (clojure-test-mode +1))

     (setq my-clojure-mode-hook 'clojure-mode-defaults)

     (add-hook 'my-clojure-mode-hook (lambda ()
                                  (run-hooks 'my-clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (defun cider-repl-mode-defaults ()
       (subword-mode +1)
       (smartparens-strict-mode +1)
       (cider-repl-toggle-pretty-printing))

     (setq cider-repl-mode-hook 'cider-repl-mode-defaults)
     ;; Prevent C-c C-k from prompting to save the file corresponding to the buffer being loaded, if it's modified:
     (setq cider-prompt-save-file-on-load nil)
     (setq cider-show-error-buffer 'only-in-repl)

     (add-hook 'repl-mode-hook (lambda ()
                               (run-hooks 'cider-repl-mode-hook)))))

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; Use ac-nrepl-popup-doc to show in-line docs in an nrepl buffer
(eval-after-load "cider"
  '(define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
