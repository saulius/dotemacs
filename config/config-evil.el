(require 'evil)
(require 'evil-leader)
(require 'surround)

;; Syntactic sugar for eval-after-load dance.
;; https://github.com/purcell/emacs.d/blob/aa789c9745b13612c4fea6e638d81d8ebbfecdf8/init-utils.el#L1-L5

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(evil-mode 1)
(global-surround-mode 1)
(global-evil-leader-mode 1)

;; default leader key is ,
(setq evil-leader/leader ",")

;; keyboard shortcuts
(evil-leader/set-key
  "j" 'projectile-find-file)

(after-load 'evil
  (setq
   ;; this stops evil from overwriting the cursor color
   evil-default-cursor t
   evil-default-state 'normal
   )
)
