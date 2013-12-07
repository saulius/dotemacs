(el-get 'sync '(tabbar))

;; from http://www.emacswiki.org/emacs/TabBarMode
(when (require 'tabbar nil t)
   ;; Enable tabbars globally:
   (tabbar-mode 1)
   ;; I use this minor-mode mainly as a global mode (see below):
   (define-minor-mode tabbar-on-term-only-mode
     "Display tabbar on terminals and buffers in fundamental mode only."
     :init-value t
     :lighter nil
     :keymap nil
     (if tabbar-on-term-only-mode
         ;; filter is enabled
         (if (eq major-mode 'term-mode); <- this can be easily customizable...
             (tabbar-local-mode -1)
           (tabbar-local-mode 1))
       ;; always activate tabbar locally when we disable the minor mode:
       (tabbar-local-mode -1)))
   (defun tabbar-on-term-only-mode-on ()
     "Turn on tabbar if current buffer is a terminal."
     (unless (minibufferp) (tabbar-on-term-only-mode 1)))
   ;; Define a global switch for the mode. Note that this is not set for buffers
   ;; in fundamental mode.
   ;;
   ;; I use it 'cause some major modes do not run the
   ;; `after-change-major-mode-hook'...
   (define-globalized-minor-mode global-tabbar-on-term-only-mode
     tabbar-on-term-only-mode tabbar-on-term-only-mode-on)

;; Eventually, switch on this global filter for tabbars:
(global-tabbar-on-term-only-mode 1))

(setq tabbar-buffer-groups-function nil)

(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

(custom-set-variables
 '(tabbar-separator (quote (0.8))))

;; add separator spaces between tabs
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value (concat "  " (concat ad-return-value "  "))))
