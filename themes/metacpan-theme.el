;;; metacpan-theme.el --- Metacpan color theme for emacs 24 (deftheme)

;;; Commentary:

;; Emacs 24 color theme (deftheme) inspired by the syntax coloring used in
;; [metacpan.org](http://www.metacpan.org). This theme is by no means
;; exhaustive. It only covers the basics, .e.g.,
;; the `font-lock` family, `default`, `mode-line`, `isearch`, `ido` and other
;; major and minor modes that I personally use.

;;;  Installation:

;; Put this file under one of the directory listed in the
;; variable `custom-theme-load-path`, (if you don't know where it is,
;; just consult the emacs help system by typing `M-x describe-variable RET
;; custom-theme-load-path`)

;;; Usage:

;; To use this theme, put the following line in your `~/.emacs` file:

;;     (load-theme 'metacpan t)

;; You can disable it by invoking the command `M-x disable-theme RET metacpan
;; RET`.

;;; Screenshot

;; ![Screenshot of metacpan theme](./Screenshot.png)

(deftheme metacpan
  "Metacpan color theme")

(let ((black "#000000")
      (blue "#0000FF")
      (green "#008200")
      (red "#D71B10")
      (light-gray "#e0e0e0")
      (gray "#d9d9d9"))

  (custom-theme-set-faces
   'metacpan
   `(default ((t (:background "#fafafa" :foreground ,black))))
   `(fringe ((t (:background "#eeeeee"))))
   `(region ((t (:background "#C6DEFF" :foreground ,black))))
   `(hl-line ((t (:background "#f1f1f1"))))
   `(cursor ((t (:background ,black))))

   ;; font-lock stuff
   `(font-lock-string-face ((t (:foreground ,blue))))
   `(font-lock-comment-face ((t (:foreground ,green :italic nil))))
   `(font-lock-keyword-face ((t (:foreground "#006699" :bold nil))))
   `(font-lock-variable-name-face ((t (:foreground "#AA7700"))))
   `(font-lock-constant-face ((t (:foreground "#ff149c"))))
   `(font-lock-builtin-face ((t (:foreground "#3a235f"))))
   `(font-lock-function-name-face ((t (:foreground "#0000a2" :bold nil))))
   `(font-lock-type-face ((t (:foreground "#0000a2" :bold nil))))

   ;; ahg
   `(ahg-header-line-face ((t (:inherit 'font-lock-comment-face))))
   `(ahg-short-log-date-face ((t (:inherit 'font-lock-string-face))))
   `(ahg-short-log-user-face ((t (:inherit 'font-lock-type-face))))
   `(ahg-header-line-root-face ((t (:foreground "#cd260b"))))

   ;; magit
   `(magit-item-highlight ((t (:background "#fffab3"))))

   ;; diff
   `(diff-header ((t (:foreground ,black :background ,light-gray))))
   `(diff-file-header ((t (:foreground ,black :background
                                       ,light-gray :bold t))))
   `(diff-hunk-header ((t (:foreground ,black :background ,light-gray
                                       :bold nil :italic t))))
   `(diff-added ((t (:foreground "#333333" :background "#ddffdd"))))
   `(diff-removed ((t (:foreground "#333333" :background "#ffdddd"))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,blue))))
   `(eshell-ls-executable ((t (:foreground ,red :bold nil))))
   `(eshell-ls-directory ((t (:foreground ,blue :bold t))))

   ;; grep/compilation
   `(match ((t (:background "#ffffb1"))))
   `(compilation-info
     ((t (:background nil :foreground ,blue :bold nil :underline t))))
   `(compilation-line-number ((t (:foreground ,black :underline t))))

   ;; ido
   `(ido-first-match ((t (:bold t))))
   `(ido-only-match ((t (:foreground ,green))))
   `(ido-subdir ((t (:foreground ,blue))))

   ;; isearch
   `(isearch ((t (:background "#ff959e" :foreground ,black))))
   `(isearch-lazy-highlight-face
     ((t (:background "#c8e3ff" :foreground ,black))))


   ;; markdown, follow the org-mode mode pattern by inheriting outline-mode
   ;; (only for headings)
   `(markdown-header-face-1 ((t (:inherit outline-1))))
   `(markdown-header-face-2 ((t (:inherit outline-2))))
   `(markdown-header-face-3 ((t (:inherit outline-3))))
   `(markdown-header-face-4 ((t (:inherit outline-4))))

   `(markdown-bold-face ((t (:inherit default :weight bold))))
   `(markdown-italic-face ((t (:inherit default :italic t))))
   `(markdown-pre-face ((t (:foreground "#555555"))))
   `(markdown-blockquote-face ((t (:inherit default :italic t))))
   `(markdown-inline-code-face ((t (:inherit markdown-pre-face))))

   ;; mode-line
   `(mode-line ((t (:background "grey75" :foreground "black" :box
                                (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((t :foreground "grey20" :background "grey90"
                            :box (:line-width -1 :color "grey75" :style nil))))
   `(mode-line-buffer-id ((t (:bold t))))

   ;; paren
   `(show-paren-match ((t (:foreground "#439ba7" :background nil :bold t))))
   `(show-paren-mismatch ((t (:foreground "#bc4b4f" :background nil
                                          :bold t))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'metacpan)

;;; metacpan-theme.el ends here
