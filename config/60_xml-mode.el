(require 'nxml-mode)

;; from prelude  https://github.com/bbatsov/prelude/blob/02b3f27e65ae6e213c8046d3196eacb24630600f/modules/prelude-xml.el
(push '("<\\?xml" . nxml-mode) magic-mode-alist)

;; following files should be treated as xml files
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.atom$" . nxml-mode))

(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(setq nxml-auto-insert-xml-declaration-flag nil)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)
