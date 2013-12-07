(require 'whitespace)

(setq whitespace-line-column 80
            whitespace-style '(face tabs empty trailing lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
