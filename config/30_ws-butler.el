(el-get 'sync '(ws-butler))

(require 'ws-butler)

(add-hook 'prog-mode-hook 'ws-butler-mode)
