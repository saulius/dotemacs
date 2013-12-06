(el-get 'sync '(tabbar))

(require 'tabbar)

(setq tabbar-buffer-groups-function nil)

(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

(custom-set-variables
 '(tabbar-separator (quote (0.8))))
