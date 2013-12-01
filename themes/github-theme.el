;; from https://github.com/plmtznr/color-theme-github
;; github.el --- Adaption of github color theme for GNU Emacs 23

;; Copyright (C) 2013 Paul Metzner <paul.metzner@gmail.com>

;; Author: Paul Metzner Keywords: github color theme URL:
;; http://github.com/plmtznr/color-theme-github Version: 0.0.3 Package-Requires:
;; ((color-theme "6.6.1"))

;; This file is not a part of GNU Emacs.

;;; License:

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This is
;; distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Code:

;; #1E6823, #44A340, #8CC665, #D6E685, #DCDD8C

(deftheme github "Port of Github Theme for Emacs 24")

(let  ((*background-mode* 'light)
      (*border-color* "#191919")
      (*gitblack* "#191919")
      (*gitwhite* "#F7F7F7")
      (*mouse-color* "#BCD5FA")
      (*lightred* "#DD1144")
      (*darkred* "#990000")
      (*lightblue* "#0086B3")
      (*darkblue* "#445588")
      (*lightgreen* "#8CC665")
      (*darkgreen* "#44A340")
      (*lightpurple* "#445588")
      (*darkpurple* "#9C007B")
      (*lightcyan* "#C0FFFF")
      (*darkcyan* "#0080A3")
      (*gityellow* "#FDFDD7")
      (*gitorange* "#D26911")
      (*gitgrey1* "#EEEEEE")
      (*gitgrey2* "#999988")
      (*gitgrey3* "#999999")
      (*gitgrey4* "#333333")
      (*gitgrey5* "#DCDCDC"))

  (custom-theme-set-faces
   'github
      `(default ((t (:background, *gitwhite* :foreground, *gitblack*))))
      `(ac-candidate-face ((t (:background, *gitgrey1* :foreground, *gitblack*))))
      `(ac-selection-face ((t (:background, *mouse-color* :foreground, *gitblack*))))
      `(comint-highlight-prompt ((t (:background, *gitwhite* :foreground, *gitblack*))))
      `(cursor ((t (:background, *lightred*))))
      `(css-property ((t (:foreground, *lightblue*))))
      `(css-selector ((t (:foreground, *darkred*))))
      `(ecb-default-general-face ((t (:height 0.9))))
      `(ecb-default-highlight-face ((t (:background, *mouse-color* :foreground, *gitblack*))))
      `(ecb-directories-general-face ((t (:bold t :weight bold))))
      `(ecb-source-in-directories-buffer-face ((t (:foreground, *darkblue*))))
      `(erb-comment-delim-face ((t (:italic t :bold t :slant italic :foreground, *gitgrey2* :weight bold))))
      `(erb-comment-face ((t (:bold t :background, *gitgrey1* :foreground, *gitgrey2* :weight bold))))
      `(erb-delim-face ((t (:bold t :weight bold))))
      `(erb-exec-delim-face ((t (:bold t :weight bold))))
      `(erb-exec-face ((t (:background, *gitgrey1*))))
      `(erb-face ((t (:background, *gitgrey1*))))
      `(erb-out-delim-face ((t (:bold t :foreground, *darkblue* :weight bold))))
      `(erb-out-face ((t (:background, *gitgrey1*))))
      `(font-latex-bold-face ((((class color) (background light)) (:inherit bold :weight bold))) t)
      `(font-latex-math-face ((t nil)) t)
      `(font-latex-sectioning-0-face ((t (:inherit `default :weight bold :foreground, *darkblue*))) t)
      `(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-0-face))) t)
      `(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-0-face))) t)
      `(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-0-face))) t)
      `(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-0-face))) t)
      `(font-latex-sectioning-5-face ((t (:inherit font-latex-sectioning-0-face))) t)
      `(font-latex-slide-title-face ((t (:inherit font-lock-type-face :weight bold))) t)
      `(font-latex-sedate-face ((t (:inherit font-lock-type-face))) t)
      `(font-latex-verbatim-face ((t (:inherit font-lock-type-face))) t)
      `(font-latex-warning-face ((t (:foreground, *gitwhite* :background, *lightred* :bold t))))
      `(font-latex-sedate-face ((t (:foreground, *darkred*))))
      `(font-lock-builtin-face ((t (:foreground, *darkcyan*))))
      `(font-lock-comment-delimiter-face ((t (:foreground, *gitgrey2*))))
      `(font-lock-comment-face ((t (:foreground, *gitgrey2*))))
      `(font-lock-constant-face ((t (:foreground, *darkpurple*))))
      `(font-lock-doc-face ((t (:foreground, *lightred*))))
      `(font-lock-function-name-face ((t (:weight bold :foreground, *gitblack*))))
      `(font-lock-keyword-face ((t (:bold t :weight bold))))
      `(font-lock-negation-char-face ((t (:foreground, *gitorange*))))
      `(font-lock-reference-face ((t (nil))))
      `(font-lock-regexp-grouping-backslash ((t (:foreground, *lightgreen*))))
      `(font-lock-regexp-grouping-construct ((t (:foreground, *lightgreen*))))
      `(font-lock-string-face ((t (:foreground, *lightred*))))
      `(font-lock-type-face ((t (:foreground, *darkblue*))))
      `(font-lock-other-type-face ((t (:foreground, *darkblue*))))
      `(font-lock-variable-name-face ((t (:foreground, *lightblue*))))
      `(font-lock-warning-face ((t (:foreground, *gitwhite* :background, *lightred*))))
      `(font-lock-exit-face ((t (:foreground, *darkred*))))
      `(font-lock-color-constant-face ((t (:foreground, *lightgreen*))))
      `(font-lock-other-emphasized-face ((t (:bold t :weight bold :foreground, *darkpurple*))))
      `(fringe ((t (:foreground, *gitgrey2* :background, *gitwhite*))))
      `(git-commit-nonempty-second-line-face ((t (:inherit default))))
      `(hl-line ((t (:background, *gityellow* :underline t))))
      `(highlight ((t (:background, *mouse-color*))))
      `(link ((t (:foreground, *lightblue* :underline t))))
      `(link-visited ((t (:underline t :foreground, *darkpurple*))))
      `(linum ((t (:foreground, *gitgrey2* :background, *gitgrey1* :height 0.9))))
      `(message-cited-text-face ((t (:foreground, *lightblue*))))
      `(message-header-name-face ((t (:weight bold :foreground, *gitblack*))))
      `(message-header-other-face ((t (:foreground, *lightblue*))))
      `(message-header-to-face ((t (:foreground, *lightblue*))))
      `(message-header-cc-face ((t (:inherit message-header-to-face))))
      `(message-header-bcc-face ((t (:inherit message-header-to-face))))
      `(message-header-subject-face ((t (:foreground, *lightgreen*))))
      `(minibuffer-prompt ((t (:foreground, *gitblack* :weight bold))))
      `(mmm-default-submode-face  ((t (:background, *gitgrey1*))))
      `(mode-line ((t (:height 1.0
                               :background , *gitblack*
                               :foreground, *gitwhite*
                               :bold t
                               :box(:line-width 2 :color , *gitblack*)))))
      `(mode-line-inactive ((t (:height 1.0
                                        :background, *gitgrey3*
                                        :foreground, *gitgrey1*
                                        :box(:line-width 2 :color, *gitgrey3*)))))
      `(mouse ((t (:background, *mouse-color*))))
      `(mu4e-flagged-face ((t (:foreground, *lightred*))))
      `(mu4e-replied-face ((t (:italic t))))
      `(mu4e-unread-face ((t (:inherit `default :weight bold))))
      `(mu4e-header-highlight-face ((t (:inherit `highlight :weight medium :underline nil))))
      `(mu4e-title-face ((t (:foreground, *lightblue* :weight bold))))
      `(mu4e-view-header-key-face ((t (:weight bold :foreground, *gitblack*))))
      `(mu4e-view-header-marks-face ((t (:inherit `message-header-to-face))))
      `(mu4e-view-header-title-face ((t (:background, *gitgrey4* :foreground, *gitwhite*))))
      `(mu4e-view-link-face ((t (:foreground, *lightblue*))))
      `(mu4e-view-special-header-value-face ((t (:foreground, *lightblue*))))
      `(popup-tip-face ((t (:background, *gityellow* :foreground, *gitblack*))))
      `(quack-about-face ((t ())))
      `(quack-about-title-face ((t (:bold t :foreground, *lightgreen* :weight bold :height 2.0))))
      `(quack-banner-face ((t ())))
      `(quack-pltfile-dir-face ((t (:bold t :background, *gitgrey3* :foreground, *gitwhite* :weight bold :height 1.2))))
      `(quack-pltfile-file-face ((t (:bold t :background, *gitgrey2* :foreground, *gitblack* :weight bold :height 1.2))))
      `(quack-pltfile-prologue-face ((t (:background, *gitgrey2* :foreground, *gitblack*))))
      `(quack-pltish-class-defn-face ((t (:bold t :weight bold :foreground, *darkpurple*))))
      `(quack-pltish-comment-face ((t (:foreground, *darkcyan*))))
      `(quack-pltish-defn-face ((t (:bold t :foreground, *darkblue* :weight bold))))
      `(quack-pltish-keyword-face ((t (:bold t :weight bold))))
      `(quack-pltish-module-defn-face ((t (:bold t :weight bold :foreground, *darkpurple*))))
      `(quack-pltish-paren-face ((t (:foreground, *darkred*))))
      `(quack-pltish-selfeval-face ((t (:foreground, *lightgreen*))))
      `(quack-smallprint-face ((t (:height 0.8))))
      `(quack-threesemi-h1-face ((t (:bold t :weight bold :height 1.4))))
      `(quack-threesemi-h2-face ((t (:bold t :weight bold :height 1.2))))
      `(quack-threesemi-h3-face ((t (:bold t :weight bold))))
      `(quack-threesemi-semi-face ((t (:background, *lightcyan* :foreground, *darkcyan*))))
      `(quack-threesemi-text-face ((t (:background, *lightcyan* :foreground, *darkcyan*))))
      `(region ((t (:background, *mouse-color*))))
      `(sp-show-pair-match-face ((t (:background, *gitgrey1* :box nil))))
      `(sp-pair-overlay-face ((t (:background, *gitgrey1* :bold t :box nil))))
      `(trailing-whitespace ((t (:background, *lightred*))))
      `(writegood-duplicates-face ((t (:inherit `default :underline t))))
      `(writegood-passive-voice-face ((t (:inherit `writegood-duplicates-face))))
      `(writegood-weasels-face ((t (:inherit `writegood-duplicates-face))))
      ;; term
     `(term-color-black ((t (:foreground ,*gitblack* :background ,*gitblack*))))
     `(term-color-red ((t (:foreground ,*lightred* :background ,*darkred*))))
     `(term-color-green ((t (:foreground ,*lightgreen* :background ,*darkgreen*))))
     `(term-color-yellow ((t (:foreground ,*gityellow* :background ,*gityellow*))))
     `(term-color-blue ((t (:foreground ,*lightblue* :background ,*darkblue*))))
     `(term-color-magenta ((t (:foreground ,*lightpurple* :background ,*darkpurple*))))
     `(term-color-cyan ((t (:foreground ,*lightcyan* :background ,*darkcyan*))))
     `(term-color-white ((t (:foreground ,*gitwhite* :background ,*gitwhite*))))
     '(term-default-fg-color ((t (:inherit term-color-white))))
     '(term-default-bg-color ((t (:inherit term-color-black))))
   ))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'github)
