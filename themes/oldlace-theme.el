;;; oldlace-theme.el --- Emacs 24 theme with an 'oldlace' background.

;; Copyright (C) 2014 , martin haesler

;; Author: martin haesler
;; https://github.com/mswift42/oldlace-theme.git
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Created with emacs-theme-generator, https://github.com/mswift42/theme-creator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see &lt;http://www.gnu.org/licenses/&gt;.

;; This file is not part of Emacs.

;;; Commentary: light, low-contrast theme with background in oldlace color.

;;; Code:

(deftheme oldlace)
 (let ((class '((class color) (min-colors 89)))
       (fg1 "#525252")
       (fg2 "#5e5e5e")
       (fg3 "#6b6b6b")
       (fg4 "#787878")
       (bg1 "#fdf5e6")
       (bg2 "#e6dfd1")
       (bg3 "#d0c9bd")
       (bg4 "#bab4a9")
       (key2 "#546789")
       (key3 "#394d6d")
       (builtin "#7b4135")
       (keyword "#3f567b")
       (const   "#64502f")
       (comment "#949494")
       (func    "#714355")
       (str     "#305f5e")
       (type    "#634575")
       (var     "#3f5b32")
       (warning "#fa0c0c"))
   (custom-theme-set-faces
   'oldlace
        `(default ((,class (:background ,bg1 :foreground ,fg1))))
        `(font-lock-builtin-face ((,class (:foreground ,builtin))))
        `(font-lock-comment-face ((,class (:foreground ,comment))))
        `(font-lock-negation-char-face ((,class (:foreground ,const))))
        `(font-lock-reference-face ((,class (:foreground ,const))))
        `(font-lock-constant-face ((,class (:foreground ,const))))
        `(font-lock-doc-face ((,class (:foreground ,comment))))
        `(font-lock-function-name-face ((,class (:foreground ,func :bold t))))
        `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
        `(font-lock-string-face ((,class (:foreground ,str))))
        `(font-lock-type-face ((,class (:foreground ,type ))))
        `(font-lock-variable-name-face ((,class (:foreground ,var))))
        `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
        `(region ((,class (:background ,fg1 :foreground ,bg1))))
        `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
        `(hl-line ((,class (:background  ,bg2))))
        `(fringe ((,class (:background ,bg2 :foreground ,fg4))))
        `(cursor ((,class (:background ,bg3))))
        `(show-paren-match-face ((,class (:background ,warning))))
        `(isearch ((,class (:bold t :foreground ,warning :background ,bg3))))
        `(mode-line ((,class (:box (:line-width 1 :color nil :style released-button) :bold t :foreground ,fg4 :background ,bg2))))
        `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,fg2 :background ,bg1))))
        `(mode-line-buffer-id ((,class (:bold t :foreground ,fg2 :background nil))))
        `(mode-line-highlight ((,class (:background ,bg4))))
        `(vertical-border ((,class (:foreground ,fg3))))
        `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
        `(default-italic ((,class (:italic t))))
        `(link ((,class (:foreground ,const :underline t))))
        `(org-code ((,class (:foreground ,fg2))))
        `(org-hide ((,class (:foreground ,fg4))))
        `(org-level-1 ((,class (:bold t :foreground ,fg2 :height 1.1))))
        `(org-level-2 ((,class (:bold nil :foreground ,fg3))))
        `(org-level-3 ((,class (:bold t :foreground ,fg4))))
        `(org-level-4 ((,class (:bold nil :foreground ,bg4))))
        `(org-date ((,class (:underline t :foreground ,var) )))
        `(org-footnote  ((,class (:underline t :foreground ,fg4))))
        `(org-link ((,class (:underline t :foreground ,type ))))
        `(org-special-keyword ((,class (:foreground ,func))))
        `(org-verbatim ((,class (:foreground ,bg3 :underline t :slant italic))))
        `(org-block ((,class (:foreground ,fg3))))
        `(org-quote ((,class (:inherit org-block :slant italic))))
        `(org-verse ((,class (:inherit org-block :slant italic))))
        `(org-todo ((,class :foreground ,keyword :bold t)))
        `(org-done ((,class (:bold t :foreground ,bg4))))
        `(org-warning ((,class (:underline t :foreground ,warning))))
        `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
        `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
        `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
        `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
        `(org-scheduled ((,class (:foreground ,type))))
        `(org-ellipsis ((,class (:foreground ,builtin))))
        `(org-verbatim ((,class (:foreground ,fg4))))
        `(org-document-info-keyword ((,class (:foreground ,func))))
        `(font-latex-bold-face ((,class (:foreground ,type))))
        `(font-latex-italic-face ((,class (:foreground ,key3 :italic t))))
        `(font-latex-string-face ((,class (:foreground ,str))))
        `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
        `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
        `(ido-only-match ((,class (:foreground ,warning))))
        `(org-sexp-date ((,class (:foreground ,fg4))))
        `(ido-first-match ((,class (:foreground ,keyword :bold t))))
        `(gnus-header-content ((,class (:foreground ,keyword))))
        `(gnus-header-from ((,class (:foreground ,var))))
        `(gnus-header-name ((,class (:foreground ,type))))
        `(gnus-header-subject ((,class (:foreground ,func :bold t))))
        `(mu4e-view-url-number-face ((,class (:foreground ,type))))
        `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
        `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
        `(mu4e-header-marks-face ((,class (:foreground ,type))))
        `(ffap ((,class (:foreground ,fg4))))
        `(js2-private-function-call ((,class (:foreground ,const))))
        `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
        `(js2-jsdoc-html-tag-name ((,class (:foreground ,key2))))
        `(js2-external-variable ((,class (:foreground ,const  ))))
        `(warning ((,class (:foreground ,warning))))
        `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
        `(info-quoted-name ((,class (:foreground ,builtin))))
        `(info-string ((,class (:foreground ,str))))
        `(icompletep-determined ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-register-face ((,class :foreground ,keyword)))
        `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
        `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
        `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
        `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
        `(magit-process-ok ((,class :foreground ,type)))
        `(trailing-whitespace ((,class :foreground nil :background ,warning)))
        `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
        `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
        `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
        `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
        `(magit-item-highlight ((,class :background ,bg3)))
        `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
        `(magit-hunk-heading           ((,class (:background ,bg3))))
        `(magit-section-highlight      ((,class (:background ,bg2))))
        `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
        `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
        `(magit-diffstat-added   ((,class (:foreground ,type))))
        `(magit-diffstat-removed ((,class (:foreground ,var))))
        `(magit-process-ok ((,class (:foreground ,func :weight bold))))
        `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
        `(magit-branch ((,class (:foreground ,const :weight bold))))
        `(magit-log-author ((,class (:foreground ,fg3))))
        `(magit-hash ((,class (:foreground ,fg2))))
        `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'oldlace)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; oldlace-theme.el ends here
