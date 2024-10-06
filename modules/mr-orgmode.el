;;; mr-orgmode.el --- orgmode settings -*- lexical-binding: t -*-

;; Author: Maxime Rousseau
;; Maintainer: Maxime Rousseau
;; Version: 0.0
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; pretty self explanatory

;;; Code:
;;(provide 'mr-orgmode)

;; org-mode settings
(setq org-cycle-emulate-tab 'white)
(add-hook 'org-mode-hook (lambda ()
						   (turn-off-auto-fill)
						   (visual-line-mode)))
(setq org-image-actual-width nil) ;;To set image scale

(add-hook 'org-mode-hook 'org-indent-mode) ;; not sure this works

(add-hook 'org-mode-hook (lambda ()
						   (org-indent-mode 1)
						   (org-fragtog-mode 1)
						   (plist-put org-format-latex-options :scale 1.5)
						   ))


;; ORGMODE ================================================================================
;; @TODO
;; > ***setup DRAG AND DROP - https://github.com/abo-abo/org-download***
;; > setup org agenda
;; > case study presentation build script (case.el, use pynoter for ppt?, user beamer for academia)
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))
(add-to-list 'org-latex-packages-alist '("" "tabularx"))

(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable);; Drag-and-drop to `dired`
  )

;; ==========================================================
;; ORG READING MODE -- change font hook when in darkroom mode
(defun my-darkroom-mode-hook ()
  "Custom hook for darkroom-mode."
  (setq buffer-face-mode-face '(:family "Helvetica" :height 120))
  (buffer-face-mode))

(add-hook 'darkroom-tentative-mode-hook 'my-darkroom-mode-hook)
(add-hook 'darkroom-tentative-mode-off-hook
          (lambda ()
            (buffer-face-mode -1))) ; Revert to the default face when leaving darkroom-mode


;;; mr-orgmode.el ends here
