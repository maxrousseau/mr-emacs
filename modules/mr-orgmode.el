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

(message "Hello World!")

(provide 'mr-orgmode)

;; org-mode settings
(setq org-cycle-emulate-tab 'white)
(add-hook 'org-mode-hook (lambda ()
						   (turn-off-auto-fill)
						   (visual-line-mode)))
(setq org-image-actual-width nil) ;;To set image scale
(add-hook 'org-mode-hook 'org-indent-mode) ;; not sure this works


;;; mr-orgmode.el ends here
