;;; mr-eshell.el --- eshell settings -*- lexical-binding: t -*-

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

;; some eshell settings

;;; Code:
(provide 'mr-eshell)

(setq eshell-prompt-function
      (lambda ()
        (concat (eshell/pwd) "\n $ ")))
(setq eshell-highlight-prompt nil)

;; SHELLS
;; ===============================================================================
;; VTERM @TODO configure and compare with eat/eshell other...
;;(use-package vterm
;;    :ensure t) ;; NEED TO INSTALL CMAKE!


;;; mr-eshell.el ends here
