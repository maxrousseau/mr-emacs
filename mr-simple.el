;;; mr-simple.el --- less is more, a configuration -*- lexical-binding: t -*-

;; Author: Maxime Rousseau
;; Maintainer: Maxime Rousseau
;; Version: 0.1
;; Package-Requires: (tbd)
;; Homepage: None
;; Keywords: config


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

;;; a bare minimum configuration for the tool minimalist who just wants something simple that works.
;;; goal a <300LOC config that is legible and sufficient

(provide 'mr-simple)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                               ; configuration ;;;;;;;;;;;;;;;;
(setq mr-configuration
      '((mr-startup-dir . "~/code")
       (mr-font-face . "Hack")
       (mr-font-size . "12")
       (mr-light-theme . modus-operandi-tinted)
       (mr-dark-theme . modus-vivendi-tinted)
       (mr-private-file . "~/code/mr-emacs/modules/secret.el")
       (mr-backup-dir . "~/.emacs.d/Emacs/backups")
       (mr-auto-saves-dir . "~/.emacs.d/Emacs/autosavedir/")
       (mr-personal-snippets . "~/code/mr-emacs/snippets")
       (mr-builtin-snippets . "~/.emacs.d/elpa/yasnippet-snippets-20230622.1323/snippets")
       ))

(dolist (c mr-configuration)
  (set (car c) (cdr c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mr-setup-packages ()
  "Setup the packages repositories and USE-PACKAGE."
  (require 'package)
  (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-and-compile
    (setq use-package-always-ensure t
          use-package-expand-minimally t))

  (eval-when-compile
    (require 'use-package)))


(defun mr-set-latex-path ()
  "Set the path to TeX binary."
  (setenv "PATH"
		  (concat "/Library/TeX/texbin" ":"
				  (getenv "PATH"))))


(defun mr-startup ()
  "What you will see upon opening mr-emacs.
It will cd into the main directory set by the configuration,
load your private file, and open the today dashboard"
  (setq inhibit-startup-screen t)
  (cd mr-startup-dir)
  (load-file mr-private-file))

(defun mr-completion-and-selection ()
"Configure Ivy, Counsel, and Swiper for enhanced completion and search.

Enables `ivy-mode` for Ivy-based completion, with virtual buffers and count format.
Sets up Swiper for improved buffer search and Counsel for extended command enhancements."
  (use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
  (use-package swiper)
  (use-package counsel))



(defun mr-basic-appearances ()
  "Default appearance settings for mr-emacs.

> disables ring bell
> NOTE: can't remember what global hi lock mode does
> adds highlighting for some keywords I drop here and there
> disable cursor blinking
> highlight current line
> removes all the bars
> sets the font"

  ;; bell
  (setq ring-bell-function 'ignore)

  (global-hi-lock-mode 1)

  ;; highlights
  (defun meta_highlight()
    (interactive)
    "highlight todos, notes and more"
    (highlight-regexp "@TODO" 'hi-pink)
    (highlight-regexp "@BUG" 'hi-red)
    (highlight-regexp "@HERE" 'hi-green)
    (highlight-regexp "@NOTE" 'hi-blue))
  (add-hook 'find-file-hook (lambda () (meta_highlight)))

  ;; disable all GUI bars
  (display-time-mode 1)
  (blink-cursor-mode -1)
  (global-hl-line-mode 1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; fonts
  (set-frame-font (concat mr-font-face " " mr-font-size) nil t) ;; fonts

  ;; line numbering and indentation
  (setq display-line-numbers-type 'relative) ;'relative is nice but none is fine
  (setq-default tab-width 4 indent-tabs-mode t)

  ;; autrowrap 120
  (setq-default fill-column 120)
  (setq auto-fill-mode t)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'prog-mode-hook 'turn-on-auto-fill))


(defun mr-dired-options ()
  "Set some Dired options."
  (setq dired-listing-switches "-l")
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode))))

;;; @TODO :: can be replaced by emacs crux*
(defun xah-open-in-external-app (&optional @fname)
"Open the current file ( @FNAME ) or Dired marked files in external app.
	When called in Emacs Lisp, if @fname is given, open that.
	URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
	Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(defun mr-backups ()
  "Check if the backup directories exist (if not create) and set."
  (dolist (dir (list mr-backup-dir mr-auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))

  (setq backup-directory-alist `(("." . ,mr-backup-dir))
        auto-save-file-name-transforms `((".*" ,mr-auto-saves-dir))
        auto-save-list-file-prefix (concat mr-auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,mr-backup-dir))
        tramp-auto-save-directory mr-auto-saves-dir)

  (setq backup-by-copying t    ; Don't delink hardlinks
        delete-old-versions t  ; Clean up the backups
        version-control t      ; Use version numbers on backups,
        kept-new-versions 3    ; keep some new versions
        kept-old-versions 2)   ; and some old ones, too
 )

;; @TODO improve the modeline https://protesilaos.com/codelog/2023-07-29-emacs-custom-modeline-tutorial/
(defun mr-eyecandy ()
  "Set light/dark theme and other preferences."
  (require 'modus-themes)
  (require 'spacious-padding)
  (setq modus-themes-to-toggle (list mr-dark-theme mr-light-theme))
  (modus-themes-load-theme mr-dark-theme)
  (rainbow-delimiters-mode 1)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (setq spacious-padding-subtle-mode-line t)
  (setq spacious-padding-widths '(:right-divider-width 20))
  (spacious-padding-mode 1)
  )


(defun mr-snippets ()
  "Configures local directory and builtin snippets with yasnippets."

  (use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs (list mr-personal-snippets mr-builtin-snippets))
  (yas-recompile-all) ;; if you add new snippets
  (yas-reload-all)
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook #'yas-minor-mode)))


;; call functions
(mr-setup-packages)
(mr-set-latex-path)
(mr-startup)
(mr-completion-and-selection)
(mr-basic-appearances)
(mr-backups)
(mr-dired-options)
(mr-eyecandy)
(mr-snippets)

(message "SIMPLE.EL LOADED")
;; try to load modules
(setq mr-emacs-root (file-name-directory (or load-file-name user-init-file)))
(setq mr-emacs-module-dir (concat mr-emacs-root "modules/"))
(condition-case nil
    (load-file (concat mr-emacs-module-dir "mr-motion.el"))
  (error (message-box "Could not load modules...")))

(defun load-modules-with-error-check (module-list)
  "Load each Emacs module from MODULE-LIST and display an error message if it fails to load."
  (dolist (module module-list)
    (condition-case err
        (load-file (concat mr-emacs-module-dir module))
      (error (message "Error loading module %s: %s" module (error-message-string err))))))

;; List of modules you want to load
(setq my-modules '("mr-ai.el"
				   "mr-motion.el"
				   "mr-python.el"
				   "mr-eshell.el"
				   "mr-orgmode.el"
				   ))

;; Call the function to load the modules
(load-modules-with-error-check my-modules)


;;; simple.el ends here
