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

;; packaging
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
  (require 'use-package))

;; environment variables
(setenv "PATH"
		(concat "/Library/TeX/texbin" ":"
				(getenv "PATH")))

;; startup
(setq inhibit-startup-screen t)
(cd "~/code")

;; other functions
(load-file "~/code/secret.el")

;; swiper setup
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(use-package swiper)
(use-package counsel)

;; appearance
(setq ring-bell-function 'ignore) ;; no bell
(global-hi-lock-mode 1) ;; can't remember what this does

(defun meta_highlight()
  (interactive)
  "highlight todos, notes and more"
  (highlight-regexp "@TODO" 'hi-pink)
  (highlight-regexp "@BUG" 'hi-red)
  (highlight-regexp "@HERE" 'hi-green)
  (highlight-regexp "@NOTE" 'hi-blue))
(add-hook 'find-file-hook (lambda () (meta_highlight)))


(display-time-mode 1) ;; This status line is not great, improve on clarity of information displayed.
(blink-cursor-mode -1)
(global-hl-line-mode 1) ;; highlight current line
;; disable all GUI bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; fonts
(set-frame-font "Hack 12" nil t)

;; line numbering and indentation
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default tab-width 4 indent-tabs-mode t)

;; autrowrap 120
(setq-default fill-column 120)
(setq auto-fill-mode t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; org
(setq org-cycle-emulate-tab 'white)
(add-hook 'org-mode-hook (lambda ()
						   (turn-off-auto-fill)
						   (visual-line-mode)))
(setq org-image-actual-width nil) ;;To set image scale
(add-hook 'org-mode-hook 'org-indent-mode) ;; not sure this works


;; dired
(setq dired-listing-switches "-l")

;; Use external app to open file from dired, taken from xah lee
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

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode) ))


;; backup
(let ((backup-dir "~/.emacs.d/Emacs/backups")
      (auto-saves-dir "~/.emacs.d/Emacs/autosavedir/")
      )
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir
        )
  )
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 3    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; eshell
(setq eshell-prompt-function
      (lambda ()
        (concat (eshell/pwd) "\n $ ")))
(setq eshell-highlight-prompt nil)

;; keybindings
;; some other kbd
(global-set-key (kbd "C-; n") 'make-frame)
(global-set-key (kbd "C-; c") 'delete-frame)
(global-set-key (kbd "C-; b") 'ibuffer)

(eval-after-load "org" '(progn
						  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
						  (define-key org-mode-map (kbd "C-; /") 'counsel-org-goto-all)))
(add-hook 'org-beamer-mode-hook
		  (lambda () (local-set-key (kbd "C-; e") 'org-beamer-select-environment)))

(eval-after-load "dired" '(progn
			    (define-key dired-mode-map (kbd "C-; o") 'xah-open-in-external-app) ))

(global-set-key (kbd "C-; e") 'eshell)
(global-set-key (kbd "C-; M-e") 'eshell-command)
(global-set-key (kbd "C-; l") 'display-line-numbers-mode)
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-S-s") 'swiper-all)

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; @TODO C-S-n p f b bind to the faster movements? instead of using meta...
;; @TODO bind avy jump mode to C-S-f should be easier for all big movements
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

(use-package ace-window
  :config
  (global-set-key (kbd "C-x C-o") 'ace-window))

(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-2") 'split-window-vertically)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-; C-f") 'counsel-fzf)
(global-set-key (kbd "C-; C-s") 'counsel-rg)

(use-package god-mode
  :config
  (global-set-key (kbd "C-q") #'god-mode-all)
  (custom-set-faces
   '(god-mode-lighter ((t (:inherit error)))))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  ) ;; rebind esc to ctrl for ease of use

(use-package avy
  :config
  (global-set-key (kbd "C-r") 'avy-goto-char-2))

;; programming
(icomplete-mode t)
(add-hook 'python-mode-hook
    (lambda ()
	    (setq-default indent-tabs-mode nil)
	    (setq-default tab-width 4)
	    (setq-default py-indent-tabs-mode t)
		(add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;;; simple.el ends here


;;; not cleaned yet is below ------------------



;; build function to execute program
;; flymake of flycheck setup
(use-package flycheck
  :hook (after-init . global-flycheck-mode))


;; (use-package company
;;   :ensure t
;;   :defer t
;;   :custom
;;   ;; Search other buffers with the same modes for completion instead of
;;   ;; searching all other buffers.
;;   (company-dabbrev-other-buffers t)
;;   (company-dabbrev-code-other-buffers t)
;;   ;; M-<num> to select an option according to its number.
;;   (company-show-numbers t)
;;   ;; Only 2 letters required for completion to activate.
;;   (company-minimum-prefix-length 2)
;;   ;; Do not downcase completions by default.
;;   (company-dabbrev-downcase nil)
;;   ;; Even if I write something with the wrong case,
;;   ;; provide the correct casing.
;;   (company-dabbrev-ignore-case t)
;;   ;; company completion wait
;;   (company-idle-delay 0.01)
;;   ;; No company-mode in shell & eshell
;;   (company-global-modes '(not eshell-mode shell-mode))
;;   ;; Use company with text and programming modes.
;;     :hook ((text-mode . company-mode)
;;            (prog-mode . company-mode)))
(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/code/mr-emacs/snippets"
                           "~/.emacs.d/elpa/yasnippet-snippets-20230622.1323/snippets"))
  ;;(yas-recompile-all) ;; if you add new snippets
  (yas-reload-all)
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )
;; @TODO :: figure out a way to cancel company mode the completions which interfere with snippets

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(add-hook 'company-mode-hook (lambda ()
  (substitute-key-definition 'company-complete-common
                             'company-yasnippet-or-completion
                             company-active-map)))

;; M-x yas-insert-snippet
;; tab to expand snippet @BUG tab does not always expand snippet if company displaying choices, also need to fix custom
;; snippets which don't load too well.

;;@TODO
;;> french and english spellchecker *PRIORITY*
;;> setup snippets
;;> pubsearch -- desired functionalities: pubsearch integration (browse pubmed,
;;arxiv abstracts with associated bibtex citation for easy import)

;; ZEN COMPUTER
;; @TODO  setup elfeed to get rid of the web https://github.com/skeeto/elfeed?tab=readme-ov-file

;; fast latex preview https://github.com/let-def/texpresso

;; gptel - call completion apis with emacs


;; SHELLS
;; ===============================================================================
;; VTERM @TODO configure and compare with eat/eshell other...
;;(use-package vterm
;;    :ensure t) ;; NEED TO INSTALL CMAKE!


;; EYECANDY
;; ===============================================================================

;; @TODO improve the modeline https://protesilaos.com/codelog/2023-07-29-emacs-custom-modeline-tutorial/
(setq custom-safe-themes t)
(use-package ef-themes
  :ensure t
  )
;(setq ef-themes-to-toggle '(ef-elea-dark ef-winter))
;; cyprus and elea are nice green-ish themes
;; winter and frost are nice too and less colorful
;(setq ef-themes-mixed-fonts nil
;     ef-themes-variable-pitch-ui nil)
;; Global settings (defaults)
;;(ef-themes-select 'ef-elea-dark)
(load-theme 'dracula)


;; (use-package nyan-mode
;;   :config
;;   (setq nyan-wavy-trail t)
;;   (setq nyan-animate-nyancat t)
;;   (nyan-mode 1))

;; @TODO REFACTOR THIS - does not work with ef-themes
;; (defun switch-theme ()
;;   (interactive)
;;   (disable-theme current-theme)
;;   (cond ((equal current-theme 'doom-dracula)
;;          (load-theme other-theme t)
;;          (setq current-theme other-theme)
;;          (setq other-theme 'doom-dracula))
;;         ((equal current-theme 'doom-earl-grey)
;;          (load-theme other-theme t)
;;          (setq current-theme other-theme)
;;          (setq other-theme 'doom-earl-grey))))
;; (global-set-key (kbd "C-; t") 'switch-theme)


;; emojis
;; https://ianyepan.github.io/posts/emacs-emojis/
;; @TODO find better emoji font
(use-package emojify
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-; q") #'emojify-insert-emoji))

;; EDITING =======================================================================

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-; m") 'mc/edit-lines)
  ;; @BUG conflict with godmode, need to exit god-mode when entering this
  ;; or figure our something else
  )


;; E/SHELL =======================================================================

;; WHICH-KEY

;; HYDRA
;; ===============================================================================
;; @TODO setup fast ops for dired, if good maybe combine with god-mode and snipe


;; TREEMACS or other (https://github.com/jojojames/dired-sidebar)
;; ===============================================================================
;; I almost never use this
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 16)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)


(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; Projectile
;; ===============================================================================
;; @TODO


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

(plist-put org-format-latex-options :scale 2)

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
