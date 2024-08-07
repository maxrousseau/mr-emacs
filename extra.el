;; Extra emacs config
;;		> load and setup bells and whistles with 3rd party packages
;;
;;             /\
;; /vvvvvvvvvvvv \--------------------------------------,
;; `^^^^^^^^^^^^ /====================================="
;;             \/
;;
;; ================================================================================
;;
;; Packages:
;;  Yasnippets
;;  Flymake(flycheck)
;;  Eglot (lsp), Company, python-jedi (?)
;;  Swiper, ivy, counsel
;;
;; Implement flycheck/make, which-key, hydra, eldoc-box, popper.el (for eshell),
;;
;;
;; SETUP ================================================================================
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

;; @TODO setup undo-redo commands

;; @NOTE
;; consider replacing all movement keybindings with god mode, with avy or ace-jump-mode
;; consider moving all function keybindings to hydra...
;; keys -> M-? (?), n p b f (simplify mvt)


;; @TODO static website in org-mode (custom html/css export)
;;@NOTE this requires counsel to be installed (M-x package-install counsel from elpa)
;; @TODO -- move all keybinds to general.el


;; MOVEMENT and KEYBINDINGS  ==================================================
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-S-s") 'swiper-all)

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; @TODO C-S-n p f b bind to the faster movements? instead of using meta...
;; @TODO bind avy jump mode to C-S-f should be easier for all big movements
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-; C-a") 'avy-goto-char-2)

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
  (god-mode-all)
  ) ;; rebind esc to ctrl for ease of use

(use-package avy
  :config
  (global-set-key (kbd "C-r") 'avy-goto-char-2))

;; swiper setup
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(use-package swiper)
(use-package counsel)


;; PYTHON ================================================================================
;; @TODO documentation lookup***
;; @TODO dap-mode configuration for debugging
;; @TODO projectile setup
;; @TODO setup tabnine

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

(use-package pyvenv
  :ensure t
  :defer t)

(setenv "PATH" (concat (getenv "PATH") "/opt/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

(add-hook 'python-mode-hook 'eglot-ensure)


;; @TODO -- configure a function to send a command to be executed by eshell (ex
;; python main.py -debug); maybe use projectile for this?
(add-hook 'python-mode-hook
  (lambda ()
    (local-set-key [f1] 'python-shell-send-buffer)
    (local-set-key [f2] 'python-shell-send-defun)
    ))

(use-package blacken
  :ensure t
  :defer t
  :custom
  (blacken-allow-py36 t)
  :hook (python-mode . blacken-mode))


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
(load-theme 'oldlace)


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


;; ==========================================================

;; WRITING
;; ltex lsp, language tool with flycheck?, etc...

;; NOTES and anki
;; Define the functions (as provided earlier)

(defun convert-anchored-notes-in-buffer ()
  "Converts all occurrences of '* Anchored Note, page XXX' to '[page XXX]' in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\* Anchored Note, page \\([0-9]+\\)" nil t)
      (replace-match "[page \\1]"))))

(defun convert-anchored-notes-in-region (start end)
  "Converts all occurrences of '* Anchored Note, page XXX' to '[page XXX]' in the selected region."
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\* Anchored Note, page \\([0-9]+\\)" nil t)
      (replace-match "[page \\1]"))
    (widen)))

(defun wrap-with-c1-tags (start end)
  "Wraps the selected region with '{{c1:' before and '}}' after."
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (insert "{{c1:")
    (goto-char (point-max))
    (insert "}}")
    (widen)))

(defun convert-org-to-inline-html (start end)
  "Converts selected Org mode snippet to inline HTML."
  (interactive "r")
  (let ((org-content (buffer-substring-no-properties start end))
        (html-content ""))
    (with-temp-buffer
      (insert org-content)
      (org-mode)
      (setq html-content (org-export-string-as (buffer-string) 'html t
                                               '((standalone . nil)
                                                 (html-inline-images . t)
                                                 (html-style-include-default . nil)
                                                 (section-numbers . nil)
                                                 (toc . 0)))))
    (delete-region start end)
    (insert (replace-regexp-in-string "\n" "" html-content))
    (insert " |")))

;; Define the minor mode
(define-minor-mode anki-edit-mode
  "A minor mode for custom functions."
  :lighter " AnkiEd"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c b") 'convert-anchored-notes-in-buffer)
            (define-key map (kbd "C-c a") 'convert-anchored-notes-in-region)
            (define-key map (kbd "C-c w") 'wrap-with-c1-tags)
            (define-key map (kbd "C-c h") 'convert-org-to-inline-html)
            map)
  (if anki-edit-mode
      (progn
        (auto-fill-mode -1) ; Disable auto-fill-mode
        ;; Additional setup code when my-custom-mode is activated goes here
      )
    (auto-fill-mode 1)))

;; Activate the minor mode
;;(my-custom-mode 1)

;; publishing org-NOTES

(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)
