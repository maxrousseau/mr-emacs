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
;;  Evil, evil-leader
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

;; EVIL ================================================================================
;; @TODO improve window switching keybindings
;; @TODO setup undo-redo commands (evil)
;; @TODO emojis
;; @TODO static website in org-mode (custom html/css export)
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/evil-leader")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/evil")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/evil-org-mode")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/swiper")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/themes/")
(use-package evil
  :config (evil-mode 1))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC"))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  (require evil-org-agenda)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

;;@TODO -- emoji display problem...

;;@BUG this requires counsel to be installed (M-x package-install counsel from elpa)
;; @TODO -- move all keybinds to general.el
(evil-global-set-key 'normal (kbd "/") 'swiper-isearch)
(global-set-key (kbd "C-s") 'swiper-isearch)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
(evil-leader/set-key
  "f" 'counsel-find-file
  "b" 'ivy-switch-buffer
  "o" 'other-window)

;; swiper setup
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(use-package swiper)
(use-package counsel)

;; PYTHON ================================================================================
;; @TODO
;; dap-mode configuration for debugging
;; projectile setup
;; @TODO Snippets (see the .el file which allows to customize snippets manually)
;; @TODO Syntax checking for all modes?
;; python mode
;; build function to execute program
;; flymake of flycheck setup


;; LSP with eglot and company
(use-package company
  :ensure t
  :defer t
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.1)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)))


(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure))
;;(eglot--executable-find "pyls" t)

(use-package virtualenvwrapper
  :ensure t
  :defer t
  :custom (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs")
  (setq-default mode-line-format (cons '(:exec venv-current-name)
                                       mode-line-format))
  )

(add-hook 'venv-postactivate-hook (setq eshell-prompt-function
                                        (lambda () (concat (eshell/pwd) " (" venv-current-name
                                                           ")" "\n $ "))))


(use-package blacken
  :ensure t
  :defer t
  :custom
  (blacken-allow-py36 t)
  :hook (python-mode . blacken-mode))

;;@TODO
;;> french and english spellchecker *PRIORITY*
;;> setup snippets
;;> pubsearch -- desired functionalities: pubsearch integration (browse pubmed,
;;arxiv abstracts with associated bibtex citation for easy import)

;; eyecandy
;; other themes: doom-theme-earl-grey (light)
(use-package solo-jazz-theme
  :ensure t
  :config
  (load-theme 'solo-jazz t)
  (setq current-theme 'solo-jazz))

(setq other-theme 'dracula)

(defun switch-theme ()
  (interactive)
  (disable-theme current-theme)
  (cond ((equal current-theme 'solo-jazz)
         (load-theme other-theme t)
         (setq current-theme other-theme)
         (setq other-theme 'solo-jazz))
        ((equal current-theme 'dracula)
         (load-theme other-theme t)
         (setq current-theme other-theme)
         (setq other-theme 'dracula))))
(global-set-key (kbd))


;; ORGMODE ================================================================================
;; @TODO
;; > setup org agenda
;; > case study presentation build script (case.el, use pynoter for ppt?, user beamer for academia)
(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))
