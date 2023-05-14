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

;; @TODO improve window switching keybindings
;; @TODO setup undo-redo commands

;; @NOTE
;; consider replacing all movement keybindings with god mode, with avy or ace-jump-mode
;; consider moving all function keybindings to hydra...
;; keys -> M-? (?), n p b f (simplify mvt)


;;@TODO -- emoji display problem...
;; @TODO static website in org-mode (custom html/css export)
;;@NOTE this requires counsel to be installed (M-x package-install counsel from elpa)
;; @TODO -- move all keybinds to general.el


;; MOVEMENT and KEYBINDINGS  ==================================================
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-S-s") 'swiper-all)
(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; TODO C-S-n p f b bind to the faster movements? instead of using meta...
;; @TODO bind avy jump mode to C-S-f should be easier for all big movements
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-2") 'split-window-vertically)
(global-set-key (kbd "C-x C-0") 'delete-window)

(use-package god-mode
  :config
  (god-mode)
  (global-set-key (kbd "<escape>") #'god-local-mode)
;; @TODO write a function that binds a function key (i.e. F2) to disables the backtick character for mode switching in case I
;; need it...
  (custom-set-faces
   '(god-mode-lighter ((t (:inherit error)))))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  )

(use-package avy
  :config
  (global-set-key (kbd "C-S-f") 'avy-goto-char-2))


;; swiper setup
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(use-package swiper)
(use-package counsel)


;; PYTHON ================================================================================
;; @TODO documentation lookup
;; @TODO dap-mode configuration for debugging
;; @TODO projectile setup
;; build function to execute program
;; flymake of flycheck setup
(use-package flycheck
  :hook (after-init . global-flycheck-mode))

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
  (company-minimum-prefix-length 2)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.01)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)))


;; install pyls
(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure))

;; @TODO setup tabnine

(use-package virtualenvwrapper
  :ensure t
  :defer t
  :custom
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (cond ((string-equal system-type "windows-nt") (setq venv-location "c:/Users/roum5/Envs/"))
      ((string-equal system-type "gnu/linux") (setq venv-location "/home/max/.virtualenvs/")))
  (setq-default mode-line-format (cons '(:exec venv-current-name)
                                       mode-line-format))
  )


;; @TODO -- configure a function to send a command to be executed by eshell (ex
;; python main.py -debug); maybe use projectile for this?
(add-hook 'python-mode-hook
  (lambda ()
    (local-set-key [f1] 'python-shell-send-buffer)
    (local-set-key [f2] 'python-shell-send-defun)
    ))

;; @BUG -> this runs before activation?
;;(add-hook 'venv-postactivate-hook (setq eshell-prompt-function
;;                                        (lambda () (concat (eshell/pwd) " (" venv-current-name
;;                                                           ")" "\n $ "))))


(use-package blacken
  :ensure t
  :defer t
  :custom
  (blacken-allow-py36 t)
  :hook (python-mode . blacken-mode))


(use-package yasnippet
  :ensure t
  :defer t
  :custom
  (setq yas-snippets-dirs '("./snippets"))
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode)
  )
;; M-x describe table
;; M-x yas-insert-snippet
;; tab to expand snippet

;;@TODO
;;> french and english spellchecker *PRIORITY*
;;> setup snippets
;;> pubsearch -- desired functionalities: pubsearch integration (browse pubmed,
;;arxiv abstracts with associated bibtex citation for easy import)

;; EYECANDY
;; ===============================================================================
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; not working for now...
;; (use-package all-the-icons
;;   :ensure t
;;   :if (display-graphic-p))
;;
;; (use-package all-the-icons-dired
;;   :ensure t
;;   :if (display-graphic-p)
;;   :config
;;   (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;;   )

;;(use-package dired-sidebar
;;  :ensure t
;;  :commands (dired-sidebar-toggle-sidebar)
;;  :config (bind-key* (kbd "C-; C-s") #'dired-sidebar-toggle-sidebar)
;;  ;;(setq dired-sidebar-use-custom-font t)
;; )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  (doom-themes-org-config)
  (setq current-theme 'doom-dracula))

(setq other-theme 'doom-earl-grey)

(use-package nyan-mode
  :config
  (setq nyan-wavy-trail t)
  (setq nyan-animate-nyancat t)
  (nyan-mode 1))

;; @TODO REFACTOR THIS - set bool IS_LIGHT, and then set light and dark themes
(defun switch-theme ()
  (interactive)
  (disable-theme current-theme)
  (cond ((equal current-theme 'doom-dracula)
         (load-theme other-theme t)
         (setq current-theme other-theme)
         (setq other-theme 'doom-dracula))
        ((equal current-theme 'doom-earl-grey)
         (load-theme other-theme t)
         (setq current-theme other-theme)
         (setq other-theme 'doom-earl-grey))))
(global-set-key (kbd "C-; t") 'switch-theme)


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

;; E/SHELL =======================================================================

;; WHICH-KEY

;; HYDRA
;; ===============================================================================
;; @TODO setup fast ops for dired, if good maybe combine with god-mode and snipe to remove evil(vim)


;; TREEMACS or other (https://github.com/jojojames/dired-sidebar)
;; ===============================================================================
;; @TODO

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

;; WRITING
;; ltex lsp, language tool with flycheck?, etc...
