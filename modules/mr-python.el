;; PYTHON ================================================================================
;; @TODO documentation lookup command***
;; @TODO -- configure a function to send a command to be executed by eshell (ex
;; python main.py -debug); maybe use projectile for this?

(use-package pyvenv
  :ensure t
  :defer t)

(setenv "PATH" (concat (getenv "PATH") "/opt/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

(add-hook 'python-mode-hook 'eglot-ensure)


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

;; programming
(add-hook 'python-mode-hook
    (lambda ()
	    (setq-default indent-tabs-mode nil)
	    (setq-default tab-width 4)
	    (setq-default py-indent-tabs-mode t)
		(add-to-list 'write-file-functions 'delete-trailing-whitespace)))

										; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake of flycheck setup
(use-package flycheck
  :hook (after-init . global-flycheck-mode))
(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
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

(use-package corfu
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

;; lint with ruff
(require 'flymake-ruff)
(add-hook 'python-mode-hook #'flymake-ruff-load)
