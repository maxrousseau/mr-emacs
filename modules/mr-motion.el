;;; god-mode and other things

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))


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

;; keybindings
;; some other kbd
(global-set-key (kbd "C-; n") 'make-frame)
(global-set-key (kbd "C-; c") 'delete-frame)
(global-set-key (kbd "C-; b") 'ibuffer)

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


(eval-after-load "org" '(progn
						  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
						  (define-key org-mode-map (kbd "C-; s") 'counsel-org-goto-all)))
(add-hook 'org-beamer-mode-hook
		  (lambda () (local-set-key (kbd "C-; e") 'org-beamer-select-environment)))
