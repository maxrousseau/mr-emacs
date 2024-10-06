(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(use-package avy
  :config
  (global-set-key (kbd "C-r") 'avy-goto-char-2)
  (global-set-key (kbd "C-q") 'avy-goto-line)
  )

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

(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

; window management  ;;;;;;;;;;;;;;;;;;
(use-package ace-window
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-2") 'split-window-vertically)
(global-set-key (kbd "C-x C-0") 'delete-window)

; more powerful find commands ;;;;;;;;;
(global-set-key (kbd "C-; C-s") 'counsel-rg)
;; @TODO :: add occur here


(eval-after-load "org" '(progn
						  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
						  (define-key org-mode-map (kbd "C-; s") 'counsel-org-goto-all)))
(add-hook 'org-beamer-mode-hook
		  (lambda () (local-set-key (kbd "C-; e") 'org-beamer-select-environment)))


;; EDITING =======================================================================
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-; m") 'mc/edit-lines)
  )

;; WHICH-KEY @TODO

;; HYDRA
;; ===============================================================================
;; @TODO setup fast ops for dired, if good maybe combine with god-mode and snipe
