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
;; Evil-mode (because otherwise i'll be getting cubital tunnel)
;; ------------------------------------------------------------
;; @TODO set evil keybindings with local leader for common commands
;; @TODO improve window switching keybindings
;; @TODO setup undo-redo commands (evil)
;; @TODO darkroom mode
;; @TODO emojis
;; @TODO static website in org=mode (custom html/css export)
;; @TODO remap these asap > (other-window), (switch-to-buffer), etc
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/evil-leader")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/evil")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/evil-org-mode")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/swiper")
;;(add-to-list 'load-path "C:/Users/roum5/source/dotfiles/emacs/themes/")

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")

(require 'evil)
(evil-mode 1)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;;@TODO -- emoji display problem...

;;@BUG this requires counsel to be installed (M-x package-install counsel from elpa)
(evil-global-set-key 'normal (kbd "/") 'swiper-isearch)
(evil-global-set-key 'insert (kbd "TAB") 'dabbrev-completion)
(global-set-key (kbd "C-s") 'swiper-isearch)
;;(global-set-key (kbd "M-x") 'counsel-M-x)


(evil-leader/set-key
  "f" 'counsel-find-file
  "b" 'ivy-switch-buffer
  "o" 'other-window)

;; swiper setup
(require 'ivy)
(require 'swiper)
(require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; @TODO Snippets (see the .el file which allows to customize snippets manually)
;; @TODO Syntax checking for all modes?
;; @TODO Setup autocomplete C-; K and LSP


;; python mode
;; @TODO black formatting and reload upon save
;; build function to execute program
;; flymake of flycheck setup
;; python-check with black on save


;;@TODO
;;> pubsearch.el -- desired functionalities: pubsearch integration (browse pubmed, arxiv abstracts with associated bibtex citation for easy import)
;;> case study presentation build script (case.el, use pynoter for ppt?, user beamer for academia)
;;> french and english spellchecker *PRIORITY*
;;> setup org agenda
;;> setup snippets
;;> pomodoro mode
