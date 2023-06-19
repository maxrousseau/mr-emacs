;; Base emacs config
;;		> no third party packages to be downloaded
;;
;;             /\
;; /vvvvvvvvvvvv \--------------------------------------,
;; `^^^^^^^^^^^^ /====================================="
;;             \/
;;
;; ================================================================================
;;
;; User defined default variables
;; ------------------------------------------------------------
;;(cond ((string-equal system-type "windows-nt") (setq source_dir "C:\\Users\\roum5\\source\\"))
;;      ((string-equal system-type "gnu/linux") (setq source_dir "/home/max/src/")))
;;(setq
;;
;;(setq file_list (list
;;		 "log/src/"
;;		 "dotfiles/"
;;         "mr-emacs"))
;;
;;(setq default_buffers (mapcar (lambda (x) (concat source_dir x)) file_list)) ;; concatenate to file-list
(setq source_dir "~/code/")

;; Appearance
;; ------------------------------------------------------------
;; @TODO - whitespace and tab view
(add-to-list 'custom-theme-load-path (concat source_dir "mr-emacs/themes"))
(setq ring-bell-function 'ignore) ;; no bell

;; some highlighting of keywords
(global-hi-lock-mode 1)
(defun meta_highlight()
  (interactive)
  "highlight todos, notes and more"
  (highlight-regexp "@TODO" 'hi-pink)
  (highlight-regexp "@BUG" 'hi-red)
  (highlight-regexp "@HERE" 'hi-green)
  (highlight-regexp "@NOTE" 'hi-blue))
(add-hook 'find-file-hook (lambda () (meta_highlight)))


(display-time-mode 1) ;; This status line is not great, improve on clarity of information displayed.
;; @BUG -  disable cursor blinking or evil mode in the doc-view buffer, cursor blinking disabled
;; completely
(blink-cursor-mode -1)
(global-hl-line-mode 1) ;; highlight current line
;; disable all GUI bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(set-frame-font "Hack 10" nil t)
;;(set-frame-font "FiraCode Nerd Font Mono 8" nil t)

;; does not display line numbers by default, ps: linum-mode is very slow don't use
(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Indent with of four and use tab to allow indentation
;; use M-i to insert tab
(setq-default tab-width 4 indent-tabs-mode t)

;; Autrowrap 120
(setq-default fill-column 120)
(setq auto-fill-mode t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook (lambda ()
						   (turn-off-auto-fill)
						   (visual-line-mode)))
(setq org-cycle-emulate-tab 'white)


;; Org
;; ------------------------------------------------------------
;; @TODO :: beamer -> template for documents in org-mode (latex and beamer)
;; @TODO :: feature -> (org today/daily) -- a dashboard for tracking daily todos/goals and what was done
(setq org_files (list "log/src/orthodontics.org"
                      "log/src/research.org"
                      "log/src/chaos.org"
                      "log/src/life.org"))
(setq org-agenda-files (mapcar (lambda (x) (concat source_dir x)) org_files))
(setq org-log-done t)
(setq org-image-actual-width nil) ;;To set image scale
(add-hook 'org-mode-hook 'org-indent-mode) ;; @BUG doesnt work...
;; @TODO remove fill mode for org and just wrap line

;; Diary and calendar
;; ------------------------------------------------------------
;; function/shortcuts to implement: open calendar, edit diary, view/update diary, link diary to org-agenda/today.org


;; Dired
;; ------------------------------------------------------------
;;@TODO setup keybind for open-with and specify app
(setq dired-listing-switches "-l")
;; use external app to open file from dired, taken from xah lee
(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that.
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


;; Backup
;; ------------------------------------------------------------
;; Place backup files in a specific folder
;; Put backup files neatly away
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


;; Base keybindings
;; ------------------------------------------------------------
;; some other kbd
(global-set-key (kbd "C-; n") 'make-frame)
(global-set-key (kbd "C-; c") 'delete-frame)
(global-set-key (kbd "C-; b") 'ibuffer)

;; org
(eval-after-load "org" '(progn
						  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
						  (define-key org-mode-map (kbd "C-; /") 'counsel-org-goto-all)))
(add-hook 'org-beamer-mode-hook
		  (lambda () (local-set-key (kbd "C-; e") 'org-beamer-select-environment)))

(eval-after-load "dired" '(progn
			    (define-key dired-mode-map (kbd "C-; o") 'xah-open-in-external-app) ))

;; @TODO figure out how to fix eshell read-only mode
(global-set-key (kbd "C-; e") 'eshell)
(global-set-key (kbd "C-; M-e") 'eshell-command)
(global-set-key (kbd "C-; l") 'display-line-numbers-mode)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)


;; Eshell
;; ------------------------------------------------------------
;; minibuffer command
;; todo make the output of the command open in a new frame
;; run and build commands
;; @BUG to change the eshell prompt you need to change the regexp
(setq eshell-prompt-function
      (lambda ()
        (concat (eshell/pwd) "\n $ ")))
(setq eshell-highlight-prompt nil)

;; Programming
;; ------------------------------------------------------------
(icomplete-mode t)
(add-hook 'python-mode-hook
    (lambda ()
	    (setq-default indent-tabs-mode nil)
	    (setq-default tab-width 4)
	    (setq-default py-indent-tabs-mode t)
		(add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; How to load ipython with these commands and
;; %load_ext autoreload
;; %autoreload 2


;; Startup
;; ------------------------------------------------------------
(setq inhibit-startup-screen t)
;;(mapcar 'find-file-noselect default_buffers) ;; open silently all default buffers
(cd source_dir)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; Web and RSS
;; ------------------------------------------------------------
;; @TODO :: shortcuts for quick searches in ArXiv, PubMed, Wikipedia and others
;; from an interactive command (use eww)


;; Extras
;; ------------------------------------------------------------
;; try to load extras
(condition-case nil
    (load-file (concat source_dir "/mr-emacs/extra.el"))
  (error (message-box "Could not load extras...")))



;;; Uncomment if needed
;;; It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(define-key global-map (kbd "<f8>") 'unfill-paragraph)

;; @TODO :: writer's wrap ;; one line per sentence for orgmode and latex (can be set optionally and applied to the buffer)


;; @TODO see if I can make this work better
;; Own function to call Antidote on the buffer at hand
;; https://www.reddit.com/r/emacs/comments/dtciau/comment/hlscgqr/
(defun Antidote ()
(interactive)
(save-buffer)
(write-region (point-min) (point-max) (concat buffer-file-name ".backup"))
(auto-revert-mode)
(let ((CmdStr (concat "Antidote \"" buffer-file-name "\" &")))
(shell-command CmdStr nil nil))
(redraw-frame) (redraw-display)
)
