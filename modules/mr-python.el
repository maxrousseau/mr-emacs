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
