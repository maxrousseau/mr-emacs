;;; today-mode.el --- major mode for daily planning  -*- lexical-binding: t; -*-
;;; a minimal task tracker

(define-derived-mode today-mode text-mode "today"
  "Major mode for tracking daily tasks."
  (setq font-lock-defaults '((("\\[\\([^]]+\\)\\]" 1 font-lock-type-face)   ; Matches text inside brackets
                              ("\\<todo\\>" . font-lock-keyword-face)
							  ("\\<wip\\>" . font-lock-keyword-face)
							  ("\\<done\\>" . font-lock-keyword-face))
                             nil nil nil nil))
  (use-local-map today-mode-map))

;; CONFIGURATION ================================================================================
;; open buffer and create state
(defvar date-var (format-time-string "%d-%m-%Y"))
(defvar status-var '(todo wip done))
(defvar today-state-var nil
  "Global variable to store the state of 'today', loaded conditionally.")

;; STATE MANAGEMENT ================================================================================
(defun open-daily-tasks ()
  "Open a buffer for today's tasks."
  (interactive)

  (when-let ((buffer (get-buffer date-var))) ;; kill the buffer if it exists for refresh
	(kill-buffer buffer))

  (let ((buffer-name date-var))
    (switch-to-buffer buffer-name)
    (unless (eq major-mode 'today-mode)
      (today-mode))
      (insert (concat date-var "\n\n"))
      (insert "🎯 Tasks:\n")
	  (dotimes (index (length today-state-var))
		(insert (concat "\t" (fmt-task (nth index today-state-var)))))
	  (read-only-mode 1)))

(defun today-refresh ()
  "docstring"
  (interactive)
  (let ((cursor-position (point)))
	(open-daily-tasks)
	(goto-char cursor-position)
	)
  (save-list-of-hash-tables-to-file today-state-var "~/.today_cache")) ;; auto-save state upon modification



(defun fmt-task (task-hash)
  (format "(%s)> [%s] %s : %s \t %s \n"
		  (gethash "id" task-hash)
		  (gethash "created" task-hash)
		  (nth (gethash "status" task-hash) status-var)
		  (gethash "name" task-hash)
		  (gethash "type" task-hash))
)

;; load state on opening (state)
(defun create-task ()
  "docstring"
  (interactive)
  (let ((name (read-string "task name: "))
		(type (completing-read "type: " '("🧪research" "✍️personal" "☁️other")))
		(id (+ 1 (length today-state-var)))
		(new-item (make-hash-table :test 'equal)))
	(puthash "id" id new-item)
	(puthash "name" name new-item)
	(puthash "type" type new-item)
	(puthash "created" date-var new-item)
	(puthash "status" 0 new-item)
	(push new-item today-state-var)
	(today-refresh)
	))

(defun toggle-task ()
  "docstring"
  (interactive)
  (let ((task-id (extract-number-at-point))
		(num-item (length today-state-var)))
	(setq current-status (gethash "status" (nth (- num-item task-id) today-state-var)))

	(if (< current-status 2)
		(setq current-status (+ 1 current-status))
	  (setq current-status 0))

	(puthash "status" current-status (nth (- num-item task-id) today-state-var)))
  (today-refresh))

(defun extract-number-at-point ()
  "Extracts a number enclosed in parentheses at the beginning of the line at point."
  (interactive)
  (save-excursion  ; Preserve the original cursor position
    (let ((line-content (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))

	  (if (string-match ".*?(\\([0-9]+\\))>" line-content)
          (progn
            (setq number (string-to-number (match-string 1 line-content)))  ; Convert and store the number
            number)  ; Return the number as the function result
        (progn
          (message "No number found at point!")
          nil)))))  ; Return nil if no number is found

(defun delete-task ()
  "delete task which is under the cursor"
  (interactive)

  (let ((task-id (extract-number-at-point))
		(num-item (length today-state-var)))
	(if (y-or-n-p  "Delete current task?")
		(progn
		  (setq today-state-var (remove (nth (- num-item task-id) today-state-var) today-state-var))
		  (message "task deleted"))
	  (message "task deletion aborted")))
  (today-refresh))

(defun save-list-of-hash-tables-to-file (list-of-hash-tables file-path)
  "Save a list of hash tables (LIST-OF-HASH-TABLES) to a file (FILE-PATH)."
  (interactive "File to save list of hash tables: ")
  (with-temp-file file-path
    (prin1 list-of-hash-tables (current-buffer))))

(defun load-list-of-hash-tables-from-file (file-path)
  "Load a list of hash tables from a file (FILE-PATH)."
  (interactive "File to load list of hash tables from: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (read (current-buffer))))

(defun edit-task ()
  "docstring"
  (interactive)
  (let ((task-id (extract-number-at-point))
		(num-item (length today-state-var))
		(new-name (read-string "edit: ")))
	(puthash "name" new-name (nth (- num-item task-id) today-state-var))
	)
  (today-refresh))

;; KEYBINDINGS ================================================================================
(defvar today-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Define keybindings
    (define-key map (kbd "C-c C-c") 'create-task)
    (define-key map (kbd "C-c C-d") 'delete-task)
	(define-key map (kbd "C-c C-t") 'toggle-task)
	(define-key map (kbd "C-c C-e") 'edit-task)
    map)
  "Keymap for `today-mode'.")

;; STARTUP  ================================================================================
;; load previous state
(if (file-exists-p (expand-file-name "~/.today_cache"))
	(setq today-state-var (load-list-of-hash-tables-from-file "~/.today_cache"))
  (setq today-state-var ()))

;; @TODO ================================================================================
;; eye candy (centered buffer, emoji for status and flags, fonts and etc.) + progressbar
;; highlight overdue task (created more than 7 days ago).
;; tabs to categorize tasks
;; add a task description (\n\t\t description)
;; center the window https://github.com/anler/centered-window-mode
