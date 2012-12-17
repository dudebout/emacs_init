(defun ddb/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun ddb/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun ddb/sudo-edit (&optional other-file)
  "Edit the current file (or another if prefix given) with root privileges by using sudo."
  (interactive "P")
  (if (or other-file
          (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ddb/unfill-paragraph ()
  "Unfill current paragraph or selected region."
  (interactive)
  (let ((fill-paragraph-function nil)
        (fill-column (point-max)))
    (if (use-region-p)
        (fill-paragraph nil t)
      (fill-paragraph nil))))

(defun ddb/swap-buffers-in-windows ()
  "Swap the buffers from the selected window and next window."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun ddb/delete-current-buffer-and-delete-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun ddb/dired-toggle-show-all ()
  "Toggle the -A flag for ls in dired."
  (interactive)
  (dired-sort-other
   (if (equal (substring dired-actual-switches -1) "A")
       (substring dired-actual-switches 0 -1)
     (concat dired-actual-switches "A"))))

(defun ddb/dired-open-file (arg)
  "Abuse org-open-file to open a file at point externally."
  (interactive "P")
  (require 'org)
  (require 'ffap)
  (org-open-file (or (ffap-guesser)
                     (error "No filename at point")) arg))

(defun ddb/insert-math-latex (&optional displayed)
  "Insert \(\) (\[\] if called with prefix) at point."
  (interactive "P*")
  (if displayed
      (insert "\n\\[\n\n\\]")
    (insert "\\(\\)"))
  (if displayed
      (backward-char 3)
    (backward-char 2)))

(defun ddb/toggle-selective-display (&optional column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(provide 'ddb-interactives)
