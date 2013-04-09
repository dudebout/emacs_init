(defun ddb/conf/copy-cut-line-at-point ()
  (defadvice kill-ring-save (before copy-line-at-point activate compile)
    "When called with no active region, copy the line at point."
    (interactive
     (if (use-region-p)
         (list (region-beginning)
               (region-end))
       (progn
         (message "Copied line")
         (list (line-beginning-position)
               (line-beginning-position 2))))))

  (defadvice kill-region (before cut-line-at-point activate compile)
    "When called with no active region, kill the line at point."
    (interactive
     (if (use-region-p)
         (list (region-beginning)
               (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2))))))

(defun ddb/conf/ask-before-suspend ()
  (defadvice suspend-frame (before ask-before-suspend activate compile)
    "Asks before suspending emacs."
    (interactive
      (when (yes-or-no-p (format "Are you sure you want to suspend Emacs? "))))))

(defun ddb/conf/install-packages (package-list)
  "Install packages."
  (setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")))
  (package-initialize)
  (when (not package-archive-contents)
    (package-refresh-contents))
  (dolist (package package-list)
    (when (not (package-installed-p package))
      (package-install package))))

(defun ddb/conf/global-set-keys ()
  (winner-mode 1) ; C-c left = undo in window configuration
  (windmove-default-keybindings)
  (global-set-key (vector 'remap 'goto-line) 'ddb/goto-line-with-feedback)
  (global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-S-<down>") 'shrink-window)
  (global-set-key (kbd "C-S-<up>") 'enlarge-window)
  (global-set-key (kbd "C-<tab>") 'bury-buffer)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-=") 'ace-jump-mode)
  (global-set-key (kbd "C-'") 'er/expand-region)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c b") 'org-iswitchb)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c g") 'gnus)
  (global-set-key (kbd "C-c i") 'magit-status)
  (global-set-key (kbd "C-c k") 'ddb/kill-current-buffer-and-delete-file)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c m") 'mu4e)
  (global-set-key (kbd "<f6>") 'ddb/insert-math-latex)
  (global-set-key (kbd "<f7>") 'compile)
  (global-set-key (kbd "<f8>") 'menu-bar-mode)
  (global-set-key (kbd "<f10>") 'linum-mode)
  (global-set-key (kbd "<f11>") 'ddb/toggle-selective-display)
  (global-set-key (kbd "C-x C-r") 'ddb/rename-current-buffer-file)
  (global-set-key (kbd "C-x F") 'ddb/find-file-as-root)
  (global-set-key (kbd "C-x M-e") 'ddb/eval-and-replace)
  (global-set-key (kbd "C-x M-w") 'ddb/swap-buffers-in-windows)
  (global-set-key (kbd "C-x M-r") 'ddb/rotate-windows)
  (global-set-key (kbd "C-x M-k") 'ddb/delete-current-buffer-and-delete-file)
  (global-set-key (kbd "C-x M-s") 'ddb/sudo-edit))

(defun ddd/conf/general-behavior ()
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        x-select-enable-clipboard t
        make-backup-files nil
        mouse-yank-at-point t
        vc-follow-symlinks t
        confirm-kill-emacs nil
        visible-bell t
        reb-re-syntax 'string
        recentf-max-saved-items nil
        diff-switches "-u"
        use-dialog-box nil
        visual-line-fringe-indicators '(left-curly-arrow nil)
        confirm-nonexistent-file-or-buffer nil
        ediff-window-setup-function 'ediff-setup-windows-plain
        whitespace-style '(face trailing lines-tail tabs empty)
        whitespace-line-column nil
        bookmark-save-flag 1
        enable-recursive-minibuffers t
        sentence-end-double-space nil
        disabled-command-function nil
        compilation-read-command nil
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

  (setq-default fill-column 80
                show-trailing-whitespace t
                indicate-empty-lines t
                indicate-buffer-boundaries 'left
                indent-tabs-mode nil)

  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (column-number-mode t)
  (show-paren-mode 1)
  (global-auto-revert-mode t)
  (global-visual-line-mode)
  (display-battery-mode)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'auto-tail-revert-mode 'tail-mode)

  (add-hook 'isearch-mode-end-hook 'recenter-top-bottom)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

(defun ddb/conf/bibtex ()
  (setq bibtex-maintain-sorted-entries t
        bibtex-include-OPTkey nil
        bibtex-user-optional-fields nil
        bibtex-entry-delimiters 'braces
        bibtex-field-delimiters 'double-quotes
        bibtex-entry-format t
        bibtex-align-at-equal-sign t
        bibtex-autokey-titleword-length 0
        bibtex-autokey-year-length 4
        bibtex-autokey-names nil
        bibtex-autokey-name-separator "_"
        bibtex-autokey-name-year-separator ":"
        ;; these last two are needed to avoid having some separators showing up
        ;; when the title contains some uppercase (such as "{M}arkov).
        ;; in this case, the title acts as if being integrated even though the
        ;; title length should be 0
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-year-title-separator "")

  (defun ddb/conf/bibtex/disable-fill-column ()
    (setq fill-column 1000))

  (eval-after-load "bibtex"
    '(progn
       ;; makes sure that the author names with accents do not make latex crash
       ;; when in a label
       (push '("ä" . "a") bibtex-autokey-name-change-strings)
       (push '("ï" . "i") bibtex-autokey-name-change-strings)
       (push '("ö" . "o") bibtex-autokey-name-change-strings)
       (push '("ş" . "s") bibtex-autokey-name-change-strings)))


  (add-hook 'bibtex-mode-hook 'ddb/conf/bibtex/disable-fill-column))

(defun ddb/conf/magit ()
  (setq magit-set-upstream-on-push t
        magit-save-some-buffers 'dontask
        magit-create-branch-behaviour 'at-point
        magit-log-auto-more t
        magit-process-popup-time 5
        magit-revert-item-confirm nil
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-status-buffer-switch-function 'switch-to-buffer)

  (setq-default diff-auto-refine-mode nil))

(defun ddb/conf/twittering ()
  (setq twittering-use-master-password t
        twittering-icon-mode t))

(defun ddb/conf/elisp ()
  (autoload 'enable-paredit-mode "paredit")
  (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
  (require 'highlight-cl)
  (add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
  (add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)

  (defun ddb/conf/emacs-lisp/hook ()
    (elisp-slime-nav-mode t)
    (eldoc-mode)
    (enable-paredit-mode))

  (add-hook 'emacs-lisp-mode-hook 'ddb/conf/emacs-lisp/hook))

(defun ddb/conf/haskell ()
  (defun ddb/conf/haskell/hook ()
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indent)
    ;; temporary fix until haskell-mode is derived from prog-mode
    (ddb/conf/prog/hook))

  (add-hook 'haskell-mode-hook 'ddb/conf/haskell/hook)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
  (autoload 'turn-on-ghci-completion "ghci-completion")

  (eval-after-load "haskell-mode-map"
    '(define-key haskell-mode-map (kbd "C-;") 'haskell-hoogle)))

(defun ddb/conf/python ()
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode)))

(defun ddb/conf/latex ()
  (setq TeX-electric-sub-and-superscript t
        TeX-parse-self t
        TeX-save-query nil
        TeX-force-default-mode t
        TeX-source-correlate-method 'synctex
        preview-scale-function 1.5)

  (defun ddb/conf/latex/reftex ()
    (progn
      (turn-on-reftex)
      (reftex-set-cite-format 'default)
      (setq reftex-plug-into-AUCTeX t)))

  (defun ddb/conf/latex/synctex ()
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

  (add-to-list 'auto-mode-alist '("\\.tikz\\'" . latex-mode))

  (add-hook 'LaTeX-mode-hook 'ddb/conf/latex/reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-toggle-debug-warnings)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

(defun ddb/conf/octave ()
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

(defun ddb/conf/less ()
  (add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode)))

(defun ddb/conf/yaml ()
  (autoload 'yaml-mode "yaml-mode" "YAML major mode" t)
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(defun ddb/conf/shakespearean ()
  (add-to-list 'auto-mode-alist '("\\.hamlet\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.lucius\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.julius\\'" . javascript-mode)))

(defun ddb/conf/org ()
  (require 'org-protocol)
  (org-clock-persistence-insinuate)

  (setq org-startup-indented t
        org-use-speed-commands t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-ellipsis " [+]"
        org-clock-persist 'history
        org-log-done (quote time)
        org-log-note-clock-out t
        org-reverse-note-order t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-start-on-weekday nil
        org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-custom-commands
        '(("u" alltodo ""
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if 'scheduled 'deadline))))
           (org-agenda-overriding-header "Unscheduled TODO entries: "))
          ("w" tags-todo "@work"))
        org-capture-templates
        `(("r" "read" entry
           (file+headline org-default-notes-file ,(format "todo capture %s" system-name))
           "* TODO read %?%a\nSCHEDULED: %t")
          ("a" "Appointment" entry
           (file+headline org-default-notes-file ,(format "events capture %s" system-name))
           "* APPT %?\nSCHEDULED: %t")
          ("d" "Todo today" entry
           (file+headline org-default-notes-file ,(format "todo capture %s" system-name))
           "* TODO %?\nSCHEDULED: %t")
          ("w" "Waiting event" entry
           (file+headline org-default-notes-file ,(format "todo capture %s" system-name))
           "* WAITING %?\nSCHEDULED: %t")
          ("e" "Event" entry
           (file+headline org-default-notes-file ,(format "events capture %s" system-name))
           "* %?")
          ("n" "Note" entry
           (file+headline org-default-notes-file ,(format "notes capture %s" system-name))
           "* %?")
          ("o" "org-protocol" entry ;; 'w' for 'org-protocol'
           (file+headline org-default-notes-file ,(format "web capture %s" system-name))
           "* %^{Title}\n%i%c\n%u\n%i"))
        org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w)
                        ("@home" . ?h)
                        (:endgroup . nil)
                        ("emacs" . ?e)
                        ("haskell" . ?a)
                        ("website" . ?s)
                        ("contact" . ?c)
                        ("bug" . ?b))
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d)")
                            (sequence "APPT(a)" "|" "MET(m)")
                            (sequence "READ(r)" "WATCH(a)" "|" "CONSUMED(c)")
                            (sequence "PROJECT(p)" "STARTED(s)" "|" "FINISHED(f)"))
        org-export-latex-hyperref-format "\\cref{%s}"
        org-export-latex-href-format "\\href{%s}{\\nolinkurl{%s}}"
        org-format-latex-options '(:foreground default
                                               :background default
                                               :scale 1.5
                                               :html-foreground "Black"
                                               :html-background "Transparent"
                                               :html-scale 1.0
                                               :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        org-export-latex-packages-alist '(("" "ddbsymb" t))
        org-format-latex-header-extra "\\bibliographystyle{plain}\n\\bibliography{ddb}"
        org-export-with-LaTeX-fragments t
        org-link-abbrev-alist `(("citation" . ,ddb/org-bib/citation)
                                ("paper" . ,ddb/org-bib/paper)
                                ("notes" . ,ddb/org-bib/notes)))

  (defun ddb/conf/org-reftex ()
    (org-defkey org-mode-map (kbd "C-c [") 'undefined)
    (org-defkey org-mode-map (kbd "C-c ]") 'undefined)
    (autoload 'reftex-citation "reftex")
    (org-defkey org-mode-map (kbd "C-c [") 'reftex-citation)
    ;; using reftex-set-cite-format requires the visited buffer to
    ;; be linked to a file
    (setq reftex-cite-format "[[bib:%l]]"))

  (add-hook 'org-mode-hook 'ddb/conf/org-reftex)

  (eval-after-load "org"
    '(progn
       (require 'org-latex)
       (require 'org-special-blocks)
       (require 'ddb-org-bib)
       (add-to-list 'org-export-latex-classes
                    '("ieeecdc"
                      "\\documentclass{ieeecdc}
\\usepackage[AUTO]{inputenc}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]"
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                      ("\\paragraph{%s}" . "\\paragraph*{%s}"))))))

(defun ddb/conf/gnus ()
  (setq gnus-always-read-dribble-file t
        gnus-message-archive-method nil)

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(defun ddb/conf/smex ()
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun ddb/conf/uniquify ()
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(defun ddb/conf/external-programs ()
  (setq browse-url-generic-program "conkeror"
        browse-url-browser-function 'browse-url-generic)
  (setq-default ispell-program-name "hunspell"))

(defun ddb/conf/saveplace ()
  (require 'saveplace)
  (setq-default save-place t))

(defun ddb/conf/prog ()
  (defun ddb/conf/prog/hook ()
    (electric-pair-mode)
    (idle-highlight-mode t)
    (flyspell-prog-mode)
    (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (auto-fill-mode t)
    (hl-line-mode t))
  (add-hook 'prog-mode-hook 'ddb/conf/prog/hook))

(defun ddb/conf/text ()
  (defun ddb/conf/text/hook ()
    (flyspell-mode))
  (add-hook 'text-mode-hook 'ddb/conf/text/hook))

(defun ddb/conf/dired ()
  ;; needed to have 'C-x C-j' bound properly
  (require 'dired)
  (setq dired-listing-switches "-lh"
        dired-dwim-target t)

  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (next-line 1))

  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (next-line -1))

  (define-key dired-mode-map
    (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)


  (eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (kbd ".") 'ddb/dired-toggle-show-all)
       (define-key dired-mode-map (kbd "C-c C-o") 'ddb/dired-open-file))))

(defun ddb/conf/dired-details ()
  (require 'dired-details)
  (setq dired-details-hidden-string "[+] ")
  (dired-details-install)
  (define-key dired-mode-map "," 'dired-details-toggle))

(defun ddb/conf/dired+ ()
  (require 'dired+))

(defun ddb/conf/ibuffer ()
  (require 'gnus)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  (setq ibuffer-saved-filter-groups
        '(("default"
           ("git:ddb"
            (or
             (filename . "ddb/ddb")
             (name . "ddb")))
           ("git:emacs_init"
            (or
             (filename . "emacs_init")
             (name . "emacs_init")))
           ("git:notebook"
            (or
             (filename . "notebook")
             (name . "notebook")))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           ("gnus" (or
                    (mode . message-mode)
                    (mode . gnus-group-mode)
                    (mode . gnus-summary-mode)
                    (mode . gnus-article-mode)
                    (name . "gnus")
                    (name . "^\\.newsrc-dribble"))))))

  (defun ddb/conf/ibuffer/set-initial-group ()
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook 'ddb/conf/ibuffer/set-initial-group))

(defun ddb/conf/multiple-cursors ()
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-S-c C-s") 'mc/mark-all-in-region)

  (global-set-key (kbd "C-\"") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-|") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-more-like-this-extended))

(defun ddb/conf/ffap-latex ()
  (setq ffap-kpathsea-depth 4)

  (eval-after-load "ffap"
    '(defun ffap-latex-mode (name)
       (ffap-tex-init)
       ;; only rare need for ""
       (ffap-locate-file name '(".cls" ".sty" ".tex" ".bib" ".tikz" "") ffap-tex-path))))

(defun ddb/conf/auto-complete ()
  (require 'auto-complete-config)
  (ac-config-default)
  (ac-flyspell-workaround))

(defun ddb/conf/yasnippet ()
  (require 'yasnippet)
  (yas/load-directory yas/root-directory)
  (yas/global-mode))

(defun ddb/conf/ido ()
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-create-new-buffer 'always
        ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.hi\\'")
        ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t))

(defun ddb/conf/ido-ubiquitous ()
  (ido-ubiquitous-mode t))

(defun ddb/conf/changelog ()
  (make-local-variable 'add-log-full-name)
  (make-local-variable 'add-log-mailing-address)
  (defun ddb/conf/changelog/set-git-name-and-email ()
    (when (and buffer-file-name
               (eq (vc-backend buffer-file-name) 'Git))
      (setq add-log-full-name (split-string (shell-command-to-string "git config user.name") "\n" t)
            add-log-mailing-address (split-string (shell-command-to-string "git config user.email") "\n" t))))
  (add-hook 'find-file-hook 'ddb/conf/changelog/set-git-name-and-email))

(defun ddb/conf/rainbow-delimiters ()
  (global-rainbow-delimiters-mode))

(defun ddb/conf/window-number ()
  (require 'window-number)
  (window-number-meta-mode))

(defun ddb/conf/helm ()
  (global-set-key (kbd "C-c f") 'helm-find-files)
  (global-set-key (kbd "<f5>") 'helm-mini))

(defun ddb/conf/projectile ()
  (projectile-global-mode))

(defun ddb/conf/git-gutter ()
  (global-git-gutter-mode t))

(defun ddb/conf/comint ()
  (defun ddb/conf/comint/disable-whitespace-mode ()
    (setq show-trailing-whitespace nil))
  (add-hook 'term-mode-hook 'ddb/conf/comint/disable-whitespace-mode))

(provide 'ddb-conf)
