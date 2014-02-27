;;; General
;;;; LaTeX
(defun ddb/conf/latex ()
  (setq TeX-electric-sub-and-superscript t
        TeX-parse-self t
        TeX-save-query nil
        TeX-force-default-mode t
        TeX-source-correlate-method 'synctex
        preview-scale-function 1.5)

  ; this should be automatic in AucTeX but is not
  (add-hook 'latex-mode-hook 'TeX-latex-mode)

  (defun ddb/conf/latex/reftex ()
    (progn
      (turn-on-reftex)
      (reftex-set-cite-format 'default)
      (setq reftex-plug-into-AUCTeX t)))

  (defun ddb/conf/latex/synctex ()
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

  (add-to-list 'auto-mode-alist '("\\.tikz\\'" . latex-mode))

  (add-hook 'LaTeX-mode-hook 'ddb/conf/latex/reftex)
  (add-hook 'LaTeX-mode-hook 'ddb/conf/latex/reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-toggle-debug-warnings)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))
;;;; General behavior
(defun ddb/conf/general-behavior ()
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        x-select-enable-clipboard t
        make-backup-files nil
        mouse-yank-at-point t
        vc-follow-symlinks t
        confirm-kill-emacs nil
        minibuffer-depth-indicate-mode t
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
  (global-hl-line-mode)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'auto-tail-revert-mode 'tail-mode)

  (add-hook 'isearch-mode-end-hook 'recenter-top-bottom)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))
;;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.julius\\'" . javascript-mode))
;;;; keybindings
(defun ddb/conf/global-set-keys ()
  (winner-mode 1) ; C-c left = undo in window configuration
  (windmove-default-keybindings)
  (global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-S-<down>") 'shrink-window)
  (global-set-key (kbd "C-S-<up>") 'enlarge-window)
  (global-set-key (kbd "C-<tab>") 'bury-buffer)
  (global-set-key (kbd "M-m") 'jump-char-forward)
  (global-set-key (kbd "M-S-m") 'jump-char-backward)
  (global-set-key (kbd "<f7>") 'compile)
  (global-set-key (kbd "C-M-/") 'hippie-expand)
  (global-set-key (kbd "<f12>") 'ddb/display-org-agenda) ;; should be moved in a package requiring org
  (global-set-key (kbd "<f10>") 'linum-mode)
  (global-set-key (kbd "<f8>") 'menu-bar-mode)
  (global-set-key (kbd "C-a") 'ddb/beginning-of-line-or-indentation)
  (global-set-key (kbd "C-c k") 'ddb/kill-current-buffer-and-delete-file)
  (global-set-key (kbd "<f11>") 'ddb/toggle-selective-display)
  (global-set-key (kbd "C-x M-w") 'ddb/rename-current-buffer-file)
  (global-set-key (kbd "C-x F") 'ddb/find-file-as-root)
  (global-set-key (kbd "C-x M-e") 'ddb/eval-and-replace)
  (global-set-key (kbd "C-x M-b") 'ddb/swap-buffers-in-windows)
  (global-set-key (kbd "C-x M-r") 'ddb/rotate-windows)
  (global-set-key (kbd "C-x M-k") 'ddb/delete-current-buffer-and-delete-file)
  (global-set-key (kbd "C-x M-s") 'ddb/sudo-edit)
  (global-set-key (kbd "M-/") 'ddb/describe-symbol-at-point-in-popup)
  (global-set-key (vector 'remap 'goto-line) 'ddb/goto-line-with-feedback))
;;;; changelog
(defun ddb/conf/changelog ()
  (make-local-variable 'add-log-full-name)
  (make-local-variable 'add-log-mailing-address)
  (defun ddb/conf/changelog/set-git-name-and-email ()
    (when (and buffer-file-name
               (eq (vc-backend buffer-file-name) 'Git))
      (setq add-log-full-name (split-string (shell-command-to-string "git config user.name") "\n" t)
            add-log-mailing-address (split-string (shell-command-to-string "git config user.email") "\n" t))))
  (add-hook 'find-file-hook 'ddb/conf/changelog/set-git-name-and-email))
;;;; copy-cut-line-at-point
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
;;;; aske-before-suspend
(defun ddb/conf/ask-before-suspend ()
  (defadvice suspend-frame (before ask-before-suspend activate compile)
    "Asks before suspending emacs."
    (interactive
      (when (yes-or-no-p (format "Are you sure you want to suspend Emacs? "))))))
;;; Specific
;; could not put in auto-use-package
;; for some reason flymake bugs and there is no command to be run for the flymake process
;; (list processes will give " file.hs" with no command)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))


(defun ddb/init/artbollocks-mode ()
  (autoload 'artbollocks-mode "artbollocks-mode" nil t) ;; eventually move to ddb/commands/artbollocks-mode
)

(defun ddb/config/artbollocks-mode ()
  (setq artbollocks-weasel-words-list
        (delete-dups
         '( ;; from writegood-mode
           "many" "various" "very" "fairly" "several" "extremely" "exceedingly"
           "quite" "remarkably" "few" "surprisingly" "mostly" "largely" "huge"
           "tiny" "are a number" "is a number" "excellent" "interestingly"
           "significantly" "substantially" "clearly" "vast" "relatively"
           "completely" "literally" "not rocket science" "outside the box"
           ;; from sachachua
           "one of the" "should" "just" "sort of" "a lot" "probably" "maybe"
           "perhaps" "I think" "really" "pretty" "maybe" "nice" "utilize"
           "leverage" "many" "various" "very" "fairly" "several" "extremely"
           "exceedingly" "quite" "remarkably" "few" "surprisingly" "mostly"
           "largely" "huge" "tiny" "excellent" "interestingly" "significantly"
           "substantially" "clearly" "vast" "relatively" "completely"
           "is a number" "are a number")))

  (setq artbollocks-jargon-list '())

  (defun ddb/regexp-from-list (list)
    (concat "\\b" (regexp-opt list t) "\\b"))

  (setq artbollocks-weasel-words-regex
        (ddb/regexp-from-list artbollocks-weasel-words-list))

  (setq artbollocks-jargon-regex
        (ddb/regexp-from-list artbollocks-jargon-list)))

(defun ddb/init/eproject ()
  (define-project-type latex (generic) (look-for "Makefile"))
  (add-hook 'latex-project-file-visit-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (format "cd %s; make" (eproject-root)))
              (when eproject-TeX-master
                (setq TeX-master (format "%s%s" (eproject-root) eproject-TeX-master)))))
  (defcustom eproject-TeX-master nil "Sets the TeX master file for eproject" :safe 'stringp))


(defun ddb/init/outshine ()
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))

(defun ddb/init/paredit ()
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(defun ddb/init/lexbind-mode ()
  (add-hook 'emacs-lisp-mode-hook 'lexbind-mode))

(defun ddb/init/redshank ()
  (add-hook 'emacs-lisp-mode-hook 'redshank-mode))

(defun ddb/init/elisp-slime-nav ()
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(defun ddb/init/eldoc ()
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(defun ddb/config/haskell-mode ()
  (defun ddb/hook/haskell-mode ()
    (subword-mode)
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indent)
    (setq haskell-program-name "cabal repl"))

  (add-hook 'haskell-mode-hook 'ddb/hook/haskell-mode)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
  (autoload 'turn-on-ghci-completion "ghci-completion")

  (eval-after-load "haskell-mode-map"
    '(define-key haskell-mode-map (kbd "C-;") 'haskell-hoogle)))


(defun ddb/config/term-mode ()
  (defun ddb/hook/term-mode ()
    (setq show-trailing-whitespace nil))
  (add-hook 'term-mode-hook 'ddb/hook/term-mode))

(setq ddb/bind/ace-jump-mode '("C-=" . ace-jump-mode)
      ddb/bind/expand-region '("C-'" . ex/expand-region)
      ddb/bind/gnus '("C-c g" . gnus)
      ddb/bind/helm-files '("C-c f" . helm-find-files)
      ddb/bind/helm-misc '("<f5>" . helm-mini)
      ddb/bind/ibuffer '("C-x C-b" . ibuffer)
      ddb/bind/magit '("C-c i" . magit-status)
      ddb/bind/magit-blame '("C-c M-l" . magit-blame-mode)
      ddb/bind/mu4e '("C-c m" . mu4e)
      ddb/bind/multiple-cursors '(("C-S-c C-S-c" . mc/edit-lines)
                                  ("C-S-c C-e" . mc/edit-ends-of-lines)
                                  ("C-S-c C-a" . mc/edit-beginnings-of-lines)
                                  ("C-S-c C-s" . mc/mark-all-in-region)
                                  ("C-\"" . mc/mark-next-like-this)
                                  ("C-|" . mc/mark-all-like-this)
                                  ("C-<" . mc/mark-previous-like-this)
                                  ("C->" . mc/mark-more-like-this-extended))
      ddb/bind/org '(("C-c a" . org-agenda)
                     ("C-c b" . org-iswitchb)
                     ("C-c l" . org-store-link))
      ddb/bind/org-capture '("C-c c" . org-capture)
      ddb/bind/smex '(("M-x" . smex)
                      ("M-X" . smex-major-mode-commands)
                      ("C-c C-c M-x" . execute-extended-command)))

(setq ddb/mode/css-mode '("\\.lucius\\'" . css-mode)
      ddb/mode/less-css-mode '("\\.less\\'" . less-css-mode)
      ddb/mode/markdown-mode '("\\.md\\'" . markdown-mode)
      ddb/mode/octave-mod '("\\.m\\'" . octave-mode)
      ddb/mode/yaml-mode '("\\.yaml\\'" . yaml-mode))

(defun ddb/config/proced ()
  (setq proced-auto-update-interval 2)

  (defun ddb/hook/proced-mode ()
    (setq proced-auto-update-flag t)
    (visual-line-mode -1))

  (add-hook 'proced-mode-hook 'ddb/hook/proced))

(defun ddb/config/bibtex ()
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

  ;; makes sure that the author names with accents do not make latex crash when
  ;; in a label
  (push '("ä" . "a") bibtex-autokey-name-change-strings)
  (push '("ï" . "i") bibtex-autokey-name-change-strings)
  (push '("ö" . "o") bibtex-autokey-name-change-strings)
  (push '("ş" . "s") bibtex-autokey-name-change-strings)

  (defun ddb/hook/bibtex-mode ()
    (setq fill-column 1000))

  (add-hook 'bibtex-mode-hook 'ddb/hook/bibtex-mode))

(defun ddb/config/magit ()
  (setq magit-set-upstream-on-push 'askifnotset
        magit-completing-read-function 'magit-ido-completing-read
        magit-save-some-buffers 'dontask
        magit-create-branch-behaviour 'at-point
        magit-log-auto-more t
        magit-process-popup-time 5
        magit-revert-item-confirm nil
        magit-rewrite-inclusive nil
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-status-buffer-switch-function 'switch-to-buffer)

  (setq-default diff-auto-refine-mode nil)
  (remove-hook 'magit-status-insert-sections-hook
               'magit-insert-status-tags-line))

(defun ddb/config/twittering-mode ()
  (setq twittering-use-master-password t
        twittering-icon-mode t))

(defun ddb/config/org ()
  ;; TEMPORARY http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16265
  (setq-default cache-long-scans nil)

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
        org-special-ctrl-a/e t
        org-special-ctrl-k t
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

  (defun ddb/default-directory-org-agenda ()
    (setq default-directory org-directory))

  (add-hook 'org-agenda-mode-hook 'ddb/default-directory-org-agenda)

  (eval-after-load "org"
    '(progn
       (require 'ox-latex)
       (require 'ddb-org-bib)
       (org-clock-persistence-insinuate)
       (add-to-list 'org-latex-classes
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

(defun ddb/config/erc-join ()
  (erc-autojoin-mode 1))

(defun ddb/config/erc-services ()
  (erc-services-mode 1))

(defun ddb/config/gnus ()
  (setq gnus-always-read-dribble-file t
        gnus-message-archive-method nil)

  (defun ddb/hook/gnus-group ()
    (gnus-topic-mode))

  (define-key gnus-summary-mode-map (kbd "<f9>") 'ddb/gnus-follow-gmane-link)

  (add-hook 'gnus-group-mode-hook 'ddb/hook/gnus-group))

(defun ddb/config/smex ()
  (smex-initialize))

(defun ddb/config/uniquify ()
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(defun ddb/conf/external-programs ()
  (setq browse-url-generic-program "firefox"
        browse-url-browser-function 'browse-url-generic)
  (setq-default ispell-program-name "hunspell"))

(defun ddb/init/saveplace ()
  (setq-default save-place t))

(defun ddb/config/dired ()
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

  (eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
       (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
       (define-key dired-mode-map (kbd ".") 'ddb/dired-toggle-show-all)
       (define-key dired-mode-map (kbd "C-c C-o") 'ddb/dired-open-file))))

(defun ddb/conf/dired-details ()
  (eval-after-load "dired"
    '(progn
       (require 'dired-details)
       (setq dired-details-hidden-string "[+] ")
       (dired-details-install)
       (define-key dired-mode-map "," 'dired-details-toggle))))

(defun ddb/conf/dired+ ()
  (eval-after-load "dired"
    '(require 'dired+)))

(defun ddb/config/ibuffer ()
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

  (defun ddb/hook/ibuffer-mode ()
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook 'ddb/hook/ibuffer-mode))

(defun ddb/config/ffap ()
  (setq ffap-kpathsea-depth 4)

  (defun ffap-latex-mode (name)
    (ffap-tex-init)
    ;; only rare need for ""
    (ffap-locate-file name '(".cls" ".sty" ".tex" ".bib" ".tikz" "") ffap-tex-path)))

(defun ddb/init/yasnippet ()
  (yas-global-mode 1))

(defun ddb/init/auto-complete-config ()
  (ac-config-default)
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-yasnippet))
  (ac-flyspell-workaround))

(defun ddb/init/ido ()
  (setq ido-create-new-buffer 'always
        ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.hi\\'")
        ido-enable-flex-matching t
        ido-use-virtual-buffers t
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t)
  (ido-mode t)
  (ido-everywhere t))

(defun ddb/init/ido-ubiquitous ()
  (ido-ubiquitous-mode t))

(defun ddb/init/ido-vertical-mode ()
  (ido-vertical-mode 1))

(defun ddb/init/rainbow-delimiters ()
  (global-rainbow-delimiters-mode))

(defun ddb/init/window-number ()
  (window-number-meta-mode))

(defun ddb/init/helm-descbinds ()
  (helm-descbinds-mode))

(defun ddb/init/projectile ()
  (projectile-global-mode))

(defun ddb/init/git-gutter ()
  (global-git-gutter-mode t))

(defun ddb/init/prog ()
  (defun ddb/hook/prog-mode ()
    (electric-pair-mode)
    (idle-highlight-mode t)
    (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (auto-fill-mode t))
  (add-hook 'prog-mode-hook 'ddb/hook/prog-mode))

(defun ddb/init/flyspell ()
  (defun ddb/hook/flyspell/text-mode ()
    (flyspell-mode))
  (defun ddb/hook/flyspell/prog-mode ()
    (flyspell-prog-mode))
  (add-hook 'text-mode-hook 'ddb/hook/flyspell/text-mode)
  (add-hook 'prog-mode-hook 'ddb/hook/flyspell/prog-mode))

(provide 'ddb-conf)
