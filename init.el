(add-to-list 'load-path "~/repositories/emacs_init")
(add-to-list 'load-path "~/repositories/emacs_priv")
(require 'ddb-paths)
(require 'ddb-accounts)

(load custom-file 'noerror)

(require 'ddb-interactives)
(require 'ddb-conf)

(setq ddb/packages '(ace-jump-mode paredit smex ido-ubiquitous auctex org zenburn-theme magit haml-mode haskell-mode twittering-mode auto-complete yasnippet idle-highlight-mode anything anything-config expand-region))

(ddb/conf/install-packages ddb/packages)

(load-theme 'zenburn)

(ddd/conf/general-behavior)
(ddb/conf/copy-cut-line-at-point)
(ddb/conf/ask-before-suspend)
(ddb/conf/global-set-keys)
(ddb/conf/external-programs)

(ddb/conf/uniquify)
(ddb/conf/saveplace)
(ddb/conf/anything)
(ddb/conf/smex)
(ddb/conf/ffap-latex)
(ddb/conf/auto-complete)
(ddb/conf/yasnippet)
(ddb/conf/ido)
(ddb/conf/changelog)

(ddb/conf/prog)
(ddb/conf/text)
(ddb/conf/elisp)
(ddb/conf/haskell)
(ddb/conf/latex)
(ddb/conf/bibtex)

(ddb/conf/ibuffer)
(ddb/conf/dired)
(ddb/conf/gnus)
(ddb/conf/twittering)
(ddb/conf/org)

(require 'ddb-temp)
