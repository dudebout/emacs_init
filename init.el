(add-to-list 'load-path "~/repositories/emacs_init")
(add-to-list 'load-path "~/repositories/emacs_priv")
(require 'ddb-paths)
(require 'ddb-accounts)

(load custom-file 'noerror)

(require 'ddb-interactives)
(require 'ddb-conf)

(setq ddb/packages '(paredit smex ido-ubiquitous auctex org zenburn-theme magit haskell-mode twittering-mode auto-complete yasnippet idle-highlight-mode anything anything-config))

(ddb/conf/install-packages ddb/packages)

(load-theme 'zenburn)

(ddd/conf/general-behavior)
(ddb/conf/copy-cut-line-at-point)
(ddb/conf/ask-before-suspend)
(ddb/conf/activate-disabled-functions)
(ddb/conf/global-set-keys)
(ddb/conf/external-programs)

(ddb/conf/ibuffer)
(ddb/conf/dired)
(ddb/conf/uniquify)
(ddb/conf/prog)
(ddb/conf/text)
(ddb/conf/saveplace)
(ddb/conf/bibtex)
(ddb/conf/gnus)
(ddb/conf/latex)
(ddb/conf/org)
(ddb/conf/paredit)
(ddb/conf/smex)
(ddb/conf/twittering)
(ddb/conf/ffap-latex)
(ddb/conf/auto-complete)
(ddb/conf/yasnippet)
(ddb/conf/ido)
(ddb/conf/changelog)
(ddb/conf/anything)

(require 'ddb-temp)
