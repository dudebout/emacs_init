(add-to-list 'load-path "~/repositories/emacs_init")
(add-to-list 'load-path "~/repositories/emacs_priv")
(require 'ddb-paths)
(require 'ddb-accounts)

(load custom-file 'noerror)

(require 'ddb-interactives)
(require 'ddb-conf)

(load-theme 'zenburn)

(require 'cask "~/external/cask.el/cask.el")
(cask-setup "~/repositories/emacs_init")
(epl-initialize)


(ddb/conf/general-behavior)
(ddb/conf/copy-cut-line-at-point)
(ddb/conf/ask-before-suspend)
(ddb/conf/global-set-keys)
(ddb/conf/external-programs)

(ddb/conf/proced)
(ddb/conf/uniquify)
(ddb/conf/saveplace)

(ddb/conf/helm)
(ddb/conf/projectile)
(ddb/conf/ffap-latex)
(ddb/conf/yasnippet)
(ddb/conf/auto-complete)
(ddb/conf/ido)
(ddb/conf/changelog)
(ddb/conf/rainbow-delimiters)
(ddb/conf/window-number)

(ddb/conf/prog)
(ddb/conf/text)
(ddb/conf/comint)
(ddb/conf/elisp)
(ddb/conf/lexbind)
(ddb/conf/clojure)
(ddb/conf/haskell)
(ddb/conf/python)
(ddb/conf/latex)
(ddb/conf/octave)
(ddb/conf/less)
(ddb/conf/yaml)
(ddb/conf/shakespearean)

(ddb/conf/multiple-cursors)
(ddb/conf/ibuffer)
(ddb/conf/dired)
(ddb/conf/dired-details)
(ddb/conf/dired+)
(ddb/conf/gnus)

(ddb/conf/org)
(ddb/conf/twittering)
(ddb/conf/git-gutter)

(require 'ddb-temp)

(add-to-list 'load-path "~/external/use-package/")
(require 'use-package)

; Add a debug statement for explicit statement of what is loaded exactly
(defmacro ddb/use-package (package-name &rest args)
  (cl-letf ((ddb/config (intern (format "ddb/config/%s" package-name))))
    (let* ((ddb/bind (intern (format "ddb/bind/%s" package-name)))
           (ddb/config/arg (when (fboundp ddb/config)
                            `(:config (,ddb/config))))
           (ddb/bind/arg (when (boundp ddb/bind)
                            `(:bind ,ddb/bind))))
      `(use-package ,package-name
         ,@ddb/bind/arg
         ,@ddb/config/arg
         ,@args))))

(defmacro jit-require (package-symbol &rest args)
  (let ((name (intern (format "%s" (cadr package-symbol)))))
    `(ddb/use-package ,name  ,@args)))

; Add an error message if package not found
(defmacro soft-jit-require (package-symbol &rest args)
  (when (locate-library (format "%s" (cadr package-symbol)))
    `(jit-require ,package-symbol ,@args)))

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

(soft-jit-require 'ace-jump-mode)
(soft-jit-require 'bibtex :defer t)
(soft-jit-require 'expand-region)
(soft-jit-require 'ido-ubiquitous)
(soft-jit-require 'magit)
(soft-jit-require 'smex :defer nil)
