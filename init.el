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

(ddb/conf/prog)
(ddb/conf/text)
(ddb/conf/comint)
(ddb/conf/elisp)
(ddb/conf/lexbind)
(ddb/conf/clojure)
(ddb/conf/haskell)
(ddb/conf/python)
(ddb/conf/latex)

(ddb/conf/multiple-cursors)
(ddb/conf/ibuffer)
(ddb/conf/dired)
(ddb/conf/dired-details)
(ddb/conf/dired+)
(ddb/conf/gnus)
(ddb/conf/org)

(require 'ddb-temp)

(add-to-list 'load-path "~/external/use-package/")
(require 'use-package)

(defvar auto-use-package-symbol-string "auto-use-package/%s/%s")

(defun auto-use-package-symbol (keyword package-name)
  (intern (format auto-use-package-symbol-string keyword package-name)))

; Add a debug statement for explicit statement of what is loaded exactly
(defmacro auto-use-package (package-name &rest args)
  (cl-letf ((auto-config (auto-use-package-symbol "config" package-name))
            (auto-init (auto-use-package-symbol "init" package-name)))
    (let* ((auto-bind (auto-use-package-symbol "bind" package-name))
           (auto-init/arg (when (fboundp auto-init)
                            `(:init (,auto-init))))
           (auto-config/arg (when (fboundp auto-config)
                            `(:config (,auto-config))))
           (auto-bind/arg (when (boundp auto-bind)
                            `(:bind ,auto-bind))))
      `(use-package ,package-name
         ,@auto-bind/arg
         ,@auto-init/arg
         ,@auto-config/arg
         ,@args))))

(setq auto-use-package-symbol-string "ddb/%s/%s")

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

(auto-use-package ace-jump-mode)
(auto-use-package auto-complete-config)
(auto-use-package rainbow-delimiters)
(auto-use-package bibtex :defer t)
(auto-use-package expand-region)
(auto-use-package helm-files)
(auto-use-package helm-misc)
(auto-use-package helm-descbinds)
(auto-use-package ido)
(auto-use-package ido-ubiquitous)
(auto-use-package magit)
(auto-use-package projectile)
(auto-use-package smex)
(auto-use-package twittering)
(auto-use-package git-gutter)
(auto-use-package window-number)
(auto-use-package yasnippet)

(auto-use-package octave-mod :mode ("\\.m\\'" . octave-mode))
(auto-use-package less-css-mode :mode ("\\.m\\'" . less-css-mode))
(auto-use-package yaml-mode :mode ("\\.yaml\\'" . yaml-mode))


(ddb/conf/shakespearean)
(ddb/conf/changelog)
(ddb/conf/ffap-latex)
