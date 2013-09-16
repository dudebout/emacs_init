(add-to-list 'load-path "~/repositories/emacs_init")
(add-to-list 'load-path "~/repositories/emacs_priv")
(require 'ddb-paths)
(require 'ddb-accounts)

(load custom-file 'noerror)

(require 'ddb-interactives)
(require 'ddb-conf)

(load-theme 'zenburn)

(setq ddb/packages '(ace-jump-mode paredit smex ido-ubiquitous auctex org haml-mode haskell-mode twittering-mode auto-complete idle-highlight-mode expand-region minimap ssh-config-mode rainbow-delimiters rainbow-mode dired+ bookmark+ dired-details multiple-cursors less-css-mode yaml-mode window-number elisp-slime-nav git-gutter ghci-completion git-commit-mode gitconfig-mode gitignore-mode projectile helm highlight-cl redshank helm-descbinds jump-char elpy lein nrepl ac-nrepl lexbind-mode diminish))

;; for ido-ubiquitous
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(ddb/conf/install-packages ddb/packages)

(ddb/conf/general-behavior)
(ddb/conf/proced)
(ddb/conf/copy-cut-line-at-point)
(ddb/conf/ask-before-suspend)
(ddb/conf/global-set-keys)
(ddb/conf/external-programs)

(ddb/conf/uniquify)
(ddb/conf/saveplace)
(ddb/conf/helm)
(ddb/conf/projectile)
(ddb/conf/smex)
(ddb/conf/ffap-latex)
(ddb/conf/yasnippet)
(ddb/conf/auto-complete)
(ddb/conf/ido)
(ddb/conf/ido-ubiquitous)
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
(ddb/conf/bibtex)
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
;(ddb/conf/magit)
(ddb/conf/org)
(ddb/conf/twittering)
(ddb/conf/git-gutter)



(add-to-list 'load-path "~/external/use-package/")
(require 'use-package)

;; (use-package magit
;;   :bind ddb/bind/magit
;;   :config ddb/conf/magit)

;; (defmacro ddb/use-package (package-name)
;;   `(let ((ddb/bind (eval (intern (concat "ddb/bind/" (symbol-name (quote ,package-name)))))))
;;        (use-package ,package-name
;;          :bind ddb/bind)))

;; (defmacro ddb/use-package (package-name)
;;   (let ((ddb/bind (intern (format "ddb/bind/%s" package-name))))
;;     (cl-letf ((ddb/config (intern (format "ddb/conf/%s" package-name))))
;;       `(use-package ,package-name
;;          :bind ,ddb/bind
;;          :config ,ddb/config))))

(defmacro ddb/use-package (package-name &rest args)
  (cl-letf ((ddb/config (intern (format "ddb/conf/%s" package-name))))
    (let* ((ddb/bind (intern (format "ddb/bind/%s" package-name)))
           (ddb/config/arg (when (fboundp ddb/config)
                            `(:config ,ddb/config)))
           (ddb/bind/arg (when (boundp ddb/bind)
                            `(:bind ,ddb/bind))))
      `(use-package ,package-name
         ,@ddb/bind/arg
         ,@ddb/config/arg
         ,@args))))

;; (defmacro auto-require (package-name)
(require 'ddb-temp)

(ddb/use-package magit)
