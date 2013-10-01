(require 'use-package)

(defvar auto-use-package-symbol-string "auto-use-package/%s/%s")

(defun auto-use-package-symbol (keyword package-name)
  (intern (format auto-use-package-symbol-string keyword package-name)))

; Add a debug statement for explicit statement of what is loaded exactly
(defmacro auto-use-package (package-name &rest args)
  (cl-letf ((auto-init (auto-use-package-symbol "init" package-name))
            (auto-config (auto-use-package-symbol "config" package-name)))
    (let* ((auto-bind (auto-use-package-symbol "bind" package-name))
           (auto-mode (auto-use-package-symbol "mode" package-name))
           (auto-init/arg (when (fboundp auto-init)
                            `(:init (,auto-init))))
           (auto-config/arg (when (fboundp auto-config)
                            `(:config (,auto-config))))
           (auto-bind/arg (when (boundp auto-bind)
                            `(:bind ,(symbol-value auto-bind))))
           (auto-mode/arg (when (boundp auto-mode)
                            `(:mode ,(symbol-value auto-mode)))))
      `(use-package ,package-name
         ,@auto-init/arg
         ,@auto-config/arg
         ,@auto-bind/arg
         ,@auto-mode/arg
         ,@args))))

(provide 'auto-use-package)
