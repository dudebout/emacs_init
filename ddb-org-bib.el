(require 'org)

(org-add-link-type "bib" 'ddb/org-bib-open 'ddb/org-bib-export)

(defun ddb/org-bib-open (bib-key &optional in-emacs)
  (progn
    (message "Choose a type: citation(c), paper(p), notes(n):")
    (let* ((type_string (char-to-string (read-char-exclusive)))
           (type (cond ((string= "c" type_string) "citation")
                       ((string= "p" type_string) "paper")
                       ((string= "n" type_string) "notes"))))
        (org-open-link-from-string (format "[[%s:%s]]" type bib-key) in-emacs))))

(defun ddb/org-bib-export (path desc format)
  ;; remember that desc has been pre-escaped for latex
  (when (string= desc (format "bib:%s" path))
    (setq desc path))
  (cond
   ((eq format 'latex) (format "\\cite{%s}" path))
   (t desc)))

(provide 'ddb-org-bib)
