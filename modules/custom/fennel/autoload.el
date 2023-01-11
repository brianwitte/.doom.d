;;; custom/fennel/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun fnl-eval-sexp-at-point ()
  (interactive)
  (let ((evil-move-beyond-eol t))
    (save-excursion
      (when (looking-at "(\\|\\[\\|{")
        (forward-char))
      (goto-char (plist-get (sp-get-enclosing-sexp) :end))
      (call-interactively 'lisp-eval-last-sexp))))
