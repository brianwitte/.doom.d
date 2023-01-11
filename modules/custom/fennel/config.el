;;; custom/fennel/config.el -*- lexical-binding: t; -*-

;; (define-key fennel-mode-map (kbd "M-.") 'fennel-find-definition)
;; (define-key fennel-mode-map (kbd "M-,") 'fennel-find-definition-pop)
;; (define-key fennel-mode-map (kbd "M-'") 'fennel-find-module-definition)
;; (define-key fennel-mode-map (kbd "C-c C-k") 'fennel-reload)
;; (define-key fennel-mode-map (kbd "C-c C-l") 'fennel-view-compilation)
;; (define-key fennel-mode-map (kbd "C-c C-z") 'fennel-repl)
;; (define-key fennel-mode-map (kbd "C-c C-t") 'fennel-format)
;; (define-key fennel-mode-map (kbd "C-c C-f") 'fennel-show-documentation)
;; (define-key fennel-mode-map (kbd "C-c C-d") 'fennel-show-documentation)
;; (define-key fennel-mode-map (kbd "C-c C-v") 'fennel-show-variable-documentation)
;; (define-key fennel-mode-map (kbd "C-c C-a") 'fennel-show-arglist)
;; (define-key fennel-mode-map (kbd "C-c C-p") 'fennel-macroexpand)
;; ;; lisp-mode functions
;; (define-key fennel-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
;; (define-key fennel-mode-map (kbd "C-c C-e") 'lisp-eval-defun)
;; (define-key fennel-mode-map (kbd "C-M-x")   'lisp-eval-defun)
;; (define-key fennel-mode-map (kbd "C-c C-n") 'lisp-eval-form-and-next)
;; (define-key fennel-mode-map (kbd "C-c C-p") 'lisp-eval-paragraph)
;; (define-key fennel-mode-map (kbd "C-c C-r") 'lisp-eval-region)

(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(use-package! fennel-mode
  :defer t
  :config
  (set-lookup-handlers! 'fennel-mode
    :definition #'fennel-find-definition
    :documentation #'fennel-show-documentation)
  (set-repl-handler! 'fennel-mode #'fennel-repl)

  (setq-hook! 'fennel-mode-hook
    ;; To match the `tab-width' default for other lisp modes
    tab-width 2
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp "[ \t]*;;;;* [^ \t\n]")

  (map! (:localleader
         (:map (fennel-mode-map)
               "c"  #'fennel-repl
               "m"  #'fennel-macroexpand
               (:prefix ("d" . "debug")
                        "d" #'cider-debug-defun-at-point)
               (:prefix ("e" . "eval")
                        "d" #'lisp-eval-defun
                        "e" #'fnl-eval-sexp-at-point
                        "r" #'lisp-eval-region)
               (:prefix ("g" . "goto")
                        "b" #'cider-pop-back
                        "g" #'cider-find-var
                        "n" #'cider-find-ns)
               (:prefix ("h" . "help")
                        "d" #'fennel-show-documentation
                        )
               (:prefix ("i" . "inspect")
                        "e" #'cider-enlighten-mode
                        "i" #'cider-inspect
                        "r" #'cider-inspect-last-result)
               (:prefix ("n" . "namespace")
                        "n" #'cider-browse-ns
                        "N" #'cider-browse-ns-all
                        "r" #'cider-ns-refresh)
               (:prefix ("p" . "print")
                        "p" #'cider-pprint-eval-last-sexp
                        "P" #'cider-pprint-eval-last-sexp-to-comment
                        "d" #'cider-pprint-eval-defun-at-point
                        "D" #'cider-pprint-eval-defun-to-comment
                        "r" #'cider-pprint-eval-last-sexp-to-repl)
               (:prefix ("r" . "repl")
                        "n" #'cider-repl-set-ns
                        "q" #'cider-quit
                        "r" #'cider-ns-refresh
                        "R" #'cider-restart
                        "b" #'cider-switch-to-repl-buffer
                        "B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
                        "c" #'cider-find-and-clear-repl-output
                        "l" #'cider-load-buffer
                        "L" #'cider-load-buffer-and-switch-to-repl-buffer)
               (:prefix ("t" . "test")
                        "a" #'cider-test-rerun-test
                        "l" #'cider-test-run-loaded-tests
                        "n" #'cider-test-run-ns-tests
                        "p" #'cider-test-run-project-tests
                        "r" #'cider-test-rerun-failed-tests
                        "s" #'cider-test-run-ns-tests-with-filters
                        "t" #'cider-test-run-test)))

        (:when (modulep! :editor evil +everywhere)
          :map cider-repl-mode-map
          :i [S-return] #'cider-repl-newline-and-indent
          :i [M-return] #'cider-repl-return
          (:localleader
           "n" #'cider-repl-set-ns
           "q" #'cider-quit
           "r" #'cider-ns-refresh
           "R" #'cider-restart
           "c" #'cider-repl-clear-buffer)
          :map cider-repl-history-mode-map
          :i [return]  #'cider-repl-history-insert-and-quit
          :i "q"  #'cider-repl-history-quit
          :i "l"  #'cider-repl-history-occur
          :i "s"  #'cider-repl-history-search-forward
          :i "r"  #'cider-repl-history-search-backward
          :i "U"  #'cider-repl-history-undo-other-window))
  )
