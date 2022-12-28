;;; custom/my-modeline/autoload.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022  brian witte
;; Author: brian witte <brianwitte@x86.art>
;;

;;;###autoload
(defun setup-custom-doom-modeline ()
  (doom-modeline-def-modeline
    'bwcustom
    '(bar persp-name window-number buffer-info)
    '(;; battery
      ;; grip
      ;; irc mu4e debug
      ;; repl
      major-mode
      lsp
      misc-info
      process
      ;; checker

      matches selection-info
      buffer-position))
    (doom-modeline-set-modeline 'dwcustom))
