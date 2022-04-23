;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
;;

;; to bind keys to primary commands within ztree module
(use-package ztree
  :bind (("<f8>" . ztree-diff)))

(global-diff-hl-mode)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;;(setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;;
;; wasn't installed correctly. Font issues are rarely Doom issues!
;;
(setq
 doom-font (font-spec :family "JetBrains Mono" :size 15)
 doom-variable-pitch-font (font-spec :family "Broken Console" :size 18)
 ;; doom-theme 'spacemacs-light
 )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq hour
      (string-to-number
       (substring (current-time-string) 11 13))) ;; gets the hour
(if (member hour (number-sequence 6 16)) ;; if between 06:00-16:59
    (setq now 'doom-one-light) ;; then light theme
  (setq now 'doom-one)) ;; else dark theme from 5pm
(if (equal now doom-theme) ;; only switches to the correct theme if needed
    nil
  (setq doom-theme now))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


(setq auto-revert-check-vc-info t)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; MY_KEYBINDINGS
;;

;; buffers
(map! :leader
      :desc "kill buffer"
      "k k" #'kill-buffer)
(map! :leader
      :desc "kill buffer and window"
      "k c" #'kill-buffer-and-window)

;; magit
(map! :leader
      :desc "diff with magit"
      "g d" #'magit-diff)

;; s-expression stuff
;;
;; TODO maybe
;;Move up and backward in list structure (backward-up-list).
;;C-M-d
;;Move down and forward in list structure (down-list).
;;C-M-n
;;Move forward over a list (forward-list).
;;C-M-p
;;Move backward over a list (backward-list).
;;C-M-t
;;Transpose expressions (transpose-sexps).
;;C-M-@
;;Put mark after following expression (mark-sexp).

(after! cider
  (defadvice! cider-insert-last-sexp-in-repl-a (cmd &optional args)
    :around #'cider-insert-last-sexp-in-repl
    (evil-collection-cider-last-sexp cmd args)))
