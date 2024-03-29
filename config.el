;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Brian Witte"
      user-mail-address "brianwitte@mailfence.com")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:


;; Load the theme of your choice.
(load-theme 'modus-operandi-tinted :no-confirm)

;;(setq
;; doom-font (font-spec :family "Noto Sans Mono" :size 16)
;; )

(load-file "~/.doom.d/font.el")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


(setq
 doom-localleader-key ","
 doom-localleader-alt-key "C-,"
 evil-want-C-u-scroll nil
 tab-width 4
 auto-revert-check-vc-info t
 apropos-sort-by-scores t
 doom-font-increment 1
 split-width-threshold 160
 split-height-threshold 80)

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

(map! :leader
      (:prefix ("m" . "themes")
               ("t" #'modus-themes-select))
      (:prefix ("d" . "buffer-actions")
               ("d" #'kill-buffer)
               ("k" #'kill-buffer-and-window))
      (:prefix ("r" . "search")
               ("g" #'consult-ripgrep))
      (:prefix ("g" . "magit")
               ("d" #'magit-diff))
      (:prefix ("c" . "compile")
               ("p" 'project-compile)
               ("o" 'clang-format-buffer))
      (:prefix ("l" . "LSP")
               ("s" 'lsp)
               ("f" 'lsp-ui-imenu))
      )

(map! :leader
      (:prefix ("k" .  "lispy")
               "="  #'sp-reindent
               "-"  #'sp-reindent
               "W"  #'sp-unwrap-sexp
               "b"  #'sp-forward-barf-sexp
               "B"  #'sp-backward-barf-sexp
               "c"  #'sp-convolute-sexp
               "dx" #'sp-kill-sexp
               "r"  #'sp-raise-sexp
               "s"  #'sp-forward-slurp-sexp
               "S"  #'sp-backward-slurp-sexp
               "t"  #'sp-transpose-sexp
               "w"  #'sp-wrap-sexp
               "y"  #'sp-copy-sexp))

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp)

(setq display-line-numbers-type 'relative)

(defalias 'elisp-mode 'emacs-lisp-mode)

;; disable nonsensical keys
(dolist (key '("s-n" "s-p" "s-q" "s-m" "C-x C-c"))
  (global-set-key (kbd key) nil))
