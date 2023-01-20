;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "brian witte"
      user-mail-address "brianwitte@x86.art")

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

;; theme toggle
(map! :leader
      :desc "toggle light/dark theme"
      "m t" #'modus-themes-toggle)

;; buffers
(map! :leader
      :desc "kill buffer"
      "d d" #'kill-buffer)

(map! :leader
      :desc "kill buffer and window"
      "d k" #'kill-buffer-and-window)

(map! :leader
      :desc "counsel ripgrep"
      "r g" #'consult-ripgrep)

;; magit
(map! :leader
      :desc "diff with magit"
      "g d" #'magit-diff)

;; compiling
(map! :leader
      :desc "compile project"
      "c p" 'project-compile)

(after! cider
  (defadvice! cider-insert-last-sexp-in-repl-a (cmd &optional args)
    :around #'cider-insert-last-sexp-in-repl
    (evil-collection-cider-last-sexp cmd args)))

(require 'paren)
(set-face-background 'show-paren-match "#4CBB17")
(set-face-foreground 'show-paren-match "#FF0000")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

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

;; Messages shown upon connecting to nrepl.
(setq cider-words-of-inspiration
      '("I can show you the world. Shining, shimmering, splendid. Tell me, princess, now when did you last let your heart decide?"
        "I can open your eyes--take you wonder by wonder over, sideways and under on a magic carpet ride."
        "A whole new world--a new fantastic point of view. No one to tell us no or where to go, or say we're only dreaming."
        "A whole new world--a dazzling place I never knew. But when I'm way up here, it's crystal clear that now I'm in a whole new world with you."
        "Unbelievable sights, indescribable feeling, soaring, tumbling, freewheeling through an endless diamond sky."
        "A whole new world. Don't you dare close your eyes. A hundred thousand things to see. (Hold your breath, it gets better)."
        "I'm like a shooting star; I've come so far; I can't go back to where I used to be."))
;; © Disney

;; zig stuff
(setq lsp-zig-zls-executable "~/zls/zig-out/bin/zls")

(setq default-frame-alist `((cursor-color . "#FF0000")
                            ,@default-frame-alist))
