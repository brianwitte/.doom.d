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
(setq
 doom-font (font-spec :family "JetBrains Mono" :size 18)
 )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'adwaita)
(custom-set-faces
 '(hl-line ((t (:background "#FFFFFF"))))
 )

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

(map! :leader
      :desc "kill buffer and window"
      "k c" #'kill-buffer-and-window)

(map! :leader
      :desc "counsel ripgrep"
      "r g" #'consult-ripgrep)

;; magit
(map! :leader
      :desc "diff with magit"
      "g d" #'magit-diff)

(after! cider
  (defadvice! cider-insert-last-sexp-in-repl-a (cmd &optional args)
    :around #'cider-insert-last-sexp-in-repl
    (evil-collection-cider-last-sexp cmd args)))

(eval-after-load "nrepl-client"
  '(defun nrepl-send-sync-request (request connection &optional abort-on-input tooling)
     "Send REQUEST to the nREPL server synchronously using CONNECTION.
Hold till final \"done\" message has arrived and join all response messages
of the same \"op\" that came along.
If ABORT-ON-INPUT is non-nil, the function will return nil at the first
sign of user input, so as not to hang the interface.
If TOOLING, use the tooling session rather than the standard session."
     (let* ((time0 (current-time))
            (response (cons 'dict nil))
            (nrepl-ongoing-sync-request t)
            status)
       (nrepl-send-request request
                           (lambda (resp) (nrepl--merge response resp))
                           connection
                           tooling)
       (while (and (not (member "done" status))
                   (not (and abort-on-input
                             (input-pending-p))))
         (setq status (nrepl-dict-get response "status"))
         ;; If we get a need-input message then the repl probably isn't going
         ;; anywhere, and we'll just timeout. So we forward it to the user.
         (if (member "need-input" status)
             (progn (cider-need-input (current-buffer))
                    ;; If the used took a few seconds to respond, we might
                    ;; unnecessarily timeout, so let's reset the timer.
                    (setq time0 (current-time)))
           ;; break out in case we don't receive a response for a while
           (when (and nrepl-sync-request-timeout
                      (time-less-p
                       nrepl-sync-request-timeout
                       (time-subtract nil time0)))
             (error "Sync nREPL request timed out %s" request)))
         ;; Clean up the response, otherwise we might repeatedly ask for input.
         (nrepl-dict-put response "status" (remove "need-input" status))
         (accept-process-output nil 0.01))
       ;; If we couldn't finish, return nil.
       (when (member "done" status)
         (nrepl-dbind-response response (ex err eval-error pp-stacktrace id)
           (when (and ex err)
             (cond (eval-error (funcall nrepl-err-handler))
                   (pp-stacktrace (cider--render-stacktrace-causes
                                   pp-stacktrace (remove "done" status))))) ;; send the error type
           (when id
             (with-current-buffer connection
               (nrepl--mark-id-completed id)))
           response)))))


;; zig stuff
(setq lsp-zig-zls-executable "~/zls/zls")

(require 'paren)
(set-face-background 'show-paren-match "#FFFF00")
(set-face-foreground 'show-paren-match "#FF0000")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(setq display-time-day-and-date t)
(display-time)
(setq global-mode-string nil)

(defun frame-title-prefix()
  (cond (multiple-frames (buffer-name))
        (t (concat invocation-name "@" (system-name)))))

(setq frame-title-format
      '("" (:eval (frame-title-prefix)) " " display-time-string))
