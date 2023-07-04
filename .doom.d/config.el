;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Anders Rønningen"
      user-mail-address "anders@ronningen.priv.no")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'vscode-dark-plus)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun anr/create-scratch-buffer nil
  "Create a new scratch buffer to work in (could be *scratch* - *scratchX*)."
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

(windmove-default-keybindings)

;; utf-8 ftw
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; use rg to search in projects
(setq +ivy-project-search-engines '(rg))

;; setup mac modifiers
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t))

;; yes, I am sure
(setq confirm-kill-emacs nil)

;; looks are important
(setq doom-font (font-spec :family "Source Code Pro" :size 13))

;; some magit customization
(setq magit-display-buffer-function
      #'magit-display-buffer-fullframe-status-v1)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; => code style
(defun anr/c-mode-offset-linux ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(defun anr/maybe-linux-style ()
  (when (and buffer-file-name
             (string-match "linux\\|kernel" buffer-file-name))
    (anr/c-mode-offset-linux)))

(defun anr/dts-mode-offset ()
  "Dts* files."
  (interactive)
  (setq tab-width 8)
  (setq indent-tabs-mode t))

(add-hook! c-mode 'anr/maybe-linux-style t)
(add-hook! dts-mode 'anr/dts-mode-offset t)

;; make a mode for soong
(define-derived-mode soong-mode
  javascript-mode "Soong"
  "Major mode for soong files."
  :syntax-table nil
  :abbrev-table nil
  (setq-local indent-tabs-mode nil)
  (setq-local js-indent-level 4))

;; mode-mappings
(add-to-list 'auto-mode-alist '("\\.dts$" . dts-mode))
(add-to-list 'auto-mode-alist '("\\.dtsi$" . dts-mode))
(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))
(add-to-list 'auto-mode-alist '("\\.bp\\'" . soong-mode))

;; extra keybindings
(global-set-key (kbd "C-c b")       'anr/create-scratch-buffer)
(global-set-key (kbd "<f2>")        'bm-toggle)
(global-set-key (kbd "C-<f2>")      'bm-next)
(global-set-key (kbd "S-<f2>")      'bm-previous)
(global-set-key (kbd "C-c f")       'fasd-find-file)
(global-set-key (kbd "C-c C-f")     'fzf)
(global-set-key (kbd "C-c d")       'fzf-directory)
(global-set-key (kbd "C-c <left>")  'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)

;; org setup
(after! org
  (setq org-tags-column -80)
  (setq
   org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i@)" "|" "DONE(f@)" "DELEGATED(d@)" "CANCELLED(c@)"))
   org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("DONE" . (:foreground "green" :weight bold))
     ("DELEGATED" . (:foreground "yellow" :weight bold))
     ("CANCELLED" . (:foreground "red" :weight bold))
     ))
   ;; ... other org configuration here
)

;; magit setup
(after! magit
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  )

;; diff init.el to look for new and cool stuff
(defun anr/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-private-dir "init.el")
               (concat doom-emacs-dir "templates/init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example
  )

;; show workspace in modeline
(after! doom-modeline
  (setq doom-modeline-persp-name t))

;; no new workspace on project switch
(setq +workspaces-on-switch-project-behavior nil)

;; set the fonts
;;(setq doom-font (font-spec :family "Source Code Pro" :size 11))

;; various keybindings
(map! "C-c a" #'embark-act)
(map! "C-c e" #'embark-export)
(map! :leader
      "j j" #'avy-goto-char-timer
      "j l" #'avy-goto-line)
(map! :m [tab] #'indent-for-tab-command)

;; whitespace mode
;; Doom Emacs uses Whitespace mode for tab indents only. The following restores functionality.
(use-package! whitespace
  :config
  (setq
    whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
    whitespace-display-mappings '(
      (space-mark   ?\     [?\u00B7]     [?.])
      (space-mark   ?\xA0  [?\u00A4]     [?_])
      (newline-mark ?\n    [182 ?\n])
      (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))
  ;;(global-whitespace-mode +1)
  )
