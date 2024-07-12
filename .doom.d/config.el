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
(setq doom-theme 'doom-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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
             (string-match "linux\\|kernel\\|display-drivers\\|camera-kernel" buffer-file-name))
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

;; selinux mode
(define-generic-mode 'selinux-mode
  '("#")  ;; Comments start with #
  '("allow" "type" "class" "user" "role" "attribute" "permissive" "boolean" "if" "else" "typeattribute" "binder_call")
  '(("\\(\\<\\(?:user\\|seinfo\\|name\\|domain\\|type\\|levelFrom\\)\\)\\s-*=\\([^ \t\n]*\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))
    ("\\(u:object_r:[^ \t\n]+\\)"
     (1 'font-lock-keyword-face))
    ("\\(\\<\\(?:[a-zA-Z_]+\\)_prop\\)\\|\\(\\<\\(?:[a-zA-Z_]+\\)_dir_file\\)"
     (1 'font-lock-type-face))
    ("\\(\\<\\(?:[a-zA-Z_]+\\)_domain\\)\\s-*=\\([^ \t\n]*\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))
    ("\\(\\<.*_violators\\)\\b"
     (1 '(:foreground "lightcoral" :weight bold)))
    ("\\.te\\'" . 'font-lock-type-face)
    ("\\.contexts\\'" . 'font-lock-variable-name-face))
  '("\\.te\\'" "\\.contexts\\'")  ;; File extensions
  nil
  "A mode for SELinux policy files.")

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
        (setq magit-bury-buffer-function #'magit-restore-window-configuration)
        )

;; diff init.el to look for new and cool stuff
(defun anr/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-emacs-dir "templates/init.example.el")
               (concat doom-private-dir "init.el")))

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
;; use C-x C-s to save  buffer in insert state because of my muscle memory
(map! :after company
      :map evil-insert-state-map
      "C-x C-s" #'save-buffer)

;; alternate whitespace-mode with whitespace.el defaults, doom defaults and off:
(defun anr/set-whitespace-defaults()
                                        ; only save the values the first time we get here
  (unless (boundp 'anr/default-whitespace-style)
    (setq anr/default-whitespace-style                (default-value 'whitespace-style)
          anr/default-whitespace-display-mappings     (default-value 'whitespace-display-mappings)
          anr/doom-whitespace-style                   whitespace-style
          anr/doom-whitespace-display-mappings        whitespace-display-mappings
          anr/whitespace-mode                         "doom")))

                                        ; whitespace-style etc are set up with default-values in whitespace.el and then
                                        ; modified in doom-highlight-non-default-indentation-h (in core/core-ui.el).
                                        ; This is added to after-change-major-mode-hook in doom-init-ui-h (in
                                        ; core/core-ui.el) and called a LOT: so I need to capture doom's modified
                                        ; settings after that. The trouble is, this file (config.el) is called _before_
                                        ; doom-init-ui-h which is called in window-setup-hook as the last gasp of
                                        ; doom-initialize! find-file-hook appears to work.
(add-hook 'find-file-hook #'anr/set-whitespace-defaults 'append)

                                        ; doom=>default=>off=>doom=>...
(defun anr/toggle-whitespace () (interactive)
       (cond ((equal anr/whitespace-mode "doom")
              (setq whitespace-style anr/default-whitespace-style
                    whitespace-display-mappings anr/default-whitespace-display-mappings
                    anr/whitespace-mode "default")
              (prin1 (concat "whitespace-mode is whitespace default"))
              (whitespace-mode))
             ((equal anr/whitespace-mode "default")
              (setq anr/whitespace-mode "off")
              (prin1 (concat "whitespace-mode is off"))
              (whitespace-mode -1))
             (t ; (equal anr/whitespace-mode "off")
              (setq whitespace-style anr/doom-whitespace-style
                    whitespace-display-mappings anr/doom-whitespace-display-mappings
                    anr/whitespace-mode "doom")
              (prin1 (concat "whitespace-mode is doom default"))
              (whitespace-mode))))

(global-set-key (kbd "C-<f4>") 'anr/toggle-whitespace)

;; fasd
(use-package! fasd
              :config
              (map! :leader
                    "fas" #'fasd-find-file)
              (global-fasd-mode +1))

;; ace window
(use-package! ace-window
              :config
              (map! :leader
                    "wM" #'ace-swap-window))

;; lsp mode
(after! lsp-mode
        ;; stop formatting while I type
        (setq lsp-enable-on-type-formatting nil)
        ;; disable lsp-enable-indentation, too "electric" behavior
        ;; where also following lines are affected
        (setq lsp-enable-indentation nil)
        ;; watching large projects may cause Emacs slow-down
        (setq lsp-enable-file-watchers nil)
        ;; disable
        (setq lsp-lens-enable nil)
                                        ;(setq lsp-diagnostics-provider :none)
                                        ;(setq lsp-ui-sideline-enable nil)
        )

;; cscope
(setq consult-cscope-use-initial t)
(use-package! consult-cscope
              :defer t
              :commands (consult-cscope-symbol
                         consult-cscope-definition
                         consult-cscope-called-by
                         consult-cscope-calling
                         consult-cscope-text
                         consult-cscope-egrep
                         consult-cscope-file
                         consult-cscope-including
                         consult-cscope-assignment))

(map! :leader
      (:prefix-map ("c" . "code")
                   (:prefix ("h" . "cscope")
                            :desc "Search symbol" "s" #'consult-cscope-symbol
                            :desc "Search definition" "d" #'consult-cscope-definition
                            :desc "Search text" "t" #'consult-cscope-text
                            :desc "Search file" "f" #'consult-cscope-file)))
