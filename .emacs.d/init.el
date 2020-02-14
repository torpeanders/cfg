;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup paths
(setq user-emacs-directory "~/.emacs.d/")
(setq config-dir
      (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path config-dir)

(defconst anr-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(unless (file-exists-p anr-savefile-dir)
  (make-directory anr-savefile-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management
(require 'package)

(setq package-archives
      '(
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make sure we have use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whoami
(setq user-full-name "Anders Rønningen"
      user-mail-address "anders@ronningen.priv.no")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load config files
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'config-sane-defaults)
(require 'config-mode-mappings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
(set-frame-font "Source Code Pro 13")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages

;; built-in
(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" anr-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

;; third-party
(use-package bm
  :ensure t
  :config
  (global-set-key (kbd "<f2> b") 'bm-toggle)
  (global-set-key (kbd "C-<f2>") 'bm-next)
  (global-set-key (kbd "S-<f2>") 'bm-previous)
  )

(use-package fzf
  :ensure t
  :config
  (global-set-key (kbd "C-c C-f") 'fzf)
  (global-set-key (kbd "C-c d") 'fzf-directory)
  )

(use-package fasd
  :ensure t
  :config
  :bind (("C-c g" . fasd-find-file)))

(use-package hydra
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t
  :config
  (setq org-src-fontify-natively t)
  (setq
   org-outline-path-complete-in-steps nil
   org-goto-interface 'outline-path-completion

   org-src-fontify-natively t
                                        ; clock
   org-clock-into-drawer  "CLOCK"
   org-clock-out-when-done t
   org-clock-in-switch-to-state nil
                                        ; log
   org-log-note-clock-out t
   org-duration-format (quote h:mm)
                                        ; time
   org-time-clocksum-use-fractional t
   org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i@)" "|" "DONE(f@)" "DELEGATED(d@)" "CANCELLED(c@)"))
   org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("DONE" . (:foreground "green" :weight bold))
     ("DELEGATED" . (:foreground "yellow" :weight bold))
     ("CANCELLED" . (:foreground "red" :weight bold))
     ))
  ;; Location of (most) my org files
  (setq org-directory "~/org")
  ;; Don't split line when creating new org heading with <M-return>
  (setq org-M-RET-may-split-line '((item . nil)))
  ;; hydra - headings
  (key-chord-define-global "OM"
                           (defhydra hydra-org (:color red :columns 3)
                             "Org Mode Movements"
                             ("n" outline-next-visible-heading "next heading")
                             ("p" outline-previous-visible-heading "prev heading")
                             ("N" org-forward-heading-same-level "next heading at same level")
                             ("P" org-backward-heading-same-level "prev heading at same level")
                             ("u" outline-up-heading "up heading")
                             ("g" org-goto "goto" :exit t)))
  )

(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :config (setq key-chord-two-keys-delay 0.075))

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  )

(use-package all-the-icons-ivy
  :ensure t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package avy
  :ensure t
  :bind (("C-c w" . avy-goto-word-or-subword-1)
         ("C-c c" . avy-goto-char-timer))
  :config
  (setq avy-background t))

(use-package ag
  :ensure t)

(use-package rg
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package whitespace
  :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook))
  ;;   (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu)
         ("C-x i" . imenu-anywhere)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package ace-window
  :ensure t
  :config
  (global-set-key [remap other-window] 'ace-window))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-c s") 'swiper-isearch-thing-at-point))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-c r") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (yas-reload-all)
  :diminish yas-minor-mode)

;; (use-package dtrt-indent
;;   :ensure t
;;   :config (setq dtrt-indent-active-mode-line-info nil)
;;   :hook (prog-mode . dtrt-indent-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc keybindings
(global-set-key (kbd "C-c b") 'anr/create-scratch-buffer)
(global-set-key (kbd "C-c f") 'fasd-find-file)
(global-set-key (kbd "C-c n") 'anr/cleanup-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc setup

;; c code style and indentation
(anr/normal-c-mode-offset)

(defun maybe-linux-style ()
  (when (and buffer-file-name
             (string-match "linux\\|kernel" buffer-file-name))
    (anr/linux-c-mode-offset)))

(add-hook 'c-mode-hook 'maybe-linux-style)
(add-hook 'dts-mode-hook 'anr/dts-mode-offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; machine specific setup
(cond ((file-exists-p "~/.emacs-this-pc.el")
       (load "~/.emacs-this-pc.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel-gtags yasnippet-snippets yasnippet counsel swiper ace-window ivy undo-tree which-key imenu-anywhere zop-to-char company rainbow-delimiters expand-region projectile rg ag avy nimbus-theme magit exec-path-from-shell key-chord hydra fasd fzf bm use-package systemd sws-mode rainbow-mode qml-mode kotlin-mode js2-mode jade-mode haskell-mode dts-mode dockerfile-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
