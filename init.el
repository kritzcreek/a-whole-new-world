(setq debug-on-error t)
(setq delete-old-versions -1 ); delete excess backup versions silently
(setq version-control t ); use version control
(setq vc-make-backup-files t ); make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t ); don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t ); inhibit useless and old-school startup screen
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil); sentence SHOULD end with only a point.
(setq fill-column 100); toggle wrapping text at the 90th character
(setq initial-scratch-message "Cheers love, the cavalry's here!") ; print a default message in the empty scratch buffer opened at startup

(if (display-graphic-p)
    (scroll-bar-mode 0)
  (setq-default cursor-type 'bar))
(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

(set-default 'indent-tabs-mode nil)
(setq visible-bell nil)
;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq gc-cons-threshold 50000000) ;; allow for more allocated memory before triggering the gc
(setq read-process-output-max (* 1024 1024)) ;; bigger chunk-size when reading from network processes (LSP, purs ide)
(setq line-number-display-limit-width 10000)
(setq uniquify-buffer-name-style 'forward)

(setq custom-file "~/.emacs.d/etc/custom.el")
(load custom-file)

(when (memq system-type '(darwin windows-nt))
  (setq ring-bell-function 'ignore))

;; Always start in HOME dir when on Windows, as I never start Emacs
;; from the terminal here
(when (memq system-type '(windows-nt)) (cd "~"))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

(global-display-line-numbers-mode 1)

(setq mouse-yank-at-point t)
(setq save-interprogram-paste-before-kill t)
(setq use-dialog-box nil)

(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
(setq package-archives '(("elpa" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

;; Sane indentation default
(setq tab-width 2)
(setq js-indent-level 2)

(require 'use-package)

(defvar kc/font-family "PragmataPro")
(defvar kc/font-size 160)
;; Allows per-machine config by loading the `init-$HOSTNAME.el` file
;; on startup
(defconst kc/local-conf-file
  (downcase (format "~/.emacs.d/init-%s.el" (system-name))))

(when (file-exists-p kc/local-conf-file)
  (message "Applying local overrides")
  (load kc/local-conf-file))

;; Setting up font and size
(set-face-attribute 'default nil :family kc/font-family)
(set-face-attribute 'default nil :height kc/font-size)

;; keybindings
(use-package general :ensure t
  :config
  (general-evil-setup)
  (setq general-default-keymaps 'evil-normal-state-map)
  ;; unbind space from dired map to allow for git status
  (general-define-key :keymaps 'dired-mode-map "SPC" nil)
  (general-define-key
   :keymaps 'visual
   "SPC ;"   'comment-or-uncomment-region)
  (general-define-key
   :keymaps 'normal
   "SPC b d" 'kill-this-buffer
   "SPC b b" 'switch-to-buffer
   "SPC f d" 'kc/find-user-init-file
   "SPC f t" 'kc/find-user-todo-file
   "SPC q"   'save-buffers-kill-terminal
   "SPC a d" 'dired
   "SPC TAB" 'kc/switch-to-previous-buffer
   "SPC t f" 'display-fill-column-indicator-mode
   "C-+" 'text-scale-increase
   "C--" 'text-scale-decrease
   "C-=" '(lambda () (interactive) (text-scale-set 0))))

(defun kc/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defconst kc/user-todo-file "~/Dropbox/org/todo.org")

(defun kc/find-user-todo-file ()
  "Edit the `user-todo-file', in another window."
  (interactive)
  (find-file-other-window kc/user-todo-file))

(defun kc/switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; evil
(use-package evil
  :ensure t
  :init
  (progn
    (setq
     evil-want-C-u-scroll t
     evil-want-keybinding nil
     evil-undo-system 'undo-redo)
    (evil-mode 1)
    (evil-declare-change-repeat 'company-complete)))

(use-package evil-collection :ensure t
  :after evil
  :config (evil-collection-init '(magit dired)))

(use-package evil-surround
  :ensure t
  :init
  (progn
    (global-evil-surround-mode 1)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

;; escape on quick fd
(use-package evil-escape :ensure t
  :diminish evil-escape-mode
  :config
  (evil-escape-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (unless (eq system-type 'windows-nt)
      (exec-path-from-shell-initialize)))

;; Ivy things
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 15)
  (setq ivy-count-format "(%d/%d) ")
  :general
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line)
  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "C-k" 'ivy-previous-line))

(use-package counsel :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC f f" 'counsel-find-file
   "SPC h f" 'counsel-describe-function
   "SPC h v" 'counsel-describe-variable
   "SPC u"   'counsel-unicode-char
   "SPC p s" 'counsel-rg
   "SPC SPC" 'counsel-M-x))

(use-package swiper :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC s" 'swiper))

(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)

(use-package dabbrev
  :bind* (("C-/" . #'dabbrev-completion))
  :custom
  (dabbrev-case-replace nil))

(use-package project :ensure t
  :pin elpa
  :general
  (general-define-key
   :keymaps 'normal
   "SPC p p" 'project-switch-project
   "SPC p f" 'project-find-file))

;; which-key
(use-package which-key :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  (setq company-idle-delay 0.25)
  :general
  (general-define-key
   :keymaps 'insert
   "C-SPC" 'company-complete)
  (general-define-key
   :keymaps 'company-active-map
   "<tab>" 'company-complete-selection
   "C-j" 'company-select-next
   "C-k" 'company-select-previous))

(use-package company-quickhelp
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package org-tempo :after (org))

(use-package org-tree-slide :ensure t
  :config
  (general-define-key
   :keymaps 'org-tree-slide-mode-map
   :state '(normal visual)
   "C-<right>" 'org-tree-slide-move-next-tree
   "C-<left>" 'org-tree-slide-move-previous-tree)
  (general-define-key
   :state '(normal visual)
   "SPC t p" 'org-tree-slide-mode)
  (add-hook
   'org-tree-slide-play-hook
   (lambda ()
     (text-scale-increase 5)
     (org-tree-slide-presentation-profile)
     (org-redisplay-inline-images)))
  (add-hook
   'org-tree-slide-stop-hook
   (lambda () (text-scale-set 0))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  :general
  (general-define-key
   :keymaps 'insert
   "C-M-SPC" 'yas-expand))

(use-package diminish :ensure t)

;; magit
(use-package magit :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC g s" 'magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; Highlighting TODO keywords
(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode))

;; Undo all themes
;; (mapcar #'disable-theme custom-enabled-themes)
;; (load-theme 'leuven t)

(use-package modus-themes
  :ensure t
  :config (load-theme 'modus-operandi t) ;; light
  ;; :config (load-theme 'modus-vivendi t) ;; dark
  )

(use-package doom-themes
  :ensure t
  ;; :config (load-theme 'doom-moonlight t) ;; dark
  ;; :config (load-theme 'doom-spacegrey t) ;; light
  )

(use-package doom-modeline
  :ensure t
  :config (setq doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (progn
    (setq sp-message-width nil
          sp-show-pair-from-inside t
          sp-autoescape-string-quote nil
          sp-cancel-autoskip-on-backward-movement nil))
  :config
  (progn
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
    (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
    (sp-local-pair 'purescript-mode "\\{" nil :actions nil)
    (sp-local-pair 'purescript-mode "'" nil :actions nil)

    (smartparens-global-mode)
    (show-smartparens-global-mode)))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-checker-error-threshold 10000)
  :general
  (general-define-key
   :keymaps 'normal
   "SPC e n" 'flycheck-next-error
   "SPC e p" 'flycheck-previous-error))

(use-package restclient :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-rust-server 'rust-analyzer)
  :hook ((rust-mode . lsp)
         (typescript-mode . lsp)
         (tuareg-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)

(use-package typescript-mode :ensure t
  :config (setq typescript-indent-level 2))

(use-package rust-mode :ensure t)

(use-package cargo
  :ensure t
  :general
  (general-define-key
   :keymaps 'rust-mode-map
   :states '(normal visual)
   ", c b" 'cargo-process-build
   ", c t" 'cargo-process-test
   ", c r" 'cargo-process-run
   ", c f" 'cargo-process-fmt
   ", a" 'lsp-execute-code-action
   ))

;; haskell
(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-interactive-popup-error nil))

;; Unicode input
(use-package xah-math-input
  :load-path "~/.emacs.d/lisp"
  :general
  (general-define-key
   :keymaps 'insert
   "M-SPC" 'xah-math-input-change-to-symbol))

;; purescript
(use-package purescript-mode
  :ensure t
  :diminish 'purescript-indentation-mode)

(defun kc/purescript-hook ()
  "My PureScript mode hook"
  (turn-on-purescript-indentation)
  (psc-ide-mode)
  (company-mode)
  (flycheck-mode)
  (setq-local flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package psc-ide
  :ensure t
  ;; :load-path "~/code/psc-ide-emacs/"
  :init (add-hook 'purescript-mode-hook 'kc/purescript-hook)
  :config
  (setq ;; psc-ide-debug t
        psc-ide-use-npm-bin t)
  :general
  (general-define-key
   :keymaps 'purescript-mode-map
   :states '(normal visual)
   ", s" 'psc-ide-server-start
   ", l" 'psc-ide-load-all
   ", q" 'psc-ide-server-quit
   ", t" 'psc-ide-show-type
   ", b" 'psc-ide-rebuild
   ", g g" 'psc-ide-goto-definition
   ", a i" 'psc-ide-add-import))

;; OCaml

(use-package tuareg :ensure t)

;; WASM
(use-package wat-mode
  :load-path "~/.emacs.d/lisp/wat-mode")

(use-package idris-mode :ensure t)

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode))

(use-package company-auctex
  :defer t
  :ensure t)

(use-package ethan-wspace
  :ensure t
  :diminish 'ethan-wspace-mode
  :init (setq mode-require-final-newline nil
              require-final-newline nil)
  :config (global-ethan-wspace-mode 1))

(setq debug-on-error nil)
