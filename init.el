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
(setq default-fill-column 80); toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startupa
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)

(set-default 'indent-tabs-mode nil)
(setq visible-bell nil)
;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

(setq gc-cons-threshold 50000000) ;; allow for more allocated memory before triggering the gc
(setq line-number-display-limit-width 10000)
(setq gnutls-min-prime-bits 4096)
(setq uniquify-buffer-name-style 'forward)

(setq custom-file "~/.emacs.d/etc/custom.el")
(load custom-file)

(when (eq system-type 'darwin)
  (setq ring-bell-function 'ignore
        ns-use-srgb-colorspace nil
        ns-right-alternate-modifier nil))

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

(setq mouse-yank-at-point t)
(setq save-interprogram-paste-before-kill t)
(setq use-dialog-box nil)


(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)

;; keybindings
(use-package general :ensure t
  :config
  (general-evil-setup)
  (setq general-default-keymaps 'evil-normal-state-map)
  ;; unbind space from dired map to allow for git status
  (general-define-key :keymaps 'dired-mode-map "SPC" nil)
  (general-nmap "SPC b d" 'kill-this-buffer
                "SPC b b" 'switch-to-buffer
                "SPC q"   'save-buffers-kill-terminal
                "SPC a d" 'dired
                "SPC TAB" 'switch-to-previous-buffer
                "C-+" 'text-scale-increase
                "C--" 'text-scale-decrease
                "C-=" '(lambda () (interactive) (text-scale-set 1))))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; evil
(use-package evil
  :ensure t
  :init
  (progn
    (evil-mode 1)
    (evil-declare-change-repeat 'company-complete)))

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
  (exec-path-from-shell-initialize))

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
  (general-define-key  :keymaps 'ivy-minibuffer-map
                       "C-j" 'ivy-next-line
                       "C-k" 'ivy-previous-line))

(use-package counsel :ensure t
  :general
  (general-nmap "SPC f f" 'counsel-find-file
                "SPC h f" 'counsel-describe-function
                "SPC u"   'counsel-unicode-char
                "SPC p f" 'counsel-git
                "SPC p s" 'counsel-ag
                "SPC SPC" 'counsel-M-x))


(use-package swiper :ensure t
  :general
  (general-nmap "SPC s" 'swiper))

(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)

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
  :general
  (general-imap "C-SPC" 'company-complete)
  (general-define-key :keymaps 'company-active-map
                      "C-j" 'company-select-next
                      "C-k" 'company-select-previous))

(use-package diminish
  :ensure t
  :config
  (diminish 'undo-tree-mode))
;; magit
(use-package magit :ensure t
  :general
  (general-nmap "SPC g s" 'magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit :ensure t)

;; theme
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t)
  (set-face-attribute 'default nil :family "PragmataPro")
  (set-face-attribute 'default nil :height 130))

(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

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
    (sp-local-pair 'scheme-mode "'" nil :actions nil)
    (sp-local-pair 'scheme-mode "`" nil :actions nil)
    (sp-local-pair 'inferior-scheme-mode "'" nil :actions nil)
    (sp-local-pair 'inferior-scheme-mode "`" nil :actions nil)

    (sp-local-pair 'LaTeX-mode "\"" nil :actions nil)
    (sp-local-pair 'LaTeX-mode "'" nil :actions nil)
    (sp-local-pair 'LaTeX-mode "`" nil :actions nil)
    (sp-local-pair 'latex-mode "\"" nil :actions nil)
    (sp-local-pair 'latex-mode "'" nil :actions nil)
    (sp-local-pair 'latex-mode "`" nil :actions nil)
    (sp-local-pair 'TeX-mode "\"" nil :actions nil)
    (sp-local-pair 'TeX-mode "'" nil :actions nil)
    (sp-local-pair 'TeX-mode "`" nil :actions nil)
    (sp-local-pair 'tex-mode "\"" nil :actions nil)
    (sp-local-pair 'tex-mode "'" nil :actions nil)
    (sp-local-pair 'tex-mode "`" nil :actions nil))
    (smartparens-global-mode)
    (show-smartparens-global-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; haskell

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-interactive-popup-error nil))

(use-package intero
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode)
  :general
  (general-define-key :keymaps 'intero-mode-map
                      :states '(normal visual)
                      ", i" 'intero-info
                      ", t" 'intero-type-at
                      ", l" 'intero-repl-load
                      ", c" 'intero-repl-clear-buffer
                      ", r" 'intero-restart
                      ", m t" 'intero-targets
                      ", g g" 'intero-goto-definition))

;; purescript

(use-package flycheck :ensure t)
(use-package purescript-mode
  :ensure t
  :diminish 'purescript-indentation-mode)
(use-package psc-ide
  :ensure t
;; :load-path "~/Documents/psc-ide-emacs/"
  :init
  (progn
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
    (add-hook 'purescript-mode-hook 'psc-ide-mode)
    (add-hook 'purescript-mode-hook 'company-mode)
    (add-hook 'purescript-mode-hook 'flycheck-mode))
  :general
  (general-define-key :keymaps 'purescript-mode-map
                      :states '(normal visual)
                      ", s" 'psc-ide-server-start
                      ", q" 'psc-ide-server-quit
                      ", t" 'psc-ide-show-type
                      ", b" 'psc-ide-rebuild
                      ", g g" 'psc-ide-goto-definition
                      ", a i" 'psc-ide-add-import))

(use-package org-ref
  :ensure t)

(use-package ethan-wspace
  :ensure t
  :init (setq mode-require-final-newline nil)
  :config (global-ethan-wspace-mode 1))

(setq debug-on-error nil)
