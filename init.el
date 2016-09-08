(setq debug-on-error t)
(setq use-package-verbose t)
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startupa
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)

(set-default 'indent-tabs-mode nil)
(setq sentence-end-double-space nil)

;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

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
  (general-nmap "SPC b d" 'kill-this-buffer
                "SPC b b" 'switch-to-buffer
                "SPC q"   'save-buffers-kill-terminal
                "SPC a d" 'dired
                "C-+" 'text-scale-increase
                "C--" 'text-scale-decrease
                "C-=" '(lambda () (interactive) (text-scale-set 1))))

;; evil
(use-package evil
  :ensure t
  :init
  (progn
    (evil-mode 1)
    (evil-declare-change-repeat 'company-complete)))

;; automatically insert the closing bracket
(electric-pair-mode)
;; (use-package evil-surround :ensure t
;;   :init
;;   (progn
;;     (global-evil-surround-mode 1)
;;     (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
;;     (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

;; escape on quick fd
(use-package evil-escape :ensure t
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
  :config
  (which-key-mode 1))

;; company
(use-package company :ensure t
  :config
  (global-company-mode)
  :general
  (general-imap "C-SPC" 'company-complete)
  (general-define-key :keymaps 'company-active-map
                      "C-j" 'company-select-next
                      "C-k" 'company-select-previous))

;; magit
(use-package magit :ensure t
  :general
  (general-nmap "SPC g s" 'magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit :ensure t)

;; theme
(use-package apropospriate-theme :ensure t
  :config 
  (load-theme 'apropospriate-dark t)
  (set-default-font "PragmataPro"))
;; (load-theme 'apropospriate-light t))

(use-package powerline :ensure t
  :config
  (powerline-default-theme))

;; haskell

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-interactive-popup-error nil))

(use-package intero :ensure t)

;; purescript

(use-package flycheck :ensure t)
(use-package purescript-mode :ensure t)
(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation)))
  :general
  (general-define-key :keymaps 'purescript-mode-map
                      :states '(normal visual)
                      ", s" 'psc-ide-server-start
                      ", t" 'psc-ide-show-type
                      ", g g" 'psc-ide-goto-definition
                      ", a i" 'psc-ide-add-import))
