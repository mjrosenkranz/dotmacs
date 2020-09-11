;; setup package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; make sure that it gets initialized
;; (unless (bound-and-true-p package--initialized)
;;   (setq package-enable-at-startup nil)
;;   (package-initialize))

;; get `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; misc `emacs' settings
(use-package emacs
  :init
  (setq custom-file "~/.emacs.d/custom-settings.el")
  (load custom-file t)
  (setq ring-bell-function 'ignore)
  (setq inhibit-startup-screen t)
  (set-frame-font "Jetbrains Mono 12" nil t)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;; setup ido
(use-package icomplete
  :config
  (icomplete-mode))

;; make dired list directories first
(use-package dired
  :defer
  :config
  (setq dired-listing-switches "-aBhl  --group-directories-first"))
;; make moving between and swapping windows easier
(use-package windmove
  :defer
  :bind
  ("M-<left>" . 'windmove-left)
  ("M-<right>" . 'windmove-right)
  ("M-<up>" . 'windmove-up)
  ("M-<down>" . 'windmove-down)
  ("M-S-<left>" . 'windmove-swap-states-left)
  ("M-S-<right>" . 'windmove-swap-states-right)
  ("M-S-<up>" . 'windmove-swap-states-up)
  ("M-S-<down>" . 'windmove-swap-states-down))

;; vim fusion
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

;; get this dope theme
(use-package modus-vivendi-theme
  :ensure t
  :config(load-theme 'modus-vivendi t))

(use-package olivetti
  :ensure t)

(use-package go-mode
  :ensure t
  :defer t
  :config
  (setq gofmt-command "goimports")
  (defun oct/go-mode-addons ()
    (add-hook 'before-save-hook 'gofmt nil 'local))
  (add-hook 'go-mode-hook 'oct/go-mode-addons))
  (add-hook 'go-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends) '(company-go))))


(use-package company :ensure t
  :config
  (company-mode))
(use-package company-go :ensure t)

