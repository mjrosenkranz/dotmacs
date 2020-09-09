;; start with just gnu and melpa
(setq package-archives nil)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/"))

;; we need package to actually install anything
(require 'package)

;; get `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))


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
  (menu-bar-mode -1)
  :config
  (load-theme 'modus-vivendi))

;; make moving between and swapping windows easier
(use-package windmove
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
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

;; setup ido
(use-package ido
  :config
  (ido-mode 1)
  (setq ido-max-window-height 1)
  (setq ido-everywhere t))

;; make dired list directories first
(use-package dired
  :config
  (setq dired-listing-switches "-aBhl  --group-directories-first"))
