;; startup/packages

;; (defun display-startup-time ()
;;   (message "emacs loaded in %s with %d gc"
;; 	   (format "%.2f seconds"
;; 		   (float-time
;; 		    (time-subtract after-init-time before-init-time)))
;; 	   gcs-done))
;; (add-hook 'emacs-startup-hook #'display-startup-time)


(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; auto install use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This is only needed once, near the top of the file
(eval-when-compile (require 'use-package))
;; always install all the packages
(setq use-package-always-ensure t)
;; wait to load packages by default
(setq use-package-always-defer t)


;; editing

;; utf 8 only
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; ---settings---

;; ----- ui -----

;; frame ui: 
;; get rid of clutter in menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; make the title the buffer name
(setq-default frame-title-format "%b %&")
;; no dialog boxes
(setq use-dialog-box nil)
;; just y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; please for the love of god no bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; theme
(use-package modus-themes
  :demand t
  :init
  :config
  (load-theme 'modus-vivendi t))

;; completion

(use-package which-key
  :demand t
  :config
  ;; Initialize.
  (which-key-mode))

;; keybindings 

(use-package evil
  :demand t
  :init

  ;; See `undo-fu' package.
  (setq evil-undo-system 'undo-fu)
  ;; For some reasons evils own search isn't default.
  (setq evil-search-module 'evil-search)
  ;; make the minibuffer use evil mode
  (setq evil-want-minibuffer t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  ;; Initialize.
  (evil-mode)

  (setq evil-ex-search-case 'sensitive))

(use-package evil-surround
  :demand t
  :config
  ;; Initialize.
  (global-evil-surround-mode 1))

;; keybindings

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-create-definer oct/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-define-key
    "M-k" 'evil-window-up
    "M-j" 'evil-window-down
    "M-h" 'evil-window-left
    "M-r" 'evil-window-left)

  ;; TODO: shift j/k while in visual mode should move the region

  (oct/leader-keys
   "g" '(:ignore t)
   "gs" 'magit-status
   "f" 'previous-buffer
   "j" 'next-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" default))
 '(package-selected-packages
   '(modus-themes general which-key undo-fu package-utils hl-prog-extra highlight-numbers find-file-in-project evil-surround evil-numbers diff-hl default-font-presets counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

