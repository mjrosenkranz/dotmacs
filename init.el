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


;; --- editing ---

;; utf 8 only
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; completion

(use-package which-key
  :defer 0
  :config
  (which-key-mode)
  ;; Initialize.
  (setq which-key-mode-idle-delay 1))


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

;; line numbers
;; display them always
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
(setq-default display-line-numbers-widen t)

;; i want to know the column too
(setq column-number-mode t)

;; cursor
(setq cursor-in-non-selected-windows 'hollow)
(setq highlight-nonselected-windows t)


;; parens
(show-paren-mode 1)
;; Don't blink, it's too distracting.
(setq blink-matching-paren nil)
(setq show-paren-delay 0.2)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)

;; no word wrap
(setq-default truncate-lines t)

;; theme
(use-package modus-themes
  :demand t
  :init
  :config
  (load-theme 'modus-vivendi t))

;; fonts


;; ----- ux ------
;; scrolling

;; give some room at the top and bottom
(setq scroll-margin 8)
(setq scroll-conservatively scroll-margin)

;; only scroll by one when moving out of frame
(setq scroll-step 1)
;; keep same position
(setq scroll-preserve-screen-position t)

;; make the scrolling based on pixels not lines
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 40.0)

;; always redraw immediately when scrolling
(setq fast-but-imprecise-scrolling nil)
(setq jit-lock-defer-time 0)

(setq mouse-wheel-scroll-amount '(6 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-inhibit-click-time nil)

(setq scroll-error-top-bottom t)
(setq next-error-recenter (quote (4)))


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

;; get evil bindings in stuff like magit
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :demand t
  :config
  ;; Initialize.
  (global-evil-surround-mode 1))

;; make undo work how we expect
(use-package undo-fu)

(use-package swiper
  :commands (swiper)
  :config

  ;; Go to the start of the match instead of the end. Why?
  ;; .. allows us to operate on the term just jumped to (look up reference for e.g.)
  (setq swiper-goto-start-of-match t))


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
   "j" 'next-buffer
   "n" 'evil-ex-nohighlight)

;; ---- git ------
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ;; Make magit show changes within diff line
;; (use-package magit-diff
;;   :after magit
;;   :config
;;   (setq magit-diff-refine-hunk t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" default))
 '(package-selected-packages
   '(magit-diff evil-magit magit modus-themes general which-key undo-fu package-utils hl-prog-extra highlight-numbers find-file-in-project evil-surround evil-numbers diff-hl default-font-presets counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

