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

;; tabs
(setq-default tab-width 2
              ;; Indent first then, try completions
	            tab-always-indent 'complete
	            c-basic-offset 2
	            indent-tabs-mode nil)


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ; :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy
  :after lsp)

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; ------- languages -------------

;; python
(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :config)

(use-package lsp-pyright
  :after lsp-mode
  :custom
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off")
  ;; :config
  ;; (fk/async-process
  ;;  "npm outdated -g | grep pyright | wc -l" nil
  ;;  (lambda (process output)
  ;;    (pcase output
  ;;      ("0\n" (message "Pyright is up to date."))
  ;;      ("1\n" (message "A pyright update is available.")))))
  )

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

;; typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; rust

;; completion

(use-package which-key
  :defer 0
  :config
  (which-key-mode)
  ;; Initialize.
  (setq which-key-mode-idle-delay 1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package ivy
  ;; TODO: do we need this at startup?
  :demand
  :diminish
  ;; TODO: move these to general?
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))


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

;; make a line width 80
(setq-default fill-column 80)
;; hightlight the line we on too
(global-hl-line-mode 1)

;; i want to know the column too
(setq column-number-mode t)

;; cursor
(setq cursor-in-non-selected-windows 'hollow)
(setq highlight-nonselected-windows t)
;; distracting for monkey brain
(blink-cursor-mode 0)


;; parens
(show-paren-mode 1)
;; Don't blink, it's too distracting.
(setq blink-matching-paren nil)
(setq show-paren-delay 0.2)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)

;; no word wrap
(setq-default truncate-lines t)

;; clean up the modeline
(setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
	      (border-mode-line-active bg-mode-line-active)
	      (fg-region unspecified)
	      (bg-region bg-sage)
	      (fg-hl-line unspecified)
	      (border-mode-line-inactive bg-mode-line-inactive)))

;; theme
(use-package modus-themes
  :demand t
  :init
  :config
  ;; (load-theme 'modus-operandi-tinted t)
  (load-theme 'modus-vivendi-tinted t))

;; fonts

(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'regular
                    :height 140)

(set-face-attribute 'bold nil
                    :family "Roboto Mono"
                    :weight 'medium)

(set-face-attribute 'italic nil
                    :family "Iosevka Term Medium Extended"
                    ;; :weight 'semilight
                    :slant 'italic)


;; some font adjacent stuff
(setq-default ;; what kind of weirdo uses two spaces after a period?
 sentence-end-double-space nil
 ;; supposedly faster
 bidi-paragraph-direction 'left-to-right
 ;; make truncate ellipsis
 truncate-string-ellipsis "…")


;; (use-package font-lock
;;   :defer t
;;   :custom-face
;;   (font-lock-comment-face ((t (:inherit font-lock-comment-face :italic t))))
;;   (font-lock-doc-face ((t (:inherit font-lock-doc-face :italic t))))
;;   (font-lock-string-face ((t (:inherit font-lock-string-face :italic t)))))


;; (set-fontset-font t 'unicode
;;                     (font-spec :name "Inconsolata Light"
;;                                :size 16) nil)

;; (set-fontset-font t '(#xe000 . #xffdd)
;;                      (font-spec :name "RobotoMono Nerd Font"
;;                                 :size 12) nil)


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

;; make escape do it all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) 


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
  
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "h" 'dired-up-directory
   "l" 'dired-find-file)

  ;; TODO: shift j/k while in visual mode should move the region

  (oct/leader-keys
    "g" 'magit-status
    "f" 'previous-buffer
    "j" 'next-buffer
    "b" 'counsel-switch-buffer
    "/" 'comment-line
    "n" 'evil-ex-nohighlight))

(use-package evil
  :demand t
  :init

  ;; See `undo-fu' package.
  (setq evil-undo-system 'undo-fu)
  ;; For some reasons evils own search isn't default.
  (setq evil-search-module 'evil-search)
  ;; make the minibuffer use evil mode
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)

  :config
  ;; Initialize.
  (evil-mode)

  (setq evil-ex-search-case 'sensitive))

;; get evil bindings in stuff like magit
(use-package evil-collection
  :demand t
  :after evil
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



;; ---- git ------
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ;; Make magit show changes within diff line
;; (use-package magit-diff
;;   :after magit
;;   :config
;;   (setq magit-diff-refine-hunk t))

;; ------- shell ----
;; add homebrew stuff to the path
(add-to-list 'exec-path "/opt/homebrew/bin")

(use-package direnv
  :demand
  :config
  (direnv-mode))
;; ----------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" default))
 '(package-selected-packages
   '(direnv pyvenv-mode pyenv-mode pyenv lsp-pyright pyvenv typescript-mode company-box lsp-ivy python-mode font-lock magit-diff evil-magit magit modus-themes general which-key undo-fu package-utils hl-prog-extra highlight-numbers find-file-in-project evil-surround evil-numbers diff-hl default-font-presets counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

