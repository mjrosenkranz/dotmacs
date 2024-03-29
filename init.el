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

(setq custom-file
        "~/.emacs.d/custom-settings.el")
  (load custom-file t)

;; uh idk where to put spell stuff
(setq ispell-program-name "aspell")

;; theme
;; (use-package modus-themes
;;   :demand t
;;   :init
;;   (setq modus-themes-common-palette-overrides
;;         '((border-mode-line-active bg-mode-line-active)
;;           (border-mode-line-active bg-mode-line-active)
;;           (fg-region unspecified)
;;           (bg-region bg-sage)
;;           (fg-hl-line unspecified)
;;           (border-mode-line-inactive bg-mode-line-inactive)))
;;   :config
;;   (load-theme 'modus-operandi-tinted t))

(use-package catppuccin-theme
  :demand t
  :config
  (setq catppuccin-flavor 'macchiato)
  (load-theme 'catppuccin t))


;; --- editing ---

;; utf 8 only
(prefer-coding-system 'utf-8)
;; make the clipboard work right
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
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
  :hook (lsp-completion-mode . mjr/setup-completion)
  :init
  (defun mjr/setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-completion-provider :none)
  (lsp-eldoc-enable-hover nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package dap-mode
  :custom
  (dap-auto-configure-mode t)
  (dap-auto-configure-features '(sessions locals breakpoonts expressions))
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

;; ------- Languages -------------

(setq treesit-language-source-alist
   '((cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '(
   (python-mode . python-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (css-mode . css-ts-mode)
   ;; (yaml-mode . yaml-ts-mode)
   ;; (bash-mode . bash-ts-mode)
   ;; (js2-mode . js-ts-mode)
   ;; (json-mode . json-ts-mode)
   ))

;; python
(use-package python-mode
  :hook (python-ts-mode . lsp-deferred)
  :hook (python-ts-mode . (lambda ()
                         (modify-syntax-entry ?_ "w" python-mode-syntax-table)
                         (modify-syntax-entry ?- "w" python-mode-syntax-table))))

(use-package lsp-pyright
  :after lsp-mode
  :config
  (setq python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose nil)
  :custom
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off"))

;; cpp
(use-package cc-mode
  :hook (c++-mode . lsp-deferred)
  :hook (c++-mode . (lambda ()
                         (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
                         (modify-syntax-entry ?- "w" c++-mode-syntax-table))))


  
(use-package cmake-mode
  :ensure t)
  




;; typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . lsp-deferred)
  :hook (tsx-ts-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 4))

;; rust

(use-package rustic
  :hook (rustic-mode . lsp-deferred)
  :hook (rustic-mode . (lambda ()
                         (modify-syntax-entry ?_ "w" rustic-mode-syntax-table)
                         (modify-syntax-entry ?- "w" rustic-mode-syntax-table)))
  :ensure)

;; zig
(use-package zig-mode
  :hook (zig-mode . lsp-deferred)
  :hook (rustic-mode . (lambda ()
                         (modify-syntax-entry ?_ "w" zig-mode-syntax-table)
                         (modify-syntax-entry ?- "w" zig-mode-syntax-table)))
  :ensure)

;; nix
(use-package nix-mode
  :mode "\\.nix\\'")

;; protobuf
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; completion

(use-package which-key
  :defer 0
  :config
  (which-key-mode)
  ;; Initialize.
  (setq which-key-mode-idle-delay 1))

(use-package corfu
  ;; :custom
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local
                        corfu-quit-at-boundary 'separator
                        corfu-quit-no-match t
                        corfu-auto nil)
              (corfu-mode)))
  (defun mjr/minibuffer-corfu ()
              (unless (or (bound-and-true-p mct--active)
                          (bound-and-true-p vertico--input)
                          (eq (current-local-map) read-passwd-map))
                ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
                (setq-local corfu-auto t
                            corfu-auto-prefix 2
                            corfu-echo-delay 0.0
                            corfu-popupinfo-delay nil)
                (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'mjr/minibuffer-corfu 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  ;; only need two letters to start completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-preselect-first nil)
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("RET" . nil) ; quit playin with  my cr
              ("TAB" . corfu-insert)
              ("C-n"  . corfu-next)
              ("C-p" . corfu-previous)))

(use-package cape
  :defer 10
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
  (cl-pushnew #'cape-file completion-at-point-functions)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :demand
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after vertico
  :demand)

(use-package embark
  :demand
  :bind
  (("C-<escape>" . embark-act)
   ([remap describe-bindings] . embark-bindings)))

(use-package embark-consult
  :after (:all embark consult))
              
(use-package marginalia
  :demand
  :init (marginalia-mode))
              
(use-package savehist
  :init
  (savehist-mode))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


;; ---settings---

;; ----- ui -----

;; frame ui: 
;; get rid of clutter in menu bar
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tab-bar-mode t)
(scroll-bar-mode -1)
(tooltip-mode -1)
;; set only left fringe
(set-fringe-mode '(nil . 0))

;; TODO: try just minions?
(use-package mood-line
  :demand t
  ;; Enable mood-line
  ;; :config
  ;; (mood-line-mode)
  )

;; (use-package minions
;;   :init (minions-mode))

 (setq-default mode-line-format
                      '((:eval
                         (mood-line--format
                          ;; Left
                          (format-mode-line
                           '(" "
                             evil-mode-line-tag
                             (:eval (mood-line-segment-modified))
                             (:eval (mood-line-segment-buffer-name))
                             (:eval (mood-line-segment-anzu))
                             (:eval (mood-line-segment-multiple-cursors))
                             (:eval (mood-line-segment-position))))

                          ;; Right
                          (format-mode-line
                           '((:eval (mood-line-segment-eol))
                             (:eval (mood-line-segment-encoding))
                             (:eval (mood-line-segment-vc))
                             (:eval (mood-line-segment-major-mode))
                             (:eval (mood-line-segment-misc-info))
                             (:eval (mood-line-segment-flycheck))
                             (:eval (mood-line-segment-flymake))
                             (:eval (mood-line-segment-process))
                             " "))))))



(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :init
  (git-gutter)
  (set-face-foreground 'vertical-border (face-attribute 'line-number :background))
  :config
  (setq git-gutter:update-interval 0.02))


(use-package git-gutter-fringe
  :demand t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [255] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [255] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [255] nil nil '(center repeated)))


;; make the title the buffer name
(setq-default frame-title-format "%b %&")
;; (setq-default frame-title-format '((:eval
;;                                     (format "%s: %s"
;;                                             (persp-name (get-current-persp))
;;                                             (buffer-name)))
;;                                    "%&"))

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

(defun display-line-numbers--turn-on ()
  "Turn on `display-line-numbers-mode'."
  (unless (minibufferp)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative)))

(setq-default display-line-numbers-widen t)
(setq-default display-line-numbers-width-start t)

;; make a line width 80
(setq-default fill-column 80)
;; pulse the line whenever we switch windows
(use-package pulsar
  :demand t
  :init
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-highlight-face 'pulsar-green)
  :custom
  (pulsar-pulse-functions
   '(recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     bookmark-jump
     other-window
     delete-window
     delete-other-windows
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     ;; windows and buffers
     next-buffer
     previous-buffer
     windmove-right
     windmove-left
     windmove-up
     windmove-down
     windmove-swap-states-right
     windmove-swap-states-left
     windmove-swap-states-up
     windmove-swap-states-down
     ;; evil
     evil-ex-search-next
     evil-ex-search-previous
     evil-ex-search-word-forward
     evil-ex-search-word-backward
     evil-forward-paragraph
     evil-backward-paragraph
     evil-jump-forward
     evil-jump-backward
     ;; tabs
     tab-new
     tab-close
     tab-next
     ;; org
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading)
   )
  :config
  (pulsar-global-mode 1))

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
(setq-default truncate-lines nil)
(global-visual-line-mode t)


;; show colors with text stuff
(use-package rainbow-mode
  :demand
  :config (rainbow-mode))

;; fonts

(defvar mjr/font-height 140)

(defun mjr/change-font-height (amt)
  (interactive)
  (setq mjr/font-height (+ amt mjr/font-height))
  (set-face-attribute 'default nil :height mjr/font-height))

(defun mjr/inc-font-height ()
  (interactive)
  (mjr/change-font-height 10))

(defun mjr/dec-font-height ()
  (interactive)
  (mjr/change-font-height -10))

(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'regular
                    :height mjr/font-height)

(set-face-attribute 'bold nil
                    :family "Roboto Mono"
                    :weight 'medium)

(set-face-attribute 'italic nil
                    :family "Iosevka Term Light"
                    :slant 'italic
                    :width 'expanded)


;; some font adjacent stuff
(setq-default ;; what kind of weirdo uses two spaces after a period?
 sentence-end-double-space nil
 ;; supposedly faster
 bidi-paragraph-direction 'left-to-right
 ;; make truncate ellipsis
 truncate-string-ellipsis "…")


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
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit) 


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
   ;; TODO: add window swapping
   :states '(normal insert)
   :keymaps 'override
   "s-M-<return>" 'multi-vterm-project
   "s-<return>" 'mjr/eshell
   "s-t" 'tab-new
   "s-+" 'mjr/inc-font-height
   "s-<kp-add>" 'mjr/inc-font-height
   "s--" 'mjr/dec-font-height
   ;; todo: close window if last tab
   "C-S-V" 'yank
   "s-w" 'tab-close
   "M-k" 'windmove-up
   "M-j" 'windmove-down
   "M-h" 'windmove-left
   "M-l" 'windmove-right)

 
  (general-define-key
   :states '(visual)
   :keymaps 'override
   "K" 'drag-stuff-up
   "J" 'drag-stuff-down)

  (general-define-key
   :keymaps 'evil-motion-state-map
   "g D" 'goto-def-in-new-window
   "z z" 'pulsar-recenter-center)

  (general-define-key
   :keymaps 'ctl-x-map 
   "K" 'kill-buffer-and-window)

  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-o" 'embark-export)

  (general-define-key
   ;; lets us kill the minibuffer with escape
   ;; while in normal mode.
   :states '(normal)
   :keymaps 'minibuffer-local-map
   "<escape>" 'keyboard-escape-quit)

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "h" 'dired-up-directory
   "l" 'dired-find-file)

  (general-define-key
   "C-c p" '(:keymap projectile-command-map :package projectile)
   ;; "s-<return>" 'multi-vterm-project
   "C-<return>" 'projectile-run-eshell)
  ;; TODO: shift j/k while in visual mode should move the region

  (general-define-key
   :keymaps 'eshell-mode-map
   "C-l" (lambda ()
           (interactive)
           ;; (eshell/clear 1)
           (recenter 0)))

  (oct/leader-keys
    "u" 'universal-argument
    "g" 'magit-status
    "," 'rename-buffer
    ;; "$" 'persp-rename
    ;; "s" 'persp-switch
    ;; "S" 'persp-add-new
    ;; "b" '(lambda () (interactive) (with-persp-buffer-list () (consult-buffer)))
    "b" 'consult-buffer
    "C-n" 'next-buffer
    "C-p" 'previous-buffer
    "f" '(:ignore t)
    "fd" 'dired-jump
    "ff" 'projectile-find-file
    "fo" 'projectile-find-file-other-window
    "fb" 'projectile-switch-to-buffer
    "fg" 'consult-ripgrep
    ;; "fg" 'mjr/rg-dir
    "/" 'comment-line
    "n" 'evil-ex-nohighlight
    "t" 'tab-bar-new-tab
    ;; lsp shit
    "r" '(:ignore t)
    "rr" 'lsp-find-references
    "rn" 'lsp-rename
    "k" 'lsp-describe-thing-at-point
    "a" 'lsp-execute-code-action))

(use-package evil
  :demand t
  :init

  ;; See `undo-fu' package.
  (setq evil-undo-system 'undo-fu)
  ;; For some reasons evils own search isn't default.

  (setq evil-search-module 'evil-search)
  ;; make the minibuffer use evil mode
  (setq evil-want-minibuffer nil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  ;; Initialize.
  (evil-mode)
  ;; set visual lines
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)


  (setq evil-ex-search-case 'sensitive)

  ;; function for opening a definition in a new window
  (defun goto-def-in-new-window ()
    "opens the definition under the cursor in a vsplit"
    (interactive)
    (select-window
     (split-window (selected-window) nil 'right))
    (evil-goto-definition)))

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

(use-package evil-numbers
  :demand t)

(use-package evil-anzu
  :after evil
  :custom (anzu-cons-mode-line-p nil)
  :init
  (require 'evil-anzu)
  (global-anzu-mode))

(use-package drag-stuff
  :demand t
  :config
  ;; Initialize.
  (drag-stuff-mode 1))

;; make undo work how we expect
(use-package undo-fu)

;; ---- git ------
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package forge
;;   :after magit)

(use-package github-review
  :after magit)


;; ------- shell ----
;; add homebrew stuff to the path
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "~/.nix-profile/bin")

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t)

;; environment
(setq dired-use-ls-dired nil)

(use-package envrc
  :demand
  :config
  (envrc-global-mode))

(use-package eat
  :hook (eshell-mode . (lambda ()
                         (make-local-variable 'scroll-margin)
                         (setq scroll-margin 0)
                         (remove-hook
                          'eshell-output-filter-functions
                          'eshell-postoutput-scroll-to-bottom)))
  :init
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  :config
  (setq eshell-visual-commands nil)
  (defalias 'ff 'find-file)
  (defalias 'ffo 'find-file-other-window))

;; (use-package capf-autosuggest
;;   :hook (eshell-mode . capf-autosuggest-mode))

;; --- mjr functions ---
(defun mjr/eshell ()
  (interactive)
  (eshell 'N))

;; projects

(use-package projectile
  :demand
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default))
  ;; :bind-keymap
  ;; ("C-c p" . projectile-command-map)
  :init
  ;; look for code in the... code dir
  (setq projectile-completion-system 'default)
  (setq projectile-project-search-path '("~/code"))
  ;; add this project
  (projectile-add-known-project "~/.emacs.d")
  ;; add dots
  (projectile-add-known-project "~/dots")
  (setq projectile-switch-project-action #'projectile-dired))

;; (use-package persp-mode
;;   :custom ((persp-keymap-prefix nil))
;;   :config
;;    (setq persp-add-buffer-on-after-change-major-mode t)
;;   :init (persp-mode))


(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))
(setq gc-cons-threshold (* 2 1000 1000))

(defun mjr/rg-dir ()
    (interactive)
    (let ((dir (file-name-directory buffer-file-name)))
        (counsel-rg nil dir nil (concat dir ": "))))

;; --- windows --
;; if you try to open a window in a dedicated windo then
;; we move the new window somewhere else
(setq switch-to-buffer-in-dedicated-window 'pop)
;; make switch to buffer obey the display buffer stuff
(setq switch-to-buffer-obey-display-actions t)

(setq display-buffer-alist nil)
(add-to-list 'display-buffer-alist
             '("\\*lsp-help\\*"
               (display-buffer-in-side-window display-buffer-reuse-window)
               (side . bottom)
               (slot . 0)))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :init (setq aw-dispatch-always t))

;; --- org mode ---
(use-package org
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))
