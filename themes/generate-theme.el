(defmacro oct/generate-theme (theme bg bg+ pri sec alert cur fill comm fg fg+)

	(custom-theme-set-faces
	 theme
	 `(default ((t (:background ,bg :foreground ,fg))))

	 ;; ui
	 `(cursor ((t (:background ,cur))))
	 `(fringe ((t (:foreground ,bg))))
	 ;; line numbers
	 `(line-number ((t (:background ,bg :foreground ,comm))))
	 `(linum ((t (:background ,bg :foreground ,bg+))))
	 `(line-number-current-line ((t (:background ,bg :foreground ,fg+))))
	 `(line-number-major-tick ((t (:background ,bg :foreground ,fg))))
	 `(line-number-minor-tick ((t (:background ,bg :foreground ,fg))))
	 ;; modeline
	 `(mode-line ((t (:background ,bg+ :foreground ,fg :box))))
	 ;; `(mode-line-buffer-id)
	 ;; `(mode-line-emphasis )
	 ;; `(mode-line-highlight)
	 ;; `(mode-line-inactive )
	 `(mode-line-inactive ((t (:background ,bg+ :foreground ,fg+ :box))))
	 ;; minibuffer
	 `(minibuffer-prompt ((t (:foreground ,pri))))
	 ;; editor faces
	 `(font-lock-builtin-face ((t (:foreground ,fg :slant italic :weight semi-bold))))
	 `(font-lock-comment-face ((t (:foreground ,comm :slant italic))))
	 `(font-lock-comment-delimiter-face ((t (:foreground ,comm :slant italic))))
	 `(font-lock-constant-face ((t (:foreground ,alert))))
	 `(font-lock-function-name-face ((t (:foreground ,pri))))
	 `(font-lock-keyword-face ((t (:foreground ,sec :slant italic))))
	 `(font-lock-doc-face ((t (:foreground ,fg+))))
	 `(font-lock-string-face ((t (:foreground ,fg+))))
	 `(font-lock-negation-char-face ((t (:foreground ,fg))))
	 `(font-lock-preprocessor-face ((t (:foreground ,alert))))
	 `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg+))))
	 `(font-lock-regexp-grouping-construct ((t (:foreground ,fg))))
	 `(font-lock-type-face ((t (:foreground ,alert))))
	 `(font-lock-variable-name-face ((t (:foreground ,sec))))
	 `(font-lock-warning-face ((t (:foreground ,cur))))
	 ;; highlight
	 `(region ((t (:background ,bg+ :foreground ,fg))))
	 `(highlight ((t (:inherit region))))
	 `(isearch ((t (:background ,pri :foreground ,bg))))
	 `(isearch-fail ((t (:background ,alert :foreground ,bg))))
	 `(lazy-highlight ((t (:inherit region))))

	 ;; evil
	 
	 `(evil-ex-commands ((t (:foreground ,pri))))
	 `(evil-ex-info ((t (:foreground ,bg+))))
	 ;; `(evil-ex-lazy-highlight
	 ;; `(evil-ex-search
	 ;; `(evil-ex-substitute-matches
	 ;; `(evil-ex-substitute-replacement

	 ;; company mode
	 `(company-echo ((t (:foreground ,fg, :background ,bg))))
	 `(company-echo-common ((t (:foreground ,fg+, :background ,bg))))
	 `(company-preview ((t (:foreground ,fg, :background ,bg+))))
	 `(company-preview-common ((t (:inherit 'company-preview :foreground ,pri))))
	 `(company-preview-search ((t (:inherit 'company-preview :foreground ,sec))))
	 ;; company tooltip
	 `(company-scrollbar-bg ((t (:background ,bg+))))
	 `(company-scrollbar-fg ((t (:background ,fg))))
	 `(company-template-field ((t (:foreground ,fg :background ,bg+))))
	 `(company-tooltip ((t (:background ,bg+))))
	 `(company-tooltip-annotation ((t (:foreground ,sec))))
	 `(company-tooltip-common ((t (:inherit 'company-tooltip-annotation))))
	 `(company-tooltip-selection ((t (:foreground ,bg :background ,pri))))
	 `(company-tooltip-annotation-selection ((t (:inherit 'company-tooltip-selection :weight bold))))
	 `(company-tooltip-common-selection ((t (:inherit 'company-tooltip-selection :weight bold))))

	 ;; magit
	 ;; `(magit-diff-added ((t (:background ,bg+ :foreground ,pri))))

	 ;; terminal

	 ;; rainbow parens and stuff
	 `(rainbow-delimiters-base-error-face ((t (:foreground ,alert :weight bold))))
	 `(rainbow-delimiters-base-face ((t (:foreground ,fg+))))
	 `(rainbow-delimiters-depth-1-face ((t (:foreground ,pri))))
	 `(rainbow-delimiters-depth-2-face ((t (:foreground ,sec))))
	 `(rainbow-delimiters-depth-3-face ((t (:foreground ,cur))))
	 `(rainbow-delimiters-depth-4-face ((t (:foreground ,fill))))
	 `(rainbow-delimiters-depth-5-face ((t (:foreground ,pri))))
	 `(rainbow-delimiters-depth-6-face ((t (:foreground ,sec))))
	 `(rainbow-delimiters-depth-7-face ((t (:foreground ,cur))))
	 `(rainbow-delimiters-depth-8-face ((t (:foreground ,fill))))
	 `(rainbow-delimiters-depth-9-face ((t (:foreground ,pri))))
	 `(rainbow-delimiters-mismatched-face ((t (:inherit 'rainbow-delimiters-base-error-face))))
	 `(rainbow-delimiters-unmatched-face  ((t (:inherit 'rainbow-delimiters-base-error-face))))

	 ;; flycheck/make
	 `(flycheck-error ((t (:underline (:style wave :color ,alert)))))
	 `(flycheck-delimited-error ((t (:inherit 'flycheck-error))))
	 `(flycheck-error-delimiter ((t (:inherit 'flycheck-error))))
	 `(flycheck-fringe-error ((t (:inherit 'error))))
	 `(flycheck-fringe-info ((t (:foreground ,pri))))
	 `(flycheck-fringe-warning ((t (:foreground ,cur))))
	 `(flycheck-info ((t (:underline (:style wave :color ,pri)))))
	 `(flycheck-warning ((t (:underline (:style wave :color ,cur)))))
	 `(flycheck-error-list-checker-name ((t (:inherit 'font-lock-function-name-face))))
	 `(flycheck-error-list-column-number ((t (:inherit 'default))))
	 `(flycheck-error-list-error ((t (:inherit 'error))))
	 `(flycheck-error-list-error-message ((t (:inherit 'default))))
	 `(flycheck-error-list-filename ((t (:foreground ,pri :weight bold))))
	 `(flycheck-error-list-highlight ((t (:inherit 'default :weight bold))))
	 `(flycheck-error-list-id ((t (:inherit 'font-lock-type-face))))
	 `(flycheck-error-list-id-with-explaine ((t (:inherit 'font-lock-type-face))))
	 `(flycheck-error-list-info ((t (:foreground ,pri :weight bold))))
	 `(flycheck-error-list-line-number ((t (:inherit 'default))))
	 `(flycheck-error-list-warning ((t (:foreground ,cur :weight bold))))

	 `(flymake-error ((t (:inherit 'flycheck-error))))
	 `(flymake-note ((t (:inherit 'flycheck-info))))
	 `(flymake-warning ((t (:inherit 'flycheck-warning))))
	 `(flyspell-duplicate ((t (:inherit 'flycheck-warning))))
	 `(flyspell-incorrect ((t (:inherit 'flycheck-error))))


	 ;; org
	 ;;`(org-block-begin-line ((t (:background ,bg+ :underline ,fg+ :extend nil))))
	 `(org-block-begin-line ((t (:inherit 'default))))
	 `(org-block ((t (:background ,bg+ :extend t))))
	 `(org-block-end-line ((t (:inherit 'default))))
	 ;; `(org-block-end-line ((t (:background ,bg+ :overline ,fg+ :extend nil))))

	 ;; misc
	 `(link ((t (:foreground ,sec))))

	 `(link-visited ((t (:foreground ,sec))))
	 `(error ((t (:foreground ,alert :weight bold))))
	 `(warning ((t (:foreground ,cur :weight bold))))
	 `(secondary-selection ((t (:background ,bg+ :foreground ,fg :box))))

	 ))
(provide 'generate-theme)
