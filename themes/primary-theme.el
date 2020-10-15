(deftheme primary "a simple theme")

(let ((bg    "#e1e1e1")
			(bg+   "#a6a6a6")
			(pri   "#2fbe47")
			(sec   "#639ee9")
			(alert "#e96363")
			(cur   "#e9c963")
			(fill  "#ababab")
			(comm  "#b8ad7d")
			(fg    "#504a2f")
			(fg+   "#756F50"))

	(custom-theme-set-faces
	 'primary
	 `(default ((t (:background ,bg :foreground ,fg))))

	 ;; ui
	 `(cursor ((t (:background ,cur))))
	 `(mode-line ((t (:background ,bg+ :foreground ,fg :box))))
	 `(mode-line-inactive ((t (:background ,bg+ :foreground ,fg+ :box))))

	 ;; text i guess
	 `(font-lock-keyword-face ((t (:foreground ,pri))))
	 `(font-lock-doc-face ((t (:foreground ,fg+))))
	 `(font-lock-builtin-face ((t (:foreground ,fill))))
	 `(font-lock-constant-face ((t (:foreground ,sec))))
	 `(font-lock-comment-face ((t (:foreground ,comm))))
	 `(font-lock-warning-face ((t (:foreground ,cur))))

	 `(error ((t (:foreground ,alert))))))
(provide-theme 'primary)
(provide 'primary-theme)
