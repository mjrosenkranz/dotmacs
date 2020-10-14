(deftheme primary "a simple theme")

(let (
			(bg "#efefef")
			(bg+ "#ffffff")
			(fg "#000000")
			(fg+ "#555555")

			(alert "#ff0000")
			(cur "#ffff00")
			(pri "#00ff00")
			(sec "#0000ff")
			(com "#000000"))

	(custom-theme-set-faces
	 'primary
	 `(default ((t (:background ,bg :foreground ,fg))))
	 `(cursor ((t (:background ,cur))))
	 `(mode-line ((t (:background ,bg+ :foreground ,fg :box))))
	 `(mode-line-inactive ((t (:background ,bg+ :foreground ,fg+ :box))))
	 `(error (( t (:foreground ,alert))))
	 ))
(provide-theme 'primary)
(provide 'primary-theme)
