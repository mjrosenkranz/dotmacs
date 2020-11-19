(load-file "~/.emacs.d/themes/generate-theme.el")

(deftheme primary "a simple theme")
(oct/generate-theme primary
										"#fffbeb"  ;;bg
										"#EDDFA4"  ;;bg+
										"#504a2f"  ;;fg
										"#9B6E22"  ;;fg+
										"#2fbe47"  ;;pri
										"#639ee9"  ;;sec
										"#e96363"  ;;alert
										"#F1C432"  ;;cur
										"#A59655"  ;;comm
										"#F5EED3"  ;;block
)
										
(provide-theme 'primary)
(provide 'primary-theme)
