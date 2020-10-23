(load-file "~/.emacs.d/themes/generate-theme.el")

(deftheme primary "a simple theme")
(oct/generate-theme primary
										"#e1e1e1"  ;;bg
										"#cfcfcf"  ;;bg+  
										"#2fbe47"  ;;pri  
										"#639ee9"  ;;sec  
										"#e96363"  ;;alert
										"#F1C432"  ;;cur  
										"#888888"  ;;fill 
										"#A59655"  ;;comm 
										"#504a2f"  ;;fg   
										"#9B6E22")  ;;fg+
										
(provide-theme 'primary)
(provide 'primary-theme)
