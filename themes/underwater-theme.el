(load-file "~/.emacs.d/themes/generate-theme.el")

(deftheme underwater "a simple theme")
(oct/generate-theme underwater
										"#2E383E"  ;;bg
										"#4E5E69"  ;;bg+  
										"#DCD8C5"  ;;fg   
										"#82A4AF" ;;fg+
										"#4c8ea6"  ;;pri  
										"#6AAC4A"  ;;sec  
										"#e29726"  ;;alert
										"#D7B020"  ;;cur  
										"#94A5B0"  ;;comm 
										"#262e33" ;;block
										)
										

(provide-theme 'underwater)
(provide 'underwater-theme)
