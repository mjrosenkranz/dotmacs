(require 'generate-theme)

(deftheme underwater "a simple theme")
(oct/generate-theme underwater
										"#2E383E"  ;;bg   
										"#4E5E69"  ;;bg+  
										"#4c8ea6"  ;;pri  
										"#579982"  ;;sec  
										"#6AAC4A"  ;;alert
										"#D7B020"  ;;cur  
										"#38677E"  ;;fill 
										"#94A5B0"  ;;comm 
										"#DCD8C5"  ;;fg   
										"#82A4AF")  ;;fg+
										

(provide-theme 'underwater)
(provide 'underwater-theme)
