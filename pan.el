(define-minor-mode panel-mode
  nil ; disabled by default
  :global t
  :lighter " panel"
  (if panel-mode
      (message "panel-mode2 turned on!")
      (message "panel-mode2 turned off!")))
  
