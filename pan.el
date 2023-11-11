(define-minor-mode panel-mode
  nil ; disabled by default
  :global t
  :lighter " panel"
  (if panel-mode
      (message "panel-mode2 turned on!")
      (message "panel-mode2 turned off!")))

  

(defun panel-mode--init ()
  ;; add hooks for every time a window is created, deleted, moved
  ;; or a frame is created, delted

  (panel-mode--init-advice)
  )


(defun panel-mode--deinit ()
  )

;; hook shit
;; after-frame-functions
;; delete-frame-functions
;; kill-buffer-query-functions


;; advice shit
          ;; (ad-activate 'switch-to-buffer)
          ;; (ad-activate 'display-buffer)
          ;; (ad-activate 'set-window-buffer)
          ;; (ad-activate 'switch-to-prev-buffer)
          ;; (ad-activate 'recursive-edit)
          ;; (ad-activate 'exit-recursive-edit)

(defun panel-mode--init-advice ()
    (ad-activate 'switch-to-buffer)
  )

(current-window-configuration)
#<window-configuration>

(setq panel-current-window-configuration (current-window-configuration))

(set-window-configuration panel-current-window-configuration)
