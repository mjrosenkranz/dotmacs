;; setup package
(require 'package)
(setq package-archives
						 '(("melpa" . "https://melpa.org/packages/")
							 ("org" . "https://orgmode.org/elpa/")))

;; make sure that it gets initialized
(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

;; get `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(let* ((conf "~/.emacs.d/init-def")
       (el (concat conf ".el"))
       (org (concat conf ".org")))
    (org-babel-load-file org))
