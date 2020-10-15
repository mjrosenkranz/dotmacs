;; setup package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; make sure that it gets initialized
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; update package list
(package-refresh-contents)

;; get `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))


(let* ((conf "~/.emacs.d/init-def")
       (el (concat conf ".el"))
       (org (concat conf ".org")))
    (org-babel-load-file org))
