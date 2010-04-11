;;; starter-kit-ror.el --- Ruby on Rails helpful tools

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(add-to-list 'load-path (concat dotfiles-dir "/vendor/yaml"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
(require 'rinari)

;;; Haml mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(provide 'starter-kit-ror)
