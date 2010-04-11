;;; starter-kit-ror.el --- Ruby on Rails helpful tools


(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;;; Yaml mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yaml"))

;;; Rinari (Ruby on Rails)
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
(require 'rinari)

;;; Haml mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;; Sass mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;;; Feature mode (Cucumber)
(add-to-list 'load-path (concat dotfiles-dir "/vendor/feature"))
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;;; Yasnippet
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet-0.6.1c"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet-0.6.1c/snippets"))

(add-hook 'ruby-mode-hook
          (lambda()
            (require 'line-num)
            ))


(provide 'starter-kit-ror)
