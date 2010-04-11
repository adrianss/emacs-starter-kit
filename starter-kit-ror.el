;;; starter-kit-ror.el --- Ruby on Rails helpful tools


(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;;; Yaml mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yaml"))

;;; String extensions
(require 'string-ext)

;;; Rinari (Ruby on Rails)
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
(require 'rinari)

;;; Try lunch rinari in haml or html mode
(add-hook 'haml-mode-hook
          (lambda () (rinari-launch)))

(add-hook 'html-mode-hook
          (lambda () (rinari-launch)))

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

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/completing-prompt
                             yas/dropdown-prompt))

;; Automatic Indent line in yasnippets
(setq yas/indent-line 'auto)
(setq yas/also-auto-indent-first-line t)

;; Remove scrollbars and make hippie expand
;; work nicely with yasnippet
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ;;        try-expand-dabbrev-from-kill
        ;;         try-complete-file-name
        ;;         try-complete-file-name-partially
        ;;         try-complete-lisp-symbol
        ;;         try-complete-lisp-symbol-partially
        ;;         try-expand-line
        ;;         try-expand-line-all-buffers
        ;;         try-expand-list
        ;;         try-expand-list-all-buffers
        ;;        try-expand-whole-kill
        ))

(defun indent-or-complete ()
  (interactive)
  (if (and (looking-at "$") (not (looking-back "^\\s-*")))
      (hippie-expand nil)
    (indent-for-tab-command)))
(add-hook 'find-file-hooks (function (lambda ()
                                       (local-set-key (kbd "TAB") 'indent-or-complete))))
;;; Ruby hooks
(add-hook 'ruby-mode-hook
          (lambda()
            (require 'line-num)
            (require 'ruby-electric)
            (ruby-electric-mode t)            
            ))

;;; Flymake color
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;;; Color Theme
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)

(color-theme-myror)


;;; Helper functions
;; Full screen toggle
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

(defun recenter-to-top ()
  "Take the current point and scroll it to within a
   few lines of the top of the screen."
  (interactive)
  (recenter 3))

;;; Helper Binding keys
(global-set-key [f5] 'desktop-save)
(global-set-key [f6] 'desktop-read)
(global-set-key [f7] 'cua-mode)
(global-set-key [f8] 'desktop-clear)
(global-set-key [f9] 'ecb-activate)
(global-set-key (kbd "M-n") 'toggle-fullscreen)
(global-set-key [(control shift l)] 'recenter-to-top)

(provide 'starter-kit-ror)
