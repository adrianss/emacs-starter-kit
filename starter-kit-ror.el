;;; starter-kit-ror.el --- Ruby on Rails helpful tools


(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;;; Whitespace mode
;; (require 'whitespace)

;;; Autoclosing for '' "" [] {} etc.
(require 'autoclosing-mode)

;;; TextMate mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(textmate-mode t)

;;; Yaml mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yaml"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; Sass CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(setq css-indent-offset 2)

;;; ZenCoding
(add-to-list 'load-path (concat dotfiles-dir "/vendor/zencoding"))
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;; String extensions
(require 'string-ext)

;;; Rinari (Ruby on Rails)
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
(require 'rinari)

(define-key rinari-minor-mode-map [(control shift down)] 'rinari-find-rspec)
(define-key rinari-minor-mode-map [(control shift left)] 'rinari-find-controller)
(define-key rinari-minor-mode-map [(control shift up)] 'rinari-find-model)
(define-key rinari-minor-mode-map [(control shift right)] 'rinari-find-view)

;;; Try lunch rinari in haml or html mode
(add-hook 'haml-mode-hook
          (lambda () (rinari-launch)))

(add-hook 'html-mode-hook
          (lambda () (rinari-launch)))

;;; rhtml mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rhtml"))
(require 'rhtml-mode)

;;; Use TAG file for find function in proyect
;;; try execute someting like: ctags-exuberant -a -e -f TAGS --tag-relative -R app lib vendor
(setq rinari-tags-file-name "TAGS")


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

;;; Yasnippet 0.6.1c stable
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet-0.6.1c"))
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory (concat dotfiles-dir "/vendor/yasnippet-0.6.1c/snippets"))

;;; Yasnippet from submodule
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet/snippets"))

;; Load this alternate directory of snippets
(yas/load-directory (concat dotfiles-dir "/vendor/snippets/rspec-snippets"))
(yas/load-directory (concat dotfiles-dir "/vendor/snippets/mixed-snippets"))

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

;;; Line numering
(require 'line-num)

;;; Ruby hooks
(add-hook 'ruby-mode-hook
          (lambda()
            (require 'ruby-electric)
            (ruby-electric-mode t)
            (linum-mode t)
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

;;; Set the tab indent to 4 spaces for javascript
(setq espresso-indent-level 4)

;;; Autoclosing hook for programming modes
(add-hook 'espresso-mode-hook
          (function (lambda () (autoclosing-mode t))))
(add-hook 'sgml-mode-hook
          (function (lambda () (autoclosing-mode t))))
(add-hook 'haml-mode-hook
          (function (lambda () (autoclosing-mode t))))

;;; Helper Binding keys
(global-set-key [f5] 'desktop-save)
(global-set-key [f6] 'desktop-read)
(global-set-key [f7] 'cua-mode)
(global-set-key [f8] 'desktop-clear)
(global-set-key [f9] 'ecb-activate)
(global-set-key (kbd "M-n") 'toggle-fullscreen)
(global-set-key [(control shift l)] 'recenter-to-top)

(provide 'starter-kit-ror)
