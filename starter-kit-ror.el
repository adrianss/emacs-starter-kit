;;; starter-kit-ror.el --- Ruby on Rails helpful tools


(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;;; Whitespace mode
;; (require 'whitespace)

(require 'rvm)

;;; Autoclosing for '' "" [] {} etc.
(require 'autoclosing-mode)

;;; TextMate mode
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
;; (require 'textmate)
;; (textmate-mode t)

;;; Wrap Region mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/wrap-region"))
(require 'wrap-region)
(wrap-region-global-mode t)

;;; Drag Stuff mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/drag-stuff"))
(require 'drag-stuff)
(drag-stuff-global-mode t)

;;; Yaml mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yaml"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; Sass CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(setq css-indent-offset 4)
(setq css-indent-level 4)

;;; rhtml offset 
(setq sgml-basic-offset 4)

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
(add-to-list 'auto-mode-alist '("\\.eruby$" . rhtml-mode))

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
(add-hook 'ruby-mode-hook
          (function (lambda () (autoclosing-mode t))))
(add-hook 'c-mode-hook
          (function (lambda () (autoclosing-mode t))))
(add-hook 'c++-mode-hook
          (function (lambda () (autoclosing-mode t))))

;;; Dont break lines
(auto-fill-mode nil)

(defun retro-linefeed ()
  "Indent, insert line before and indent"
  (interactive)
  (back-to-indentation)
  (indent-or-complete)
  (previous-line)
  (paredit-newline))

(global-set-key (kbd "C-<return>") 'retro-linefeed)

;;; Helper Binding keys
(global-set-key [f5] 'desktop-save)
(global-set-key [f6] 'desktop-read)
(global-set-key [f7] 'cua-mode)
(global-set-key [f8] 'desktop-clear)
(global-set-key [f9] 'ecb-activate)
(global-set-key (kbd "M-n") 'toggle-fullscreen)
(global-set-key [(control shift l)] 'recenter-to-top)
;;; Completion keys
(global-set-key (kbd "C-ñ") 'hippie-expand)
(global-set-key (kbd "C-M-ñ") 'dabbrev-completion)
(global-set-key (kbd "C-M-j") 'join-line)
(global-set-key (kbd "C-º") 'indent-region)

(define-key c-mode-map (kbd "C-c c") 'compile)
;;; unbind some textmate binding asdf asdf 
;; (let ((map *textmate-mode-map*))
;;   (define-key map [(meta t)] nil)
;;   (define-key map [(meta shift t)] nil)
;;   (define-key map [(meta shift l)] nil)
;;   (define-key map [(meta return)] nil)
;;   (define-key map [(control tab)] nil)
;;   (define-key map [(control shift tab)] nil)
;;   )


;;; TODO: refactor in separate modules this functionality (find file
;;; recursively in project)
(require 'project-root)
(setq project-roots
      `(("Generic Perl Project"
         :root-contains-files ("t" "lib")
         :filename-regex ,(regexify-ext-list '(pl pm))
         :on-hit (lambda (p) (message (car p))))
        ("Django project"
         :root-contains-files ("manage.py")
         :filename-regex ,(regexify-ext-list '(py html css js))
         :exclude-paths ("media" "contrib"))
        ("Rails project"
         :root-contains-files ("app" "public")
         :filename-regex ,(regexify-ext-list '(rb html css js yml rhtml erb builder rjs xml eruby))
         :exclude-paths ("tmp" "script" "log" ".bundle" "coverage"))))

(defun my-ido-project-files ()
  "Use ido to select a file from the project."
  (interactive)
  (let (my-project-root project-files tbl)
    (unless project-details (project-root-fetch))
    (setq my-project-root (cdr project-details))

    ;; get project files
    (setq project-files
          (split-string
           (shell-command-to-string
            (concat "find "
                    my-project-root
                    " \\( -name \"*.svn\" -o -name \"*.git\" \\) -prune -o "
                    " -type f -print "
                    " | grep -E -v "
                    "\"\.(#.+|DS_Store|svn|png|jpe?g|gif|elc|rbc|pyc|swp|psd|ai|pdf|mov|aep|dmg|zip|gz|pyc)$\" "
                    )) "\n"))
    ;; populate hash table (display repr => path)
    (setq tbl (make-hash-table :test 'equal))
    (let (ido-list)
      (mapc (lambda (path)

              ;; format path for display in ido list
              (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
              ;; strip project root
              (setq key (replace-regexp-in-string my-project-root "" key))
              ;; remove trailing | or /
              (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
              (puthash key path tbl)
              (push key ido-list)
              )
            project-files
            )
      (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))

;; bind to a key for quick access
(define-key global-map [f6] 'my-ido-project-files)

(provide 'starter-kit-ror)
