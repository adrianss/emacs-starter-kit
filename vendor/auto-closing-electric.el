;; -*-Emacs-Lisp-*-
;;
;; auto-closing-electric.el --- electric editing commands
;;
;; Usage:
;;
;;    0) copy auto-closing-electric.el into directory where emacs can find it.
;;
;;    1) modify your startup file (.emacs or whatever) by adding
;;       following line:
;;
;;            (require 'auto-closing-electric)
;;
;;       note that you need to have font lock enabled beforehand.
;;
;;    2) toggle Auto Closing Electric Mode on/off with:
;;            (auto-closing-electric t)
;;

(defgroup auto-closing-electric nil
  "Minor mode providing electric editing commands for auto closing files"
  :group 'text)

(defconst auto-closing-electric-expandable-bar
  "\\s-\\(do\\|{\\)\\s-+|")

(defvar auto-closing-electric-matching-delimeter-alist
  '((?\[ . ?\])
    (?\( . ?\))
    (?\' . ?\')
    (?\` . ?\`)
    (?\" . ?\")))

(defcustom auto-closing-electric-expand-delimiters-list '(all)
  "*List of contexts where matching delimiter should be
inserted. The word 'all' will do all insertions."
  :type '(set :extra-offset 8
              (const :tag "Everything" all )
              (const :tag "Curly brace" ?\{ )
              (const :tag "Square brace" ?\[ )
              (const :tag "Round brace" ?\( )
              (const :tag "Quote" ?\' )
              (const :tag "Double quote" ?\" )
              (const :tag "Back quote" ?\` )
              (const :tag "Vertical bar" ?\| ))
  :group 'auto-closing-electric)

(defcustom auto-closing-electric-newline-before-closing-bracket nil
  "*Controls whether a newline should be inserted before the
closing bracket or not."
  :type 'boolean :group 'auto-closing-electric)

(define-minor-mode auto-closing-electric
  "Toggle Auto Closing Electric minor mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode."
  ;; initial value.
  nil
  ;;indicator for the mode line.
  " ACEl"
  ;;keymap
  text-mode-map
  (auto-closing-electric-setup-keymap))

(defun auto-closing-electric-setup-keymap()
  (define-key text-mode-map "{" 'auto-closing-electric-curlies)
  (define-key text-mode-map "(" 'auto-closing-electric-matching-char)
  (define-key text-mode-map "[" 'auto-closing-electric-matching-char)
  (define-key text-mode-map "\"" 'auto-closing-electric-matching-char)
  (define-key text-mode-map "\'" 'auto-closing-electric-matching-char)
  (define-key text-mode-map "|" 'auto-closing-electric-bar))


(defun auto-closing-electric-code-at-point-p()
  (and auto-closing-electric
       (let* ((properties (text-properties-at (point))))
         (and (null (memq 'font-lock-string-face properties))
              (null (memq 'font-lock-comment-face properties))))))

(defun auto-closing-electric-string-at-point-p()
  (and auto-closing-electric
       (consp (memq 'font-lock-string-face (text-properties-at (point))))))

(defun auto-closing-electric-is-last-command-char-expandable-punct-p()
  (or (memq 'all auto-closing-electric-expand-delimiters-list)
      (memq last-command-event auto-closing-electric-expand-delimiters-list)))

(defun auto-closing-electric-curlies(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (auto-closing-electric-is-last-command-char-expandable-punct-p)
      (cond ((auto-closing-electric-code-at-point-p)
             (insert " ")
             (save-excursion
               (if auto-closing-electric-newline-before-closing-bracket
                   (newline))
               (insert "}")))
            ((auto-closing-electric-string-at-point-p)
             (save-excursion
               (backward-char 1)
               (when (char-equal ?\# (preceding-char))
                 (forward-char 1)
                 (insert "}")))))))

(defun auto-closing-electric-matching-char(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and (auto-closing-electric-is-last-command-char-expandable-punct-p)
       (auto-closing-electric-code-at-point-p)
       (save-excursion
         (insert (cdr (assoc last-command-event
                             auto-closing-electric-matching-delimeter-alist))))))

(defun auto-closing-electric-bar(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and (auto-closing-electric-is-last-command-char-expandable-punct-p)
       (auto-closing-electric-code-at-point-p)
       (and (save-excursion (re-search-backward auto-closing-electric-expandable-bar nil t))
            (= (point) (match-end 0))) ;looking-back is missing on XEmacs
       (save-excursion
         (insert "|"))))


(provide 'auto-closing-electric)
