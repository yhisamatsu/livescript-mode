;;; livescript-mode.el --- Major mode for editing LiveScript files

;; Copyright (C) 2012 Hisamatsu Yasuyuki

;; Author: Hisamatsu Yasuyuki <yas@null.net>
;; URL: https://github.com/YHisamatsu/livescript-mode
;; Keywords: languages livescript
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing LiveScript code.

;;; Code:

(eval-when-compile (require 'cl))

;;
;; Syntax table
;;

(defvar livescript-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Symbol constituents (default)
    (modify-syntax-entry '(0 . 127) "_   " st)
    ;; Words
    (dolist (range '((?0 . ?9) (?A . ?Z) (?a . ?z)))
      (modify-syntax-entry range "@   " st))
    ;; Whitespace
    (dolist (ch '(?\t ?\f ?\r ?\s))
      (modify-syntax-entry ch "-   " st))
    ;; Newline as comment ender
    (modify-syntax-entry ?\n ">   " st)
    ;; Parenthesis
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{  "(}  " st)
    (modify-syntax-entry ?}  "){  " st)
    ;; Comment
    (modify-syntax-entry ?# "<   " st)
    (modify-syntax-entry ?* "_ 23" st)
    ;; Regexp literal (and /* */ style comment)
    (modify-syntax-entry ?/ "$ 14" st)
    ;; Punctuation
    (dolist (ch '(?, ?: ?! ??))
      (modify-syntax-entry ch ".   " st))
    ;; String and paired quote
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?'  "\"   " st)
    ;; etc
    (modify-syntax-entry ?`  "$   " st)
    (modify-syntax-entry ?@  "'   " st)
    (modify-syntax-entry ?\\ "\\   " st)
    st)
  "Syntax table in use in `livescript-mode' buffers.")

(defvar livescript-mode-abbrev-table nil)
(define-abbrev-table 'livescript-mode-abbrev-table ())

;;
;; Font lock
;;

(defvar livescript-keywords-regexp
  (labels ((qw (seq) (mapcar #'symbol-name seq)))
    (let ((js-keywords (qw [break catch class continue delete else extends
                            finally for if in instanceof loop new return
                            super switch throw try typeof until while]))
          (cs-keywords (qw [and by is isnt not of or own then unless when]))
          (ls-keywords (qw [fallthrough it otherwise that til to]))
          (js/cs-reserved (qw [case const debugger default do enum export
                               function import let native var void with
                               __extends __hasProp])))
      (concat "\\_<"
       (regexp-opt
        (append js-keywords cs-keywords ls-keywords js/cs-reserved) t) "\\_>")))
  "Regular expression to match the keywords.")

(defvar livescript-boolean-regexp
  (concat "\\_<"
   (regexp-opt
    (mapcar #'symbol-name
            [true false yes no on off null undefined]) t) "\\>")
  "Regular expression to match booleans.")

(defvar livescript-regexp-regexp
  "\\(/\\(?:[^/\n]\\|\\[\\(?:\\\\.\\|.\\)+?\\]\\|\\\\.\\)+?/[gi]*\\)"
  "Regular expression to match regular expressions.")

(defvar livescript-string-regexp "\\(\\\\[^]}\),;[:space:]\n]+\\|<\\[.*\\]>\\)"
  "Regular expression to match strings.")

(defvar livescript-property-regexp "\\(\\w+\\)\\s-*:"
  "Regular expression to match property names.")

(defvar livescript-instance-regexp "\\(@\\w+\\)"
  "Regular expression to match instance variables.")

(defvar livescript-negate-regexp "\\(\\w+\\s-*:=\\)"
  "Regular expression to negate highlighting.")

(defvar livescript-function-name-regexp
  (let* ((ws "\\s-*") ;; optional white spaces
         (var       (concat ws "\\w+" ws))
         (assign    (concat ws "[:=]" ws))
         (args      (concat var "\\(?:," var "\\)*"))
         (arrow     "\\(?2:[-~]\\)\\2?>")
         (anon-func (concat "\\(?:(" args ")\\)?" ws arrow)))
    (concat
     ;; function's name
     "\\(?1:\\w+\\)" ws
     "\\(?:"
        ;; func([args]) =
        (format "!?%s(\\(?:%s\\)?%s)%s" ws args ws assign)
     "\\|"
        ;; func = [(args)] ->
        assign "!?" ws anon-func
     "\\)"))
  "Regular expression to match function names.")

(defvar livescript-class-name-regexp
  "\\_<class\\s-+\\(\\w+\\)"
  "Regular expression to match class names.")

(defconst livescript-font-lock-keywords-1
  `((,livescript-regexp-regexp        1 font-lock-constant-face)
    (,livescript-keywords-regexp      1 font-lock-keyword-face)
    (,livescript-function-name-regexp 1 font-lock-function-name-face)
    (,livescript-class-name-regexp    1 font-lock-type-face)
    (,livescript-negate-regexp        1 font-lock-negation-char-face)
    (,livescript-instance-regexp      1 font-lock-variable-name-face)
    (,livescript-property-regexp      1 font-lock-type-face)))

(defconst livescript-font-lock-keywords-2
  (append livescript-font-lock-keywords-1
   `((,livescript-boolean-regexp 1 font-lock-constant-face)
     (,livescript-string-regexp  1 font-lock-string-face))))

(defvar livescript-font-lock-keywords livescript-font-lock-keywords-1
  "Default font-lock-keywords of LiveScript mode.")

;;
;; Imenu support
;;

(defvar livescript-imenu-generic-expression
  `((nil ,(concat "^\\s-*\\<" livescript-function-name-regexp) 1)
    ;; TODO: more submenus here...
    )
  "Imenu generic expression for LiveScript mode.")

;;
;; Commands
;;

;; TODO: define commands here...

(defvar livescript-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: set keymap here...
    map)
  "Keymap used in LiveScript mode.")

;;
;; Setup
;;

(defun livescript-mode-variables ()
  "Setup buffer-local variable for LiveScript major mode."
  ;; Syntax table
  (set-syntax-table livescript-mode-syntax-table)
  ;; Abbrev
  (setq local-abbrev-table livescript-mode-abbrev-table)
  ;; Comment
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-add) 1)
  ;; Imenu
  (setq imenu-case-fold-search t)
  (setq imenu-syntax-alist '(("-+*/<>_" . "w")))
  (setq imenu-generic-expression livescript-imenu-generic-expression)
  ;; Font-lock
  (set (make-local-variable 'font-lock-defaults)
       '((livescript-font-lock-keywords
          livescript-font-lock-keywords-1 livescript-font-lock-keywords-2)
         nil t (("-+*/<>_" . "w")))))

;; mock `prog-mode' for Emacs 23
(when (version< emacs-version "24")
  (or (fboundp 'prog-mode)
      (define-derived-mode prog-mode fundamental-mode "")))

;;;###autoload
(define-derived-mode livescript-mode prog-mode "LiveScript"
  "Major mode for editing LiveScript code.

Basic syntax highlighting is supported.

Commands:
No command is available yet.
\\{livescript-mode-map}"
  (livescript-mode-variables))

;;
;; Customize variables
;;

(defgroup livescript nil
  "Major mode for editing LiveScript code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom livescript-mode-hook nil
  "Normal hook run when entering `livescript-mode'.
See `run-hooks'."
  :type 'hook
  :group 'livescript)

(defcustom livescript-program-name "livescript"
  "The command to evaluate LiveScript code."
  :type 'string
  :group 'livescript)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ls\\'" . livescript-mode))

(provide 'livescript-mode)

;;; livescript-mode.el ends here
