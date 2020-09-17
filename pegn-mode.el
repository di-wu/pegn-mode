;;; pegn-mode.el --- Major mode for editing PEGN files
;;
;; Copyright (C) 2020 Quint Daenen
;;
;; Author: Quint Daenen <http://github/quint>
;; Maintainer: Quint Daenen <me@di-wu.be>
;; Keywords: PEGN major-mode
;;
;;; Commentary:
;;
;; TODO

;;; Code:

; Basic Setup
(defvar pegn-mode-hook nil)                     ; allows the user to run thur own code.
(defvar pegn-mode-map                           ; allows the user to define their own keymaps.
  (let ((map (make-keymap)))                    ; consider make-sparse-keymap instead of make-keymap if you don't have a lot of keybindings
    (define-key map "\C-j" 'newline-and-indent) ; NOTE: this is an example (default keybinding for newline-and-indent)
    map)
  "Keymap for PEGN major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pegn\\'" . pegn-mode))

; Syntax Highlighting
(defconst pegn-font-lock-keywords-1
  (list ; use regexp-opt to generate the regex below.
        ; 1. M-x eval-expression
        ; 2. (regexp-opt '("LIST" "OF" "WORDS") t)
        ; 3. wrapped in < and > to indicate only surrounded by space or beginning/end-of-file.
   '("\\<\\(SP\\)\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for PEGN mode.")

(defvar pegn-font-lock-keywords pegn-font-lock-keywords-1
  "Default highlighting in PEGN mode.")

(defun pegn-indent-line ()
  "Indent current line as PEGN code.")

(defvar pegn-mode-syntax-table
  (let ((pegn-mode-syntax-table (make-syntax-table)))
    pegn-mode-syntax-table)
   "Syntax table dor pegn-mode.")

(defun pegn-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map pegn-mode-map)
  (set-syntax-table pegn-mode-syntax-table)
  ; font-lock
  (set (make-local-variable 'font-lock-defaults) '(pegn-font-lock-keywords))
  ; indent
  (set (make-local-variable 'indent-line-function) 'pegn-indent-line)
  (setq major-mode 'pegn-mode)
  (setq mode-name "PEGN")
  (run-hooks 'pegn-mode-hook))

(provide 'pegn-mode)
;;; pegn-mode ends here
