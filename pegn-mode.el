;;; pegn-mode.el --- Major mode for editing PEGN files
;;
;; Copyright (C) 2020 Quint Daenen
;;
;; Author: Quint Daenen <http://github/di-wu>
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
   ; These define all the reserved PEGN token constants:
   ; "TAB" "CRLF" "CR" "LF" "SP" "VT" "FF" "NOT" "BANG" "DQ" "HASH" "DOLLAR" "PERCENT"
   ; "AND" "SQ" "LPAREN" "RPAREN" "STAR" "PLUS" "COMMA" "DASH" "MINUS" "DOT" "SLASH"
   ; "COLON" "SEMI" "LT" "EQ" "GT" "QUERY" "QUESTION" "AT" "LBRAKT" "BKSLASH" "RBRAKT"
   ; "CARET" "UNDER" "BKTICK" "LCURLY" "LBRACE" "BAR" "PIPE" "RCURLY" "RBRACE" "TILDE"
   ; "UNKNOWN" "REPLACE" "MAXRUNE" "MAXASCII" "MAXLATIN" "LARROW" "RARROW" "LLARROW"
   ; "RLARROW" "LARROWF" "LFAT" "RARROWF" "RFAT" "WALRUS"
   '("\\<\\(A\\(?:ND\\|T\\)\\|B\\(?:A\\(?:NG\\|R\\)\\|K\\(?:SLASH\\|TICK\\)\\)\\|C\\(?:ARET\\|O\\(?:LON\\|MMA\\)\\|R\\(?:LF\\)?\\)\\|D\\(?:ASH\\|O\\(?:LLAR\\|T\\)\\|Q\\)\\|EQ\\|FF\\|GT\\|HASH\\|L\\(?:ARROWF?\\|BRA\\(?:CE\\|KT\\)\\|CURLY\\|FAT\\|LARROW\\|PAREN\\|[FT]\\)\\|M\\(?:AX\\(?:ASCII\\|LATIN\\|RUNE\\)\\|INUS\\)\\|NOT\\|P\\(?:ERCENT\\|IPE\\|LUS\\)\\|QUE\\(?:RY\\|STION\\)\\|R\\(?:ARROWF?\\|BRA\\(?:CE\\|KT\\)\\|CURLY\\|EPLACE\\|FAT\\|LARROW\\|PAREN\\)\\|S\\(?:EMI\\|LASH\\|TAR\\|[PQ]\\)\\|T\\(?:AB\\|ILDE\\)\\|UN\\(?:DER\\|KNOWN\\)\\|VT\\|WALRUS\\)\\>" . font-lock-constant-face)
   ; These define all the reserved PEGN check and class identifiers:
   ; "alphanum" "alpha" "any" "bitdig" "control" "digit" "hexdig" "lowerhex" "lower"
   ; "octdig" "punct" "quotable" "sign" "upperhex" "upper" "visible" "ws" "alnum" "ascii"
   ; "blank" "cntrl" "graph" "print" "space" "word" "xdigit"
   ; "EndLine"
   '("\\<\\(EndLine\\|a\\(?:l\\(?:num\\|pha\\(?:num\\)?\\)\\|ny\\|scii\\)\\|b\\(?:itdig\\|lank\\)\\|c\\(?:\\(?:ntr\\|ontro\\)l\\)\\|digit\\|graph\\|hexdig\\|lower\\(?:hex\\)?\\|octdig\\|p\\(?:\\(?:rin\\|unc\\)t\\)\\|quotable\\|s\\(?:ign\\|pace\\)\\|upper\\(?:hex\\)?\\|visible\\|w\\(?:ord\\|s\\)\\|xdigit\\)\\>" . font-lock-type-face))
  "Minimal highlighting expressions for PEGN mode.")

(defvar pegn-font-lock-keywords pegn-font-lock-keywords-1
  "Default highlighting in PEGN mode.")

(defun pegn-indent-line ()
  ; TODO
  "Indent current line as PEGN code.")

(defvar pegn-mode-syntax-table nil "Syntax table for pegn-mode.")
(when (not pegn-mode-syntax-table)
  (setq pegn-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\"" pegn-mode-syntax-table)
  ;; Comment delimiters.
  (modify-syntax-entry ?\# "<" pegn-mode-syntax-table)
  (modify-syntax-entry ?\n ">" pegn-mode-syntax-table)
  )

(defun pegn-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map pegn-mode-map)
  (set-syntax-table pegn-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(pegn-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'pegn-indent-line)
  (setq major-mode 'pegn-mode)
  (setq mode-name "PEGN")
  (run-hooks 'pegn-mode-hook))

(provide 'pegn-mode)
;;; pegn-mode ends here
