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
  (list
    '("\\<\\(?:A\\(?:ND\\|T\\)\\|B\\(?:A\\(?:NG\\|R\\)\\|K\\(?:SLASH\\|TICK\\)\\)\\|C\\(?:ARET\\|O\\(?:LON\\|MMA\\)\\|R\\(?:LF\\)?\\)\\|D\\(?:ASH\\|O\\(?:LLAR\\|T\\)\\|Q\\)\\|E\\(?:NDOFDATA\\|Q\\)\\|FF\\|GT\\|HASH\\|L\\(?:ARROWF?\\|BRA\\(?:CE\\|KT\\)\\|CURLY\\|FAT\\|LARROW\\|PAREN\\|[FT]\\)\\|M\\(?:AX\\(?:ASCII\\|LATIN\\|RUNE\\)\\|INUS\\)\\|NOT\\|P\\(?:ERCENT\\|IPE\\|LUS\\)\\|QUE\\(?:RY\\|STION\\)\\|R\\(?:ARROWF?\\|BRA\\(?:CE\\|KT\\)\\|CURLY\\|EPLACE\\|FAT\\|LARROW\\|PAREN\\)\\|S\\(?:EMI\\|LASH\\|TAR\\|[PQ]\\)\\|T\\(?:AB\\|ILDE\\)\\|UN\\(?:DER\\|KNOWN\\)\\|VT\\|WALRUS\\)\\>" . font-lock-constant-face)
    '("\\<\\(?:a\\(?:l\\(?:num\\|pha\\(?:num\\)?\\)\\|ny\\|scii\\)\\|b\\(?:indig\\|lank\\)\\|c\\(?:\\(?:ntr\\|ontro\\)l\\)\\|digit\\|graph\\|hexdig\\|lower\\(?:hex\\)?\\|octdig\\|p\\(?:\\(?:rin\\|unc\\)t\\)\\|quotable\\|s\\(?:ign\\|pace\\)\\|u\\(?:nipoint\\|p\\(?:hex\\|per\\)\\)\\|visible\\|w\\(?:ord\\|s\\)\\|xdigit\\)\\>" . font-lock-type-face))
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
