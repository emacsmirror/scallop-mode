;;; scallop-mode.el --- Major mode for editing Scallop programming language  -*- lexical-binding: t -*-

;; Copyright © 2025, by Ta Quang Trung

;; Author: Ta Quang Trung
;; Version: 0.0.1
;; Created: 14 May, 2025
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/taquangtrung/emacs-scallop-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs major mode for editing source code of the Scallop programming language
;; (https://www.scallop-lang.org/)

;; Features:
;; - Syntax highlighting.
;; - Automatic code indentation.

;; Installation:
;; - Automatic package installation from Melpa.
;; - Manual installation by putting the `scallop-mode.el' file in Emacs' load path.

;;; Code:

(require 'rx)

(defconst scallop-keywords
  '("and"
    "count"
    "exists"
    "forall"
    "or"
    "query"
    "rel"
    "type"
    "where")
  "List of Scallop keywords.")

(defconst scallop-special-constants
  '("inf"
    "nan"
    "true"
    "false")
  "List of Scallop special constants.")

(defconst scallop-special-operators
  '("~"
    ":"
    "="
    ":-"
    "::"
    ":="
    "<:")
  "List of Scallop special operators.")

(defconst scallop-special-types
  '("bool"
    "char"
    "f32"
    "f64"
    "i8"
    "i16"
    "i32"
    "i64"
    "i128"
    "isize"
    "u8"
    "u16"
    "u32"
    "u64"
    "u128"
    "usize"
    "str"
    "String")
  "List of Scallop special types.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting

(defvar scallop-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; C++ style comment "// ..."
    (modify-syntax-entry ?\/ ". 124" syntax-table)
    (modify-syntax-entry ?* ". 23b" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table for `scallop-mode'.")

(defvar scallop-keywords-regexp
  (concat
   (rx symbol-start)
   (regexp-opt scallop-keywords t)
   (rx symbol-end))
  "Regular expression to match Scallop keywords.")

(defvar scallop-special-constants-regexp
  (concat
   (rx symbol-start)
   (regexp-opt scallop-special-constants t)
   (rx symbol-end))
  "Regular expression to match Scallop special constants.")

(defvar scallop-special-types-regexp
  (concat
   (rx symbol-start)
   (regexp-opt scallop-special-types t)
   (rx symbol-end))
  "Regular expression to match Scallop special types.")

(defun scallop-match-regexp (regexp bound)
  "Generic regular expression matching wrapper for REGEXP until a BOUND position."
  (re-search-forward regexp bound t nil))

(defun scallop-match-relation-names (bound)
  "Search the buffer forward until the BOUND position to match relation names.
The relation names are matched in the 2nd group."
  (scallop-match-regexp
   (concat "\\(rel\\|query\\)[[:space:]]*\\([a-zA-Z_][a-zA-Z0-9_]*\\)" )
   bound))

(defun scallop-match-relation-calls (bound)
  "Search the buffer forward until the BOUND position to match relation calls.
The relation calls are matched in the 1st group."
  (scallop-match-regexp
   (concat (rx symbol-start) "\\([a-zA-Z0-9_]+\\)" (rx symbol-end)
           "[[:space:]]*\\((\\|\\[[^\\[]*\\](\\)")
   bound))

(defun scallop-match-special-operators (bound)
  "Search the buffer forward until the BOUND position to match special operators.
The operators are matched in the 1st group."
  (scallop-match-regexp
   (concat (rx (or alnum space "(" ")"))
           (regexp-opt scallop-special-operators t)
           (rx (or alnum space "(" ")")))
   bound))

(defun scallop-match-type-names (bound)
  "Search the buffer forward until the BOUND position to match type names.
The relation names are matched in the 1st group."
  (scallop-match-regexp
   (concat "type[[:space:]]*\\([a-zA-Z0-9_]+\\)" )
   bound))

(defun scallop-match-super-type-names (bound)
  "Search the buffer forward until the BOUND position to match super type names.
The relation names are matched in the 1st group."
  (scallop-match-regexp
   (concat "<:[[:space:]]*\\([a-zA-Z0-9_]+\\)" )
   bound))

(defconst scallop-font-locks
  (list
   ;; == Constants ==
   `(,scallop-special-constants-regexp . font-lock-constant-face)
   ;; == Keywords ==
   `(,scallop-keywords-regexp . font-lock-keyword-face)
   ;; == Operators ==
   ;; HACK: Temporarily use `font-lock-negation-char-face' for operators.
   ;; Not sure why using `font-lock-operator-face' doesn't highlight the operators.
   `(scallop-match-special-operators (1 font-lock-negation-char-face keep))
   ;; == Types ==
   `(,scallop-special-types-regexp . font-lock-type-face)
   `(scallop-match-type-names (1 font-lock-type-face))
   `(scallop-match-super-type-names (1 font-lock-type-face))
   ;; == Relations ==
   `(scallop-match-relation-names (2 font-lock-function-name-face))
   ;; HACK: Temporarily use `font-lock-function-name-face' for relation calls.
   ;; Not sure why using `font-lock-function-call-face' doesn't highlight the relation calls.
   '(scallop-match-relation-calls (1 font-lock-function-name-face)))
  "Font lock settings for `scallop-mode'.")

;;;;;;;;;;;;;;;;;;
;;; Indentation

(defun scallop-indent-line (&optional indent)
  "Indent the current line according to the Scallop syntax, or supply INDENT."
  (interactive "P")
  (let ((pos (- (point-max) (point)))
        (indent (or indent (scallop--calculate-indentation)))
        (shift-amount nil)
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (null indent)
        (goto-char (- (point-max) pos))
      (setq shift-amount (- indent (current-column)))
      (unless (zerop shift-amount)
        (delete-region beg (point))
        (indent-to indent))
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))))

(defun scallop--calculate-indentation ()
  "Calculate the indentation of the current line."
  (let (indent)
    (save-excursion
      (back-to-indentation)
      (let* ((ppss (syntax-ppss))
             (depth (car ppss))
             (base (* tab-width depth)))
        (setq indent base)
        (cond (;; Closing a block or a parentheses pair
               (looking-at "\s*[})]")
               (setq indent (- base tab-width)))
              (;; Indent multiple-line subtyping definition
               (looking-at "\s*<:")
               (setq indent (+ base (* 2 tab-width))))
              (;; Indent multiple-line definition
               (looking-at "\s*:-")
               (setq indent (+ base (* 2 tab-width))))
              (;; Indent multiple-line definition by looking back
               (looking-back "\s*\\(:-\\|and\\|or\\|,\\)\s*\n\s*" nil nil)
               (setq indent (+ base (* 2 tab-width))))
              (;; Indent multiple-line definition by looking at
               (looking-at "^\s*\\(:-\\|and\\|or\\)\s*")
               (setq indent (+ base (* 2 tab-width)))))))
    indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major mode settings

;;;###autoload
(define-derived-mode scallop-mode prog-mode
  "scallop-mode"
  "Major mode for editing source code of the Scallop programming language."
  :syntax-table scallop-syntax-table

  ;; Syntax highlighting
  (setq font-lock-defaults '(scallop-font-locks))

  ;; Indentation
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'scallop-indent-line)

  ;; Set comment command
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local comment-multi-line nil)
  (setq-local comment-use-syntax t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scl\\'" . scallop-mode))

;; Finally export the `scallop-mode'
(provide 'scallop-mode)

;;; scallop-mode.el ends here
