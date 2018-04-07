;;; sme-mode.el --- major mode for editing SME files. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2018, Truls Asheim

;; Author: Truls Asheim ( truls@asheim.dk )
;; Version: 1.0.0
;; Created: 13 Jan 2018
;; Keywords: languages
;; Homepage: https://github.com/truls/language-smeil/
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License version 3.

;;; Commentary:

;; Simple major mode for editing SME hardware designs
;;
;; Files with a ".sme" extension are handled automatically by this mode.
;;
;; Indentation code based on/inspired by futhark-mode

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sme\\'" . sme-mode))

(defvar sme-mode-hook nil
  "Hook for `sme-mode` that is run whenever the mode is entered.")

;;; Syntax highlighting

(defconst sme-keywords
  '("as"
    "barrier"
    "break"
    "bus"
    "case"
    "default"
    "elif"
    "else"
    "enum"
    "for"
    "from"
    "func"
    "generate"
    "if"
    "import"
    "instance"
    "network"
    "of"
    "proc"
    "range"
    "return"
    "simulation"
    "switch"
    "to"
    "var"
    "where"
    "const")
  "SME keywords.")

(defconst sme-types
  (let* ((ws-end "\\(?:[[:space:]]+\\|;\\|$\\)")
         (ws-pre "[[:space:]|(]")
         (param-pre "\\(?:\(\\|,\\)[[:space:]]*")

         (ws-opt "\\(?:[[:space:]]*\\|;\\)")
         (array-prefix (concat "\\(?:\\[[[:alnum:][:blank:]+-/*%<>&^|.]*\\]"
                               ws-opt
                               "\\)*")))
    (concat "\\(?:"
            array-prefix ws-opt "\\(?1:bool\\)" ws-end
            "\\|"
            ws-pre "\\(?1:" array-prefix ws-opt "in\\)" ws-end
            "\\|"
            ws-pre "\\(?1:" array-prefix ws-opt "out\\)" ws-end
            "\\|"
            param-pre "\\(?1:const\\)" ws-end
            "\\|\\(?1:"
            array-prefix ws-opt "int\\)" ws-end
            "\\|\\(?1:"
            array-prefix ws-opt "uint\\)" ws-end
            "\\|\\(?1:"
            array-prefix ws-opt "f32\\)" ws-end
            "\\|\\(?1:"
            array-prefix ws-opt "f64\\)" ws-end
            "\\|\\(?1:"
            array-prefix ws-opt "i[[:digit:]]+\\)" ws-end
            "\\|\\(?1:"
            array-prefix ws-opt "u[[:digit:]]+\\)" ws-end
            "\\)" )
    )
  "Regexp for matching SME types."
  )

(defconst sme-properties
  `("sync"
    "async"
    "exposed"
    "unique"
    "len")
  "SME definition modifiers."
  )

(defconst sme-constants
  `("true" "false")
  "Constants of SME (boolean values)."
  )

;; TODO: Highlight declarations properly
(defvar sme-font-lock-keywords)
(setq sme-font-lock-keywords
      (let* (
            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt sme-keywords 'words))
            (x-constants-regexp (regexp-opt sme-constants 'words))
            )

        `(
          (,sme-types . (1 font-lock-type-face))
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-constants-regexp . font-lock-constant-face)
          ;;(,x-events-regexp . font-lock-builtin-face)
          (,(regexp-opt sme-properties 'words) . font-lock-builtin-face)
          ;(,(regexp-opt sme-properties 'words) . font-lock-function-name-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(defvar sme-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define // comments.
    (modify-syntax-entry ?\/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; Variable names can include _
    (modify-syntax-entry ?\\ "_" st)
    st)
  "Syntax table used in `sme-mode'.")

;;; Indentation:

(defvar sme-indent-level 4
  "Indentation level for sme-mode.")

(defun sme-beginning-of-line-text ()
  "Move to the beginning of the non-whitespace text on this line."
  (beginning-of-line)
  (sme-goto-first-text))

(defun sme-end-of-line-text ()
  "Move to the end of the non-whitespace text on this line."
  (end-of-line)
  (sme-goto-last-text))

(defun sme-goto-first-text ()
  "Skip over whitespace."
  (while (looking-at "[[:space:]\n]")
    (forward-char)))

(defun sme-goto-last-text ()
  "Skip over whitespace."
  (while (looking-at "[[:space:]\n]")
    (backward-char)))

(defun sme-looking-at-word (word)
  "Do the same as `looking-at', but also check for blanks around WORD."
  (looking-at (concat "\\<" word "\\>")))

(defun sme-looking-at-top-level-def ()
  "Looking at a top level definition keyword."
        (or (sme-looking-at-word "sync")
            (sme-looking-at-word "async")
            (sme-looking-at-word "proc")
            (sme-looking-at-word "network")
            (sme-looking-at-word "import")
            (sme-looking-at-word "from")))

(defun sme-looking-at-block-start ()
  "Looking at a keyword starting a block."
        (or (sme-looking-at-word "if")
            (sme-looking-at-word "for")
            (sme-looking-at-word "generate")
            (sme-looking-at-word "bus")
            (sme-looking-at-word "enum")
            (sme-looking-at-word "func")
            (sme-looking-at-word "else")
            (sme-looking-at-word "elif")))

(defun sme-ends-in-semi ()
  "Check if line ends in a semicolon."
  (sme-goto-last-text)
  (looking-at ";"))

(defun sme-go-back-to-block-start ()
  "Move back to the start of a block statement."
  (forward-line -1)
  (sme-beginning-of-line-text)
  (let ((ret (sme-looking-at-block-start)))
    (unless ret
      (while (and (not
                   (save-excursion (sme-end-of-line-text)
                                   (sme-looking-at-line-end)
                                   ))
                  (not ret))
        (forward-line -1)
        (sme-beginning-of-line-text)
        (setq ret (sme-looking-at-block-start))))
    ret))

(defun sme-backward-part ()
  "Try to jump back one sexp.
The net effect seems to be that it works ok."
  (and (not (bobp))
       (ignore-errors (backward-sexp 1) t)))

(defun sme-back-actual-line ()
  "Go back to the first non-empty line, or return nil trying."
  (let (bound)
    (while (and (not (bobp))
                (forward-line -1)
                (progn (beginning-of-line)
                       (setq bound (point))
                       (end-of-line)
                       t)
                (ignore-errors
                  (re-search-backward "^[[:space:]]*$" bound))))))

(defun sme-is-beginning-of-line-text ()
  "Check if point is at the first word on a line."
  (=
   (point)
   (save-excursion
     (sme-beginning-of-line-text)
     (point))))

(defun sme-is-empty-line ()
  "Check if the line of the current point is empty.
It is considered empty if the line consists of zero or more
whitespace characters."
  (let ((cur (line-number-at-pos)))
    (sme-beginning-of-line-text)
    (not (= cur (line-number-at-pos)))))

(defun sme-looking-at-line-end ()
  "Looking at a char which may end a line."
  (or (looking-at ";")
      (looking-at "{")
      (looking-at "}")))

(defun sme-calculate-indentation ()
  "Calculate the indentation for current line."
  (let ((parse-sexp-ignore-comments t)
        (parse-sexp-lookup-properties t))

    (save-excursion
      (or
       ;; Align empty lines to beginning of previous lines.
       (save-excursion
         (ignore-errors
           (when (sme-is-empty-line)
             (sme-back-actual-line)
             (sme-goto-last-text)
             (if (or (looking-at "{")
                     (and (not (looking-at ";"))
                          (not (looking-at "}"))))
                 (progn (sme-beginning-of-line-text)
                        (+ sme-indent-level (current-column)))
               (sme-beginning-of-line-text)
               (current-column)))))

       ;; Everything from here on looks at start of line.
       (sme-beginning-of-line-text)

       ;; Top-level definitions gets aligned to column 0.
       (and (sme-looking-at-top-level-def) 0)

       ;; Align closing parentheses and commas to the matching opening
       ;; parenthesis.
       (save-excursion
         (and (looking-at (regexp-opt '(")" "]" ",")))
              (ignore-errors
                (backward-up-list 1)
                (current-column))))

       ;; Align lonely opening braces to their block-definition statements.
       (save-excursion
         (ignore-errors
           (when (looking-at "{")
             (sme-go-back-to-block-start)
             (if (sme-looking-at-block-start)
                 (current-column)
               ;; Assume that { belongs to a top-level def here.
               0)
             )))

       ;; Align closing braces to opening braces
       (save-excursion
         (when (looking-at "}")
           (or
            (ignore-errors
              (backward-up-list 1)
              (sme-beginning-of-line-text)
              (current-column))
            (ignore-errors
              (backward-up-list 1)
              (looking-at "{")
              (current-column)))))

       ;; Indent continued lines.
       (save-excursion
         (sme-back-actual-line)
         (unless
             (save-excursion (sme-beginning-of-line-text)
                             (or
                              (sme-looking-at-top-level-def)
                              (looking-at comment-start)
                              ))
           (sme-goto-last-text)
           (when (not (sme-looking-at-line-end))
             (sme-beginning-of-line-text)
             (if (save-excursion
                   (sme-back-actual-line)
                   (or (progn (sme-goto-last-text)
                              (sme-looking-at-line-end))
                       (progn (sme-beginning-of-line-text)
                              (looking-at comment-start))))
                 (+ sme-indent-level (current-column))
               (current-column)))))

       ;; Add one indentation level to lines following an opening brace
       (save-excursion
         (ignore-errors
           (backward-up-list 1)
           (when (looking-at "{")
             (sme-beginning-of-line-text)
             (+ sme-indent-level (current-column)))))

       ;; We are probably inside the declaration section of a process. Indent
       ;; first line following a top-level definition and align the rest to that
       ;; line.
       (sme-beginning-of-line-text)
       (when (looking-at comment-start)
         (sme-back-actual-line)
         (sme-beginning-of-line-text)
         (if (sme-looking-at-top-level-def)
             sme-indent-level
           (current-column)))

       ;; Default to indenting. This doesn't always give the desired results,
       ;; but it is a simple way of getting correct indentation in the
       ;; declaration parts of processes.
       sme-indent-level

       ))))

(defun sme-indent-line ()
  "Indent current line as SME code."
  (let ((savep (> (current-column) (current-indentation)))
        (indent  (or (sme-calculate-indentation)
                     (current-indentation))))
    (if savep ; The cursor is beyond leading whitespace.
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

;;;###autoload
(define-derived-mode sme-mode prog-mode "SME"
  "Major mode for editing SME (Synchronous Message Exchange) files"
  :syntax-table sme-mode-syntax-table
  :group 'sme-mode
  ;; code for syntax highlighting
  (setq font-lock-defaults '((sme-font-lock-keywords)))
  (setq comment-start "//")
  (setq comment-end "")
  (setq indent-line-function 'sme-indent-line)
  )

(provide 'sme-mode)

;;; sme-mode.el ends here
