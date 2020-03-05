;;; simple-brightscript-mode.el --- Major mode for editing Brightscript files

;; Copyright (c) 2020 Daniel Mircea
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defgroup brightscript nil
  "Major mode for editing BrightScript code."
  :prefix "simple-brightscript-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/viseztrance/simple-brightscript-mode")
  :link '(emacs-commentary-link :tag "Commentary" "simple-brightscript-mode"))

(defvar simple-brightscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst simple-brightscript-syntax-propertize-function ())

(defvar simple-brightscript-font-lock
  (let*
      ((reserved-keywords '("and" "dim" "each" "else" "end" "end for" "end function" "endif" "end sub" "endwhile" "end if" "end while" "exit" "false" "for" "function" "if" "as" "in" "line_num" "m*" "next" "objfun" "or" "pos" "print" "rem" "return" "rnd" "step" "stop" "sub" "tab" "then" "to" "true" "type" "while" "goto"))
       (primitives '("void" "dynamic" "boolean" "integer" "longinteger" "float" "double" "string" "object" "interface"))
       (object-names '("roDeviceInfo" "roMessagePort"))
       (global-functions '("CreateObject" "Sleep" "asc" "chr" "len" "str" "strI" "val" "abs" "atn" "cdbl" "cint" "cos" "exp" "fix" "int" "log" "sgn" "sgnI" "sin" "sqr" "tan" "Left" "Right" "StringI" "String" "Mid" "instr" "GetInterface" "Wait" "ReadAsciiFile" "WriteAsciiFile" "ListDir" "MatchFiles" "LCase" "UCase" "DeleteFile" "DeleteDirectory" "CreateDirectory" "RebootSystem" "ShutdownSystem" "UpTime" "csng" "FormatDrive" "EjectDrive" "CopyFile" "MoveFile" "strtoi" "rnd" "RunGarbageCollector" "GetDefaultDrive" "SetDefaultDrive" "EnableZoneSupport" "EnableAudioMixer" "Pi"))
       (reserved-keywords-regexp (regexp-opt reserved-keywords 'words))
       (primitives-regexp (regexp-opt primitives 'words))
       (object-names-regexp (regexp-opt object-names 'words))
       (global-functions-regexp (regexp-opt global-functions 'words)))

    `(
      ("^[[:space:]]*?\\(?:sub\\|function\\)\\(?:[[:space:]]*\\)?\\([[:word:]]+\\)?\\(?:[^( \t\n]\\)?"
       (1 font-lock-function-name-face)
       ("\\([[:word:]]+\\).*?\\(?:,\\|)\\)"
        (save-excursion
          (goto-char (match-end 0))
          (backward-char)
          (ignore-errors
            (forward-sexp))
          (point))
        (goto-char (match-end 0))
        (1 font-lock-variable-name-face)))
      ("\\.\\([[:word:]]*\\)\(" 1 font-lock-function-name-face)
      ("\\(==\\|invalid\\|\\(?:[[:word:]]*:\\)\\)" 1 font-lock-warning-face)
      ("goto\\(?:[[:space:]]*\\)?\\([[:word:]]*\\)" 1 font-lock-warning-face)
      ("\\_<\\(m\\)\\." 1 font-lock-variable-name-face)
      (,primitives-regexp . font-lock-type-face)
      (,object-names-regexp . font-lock-type-face)
      (,reserved-keywords-regexp . font-lock-builtin-face)
      (,global-functions-regexp . font-lock-constant-face))))

(define-derived-mode simple-brightscript-mode prog-mode "BrightScript"
  "Major mode for editing BrightScript code."
  (setq-local font-lock-defaults
              '(simple-brightscript-font-lock))

  (setq-local comment-start "'")
  (setq-local comment-end "")
  (set-syntax-table simple-brightscript-mode-syntax-table)
  (setq-local syntax-propertize-function simple-brightscript-syntax-propertize-function)
  )

(defun case-insensitive-advice ()
  (setq-local font-lock-keywords-case-fold-search t))
(advice-add 'simple-brightscript-mode :after #'case-insensitive-advice)

(provide 'simple-brightscript-mode)
