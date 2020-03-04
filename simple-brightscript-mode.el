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

(require 'generic-x)

(defconst simple-brightscript-comments '("'" "REM"))
(defconst simple-brightscript-reserved-words
  '("and" "createobject" "dim" "each" "else" "end" "endfor" "endfunction" "endif" "endsub" "endwhile" "exit" "false" "for" "function" "goto" "if" "invalid" "line_num" "m*" "next" "objfun" "or" "pos" "print" "rem" "return" "rnd" "step" "stop" "sub" "tab" "then" "to" "true" "type" "while"))

(define-generic-mode 'simple-brightscript-mode
  simple-brightscript-comments
  simple-brightscript-reserved-words
  '(("=" . 'font-lock-operator)
    ((";") . 'font-lock-builtin))
  '("\\.brs$")
  nil
  "A mode for BrightSign brightscript files")

(defun case-insensitive-advice ()
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t))
(advice-add 'simple-brightscript-mode :after #'case-insensitive-advice)

(provide 'simple-brightscript-mode)
