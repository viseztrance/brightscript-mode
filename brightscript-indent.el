;;; brightscript-indent.el --- Brightscript indentation -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Daniel Mircea, Free Software Foundation, Inc.

;; Author: Daniel Mircea <daniel@viseztrance.com>
;; URL: https://github.com/viseztrance/brightscript-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: daniel@viseztrance.com
;; Created: Mar 2020
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides basic indentation support for the Brightscript language

;;; Code:

(defcustom brightscript-indent-offset 2
  "Specifies the indentation offset for `brightscript-indent-line'."
  :type 'integer
  :group 'brightscript)

(defun brightscript-indent-perform (indentation)
  "Set the line indent to the provided INDENTATION."
  (let ((adjusted-indentation (max indentation 0)))
    (indent-line-to adjusted-indentation)))

(defun brightscript-indent-start-of-block-p ()
  "Check if current line is the start of a block such as function or loop construct."
  (or
   (looking-at-p "^[ \t]*\\_<\\(sub\\|function\\|if\\|else\\|elseif\\|while\\|for\\)\\_>")
   (looking-at-p ".*?\{[ \t]*$")))

(defun brightscript-indent-end-of-block-p ()
  "Check if current line is the end of a block such as function or loop construct."
  (or
   (looking-at-p "^[ \t]*\\_<\\(end\\|endif\\|endfor\\|endwhile\\|else\\|elseif\\)\\_>")
   (looking-at-p "^[ \t]*\}\,?")))

(defun brightscript-indent-go-to-previous-non-blank-line ()
  "Jumps the previous non blank line."
  (let ((found))
    (while (not (or found (bobp)))
      (forward-line -1)
      (unless (looking-at "^[ \t]*$")
        (setq found t)))))

(defun brightscript-indent-line-function ()
  "Indent current line as Brightscript code."
  (interactive)
  (beginning-of-line)
  (let ((new-indentation 0)
        (main-line-ends-block (brightscript-indent-end-of-block-p))
        (offset 0))
    (unless (bobp)
      (save-excursion
        (brightscript-indent-go-to-previous-non-blank-line)
        (if (brightscript-indent-start-of-block-p)
            (setq offset brightscript-indent-offset))
        (if main-line-ends-block
            (setq offset (- brightscript-indent-offset)))
        (setq new-indentation (+ (current-indentation) offset))))
    (brightscript-indent-perform new-indentation)))

(provide 'brightscript-indent)

;;; brightscript-indent.el ends here
