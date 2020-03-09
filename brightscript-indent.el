;;; brightscript-ident.el --- Brightscript indentation -*- lexical-binding: t; -*-

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
   (looking-at-p "^[ \t]*\\_<\\(sub\\|function\\|if\\|else\\|while\\|for\\)\\_>")
   (looking-at-p ".*?\{[ \t]*$")))

(defun brightscript-indent-end-of-block-p ()
  "Check if current line is the end of a block such as function or loop construct."
  (or
   (looking-at-p "^[ \t]*\\_<\\(end\\|endif\\|endfor\\|endwhile\\|else\\)\\_>")
   (looking-at-p "^[ \t]*\}\,?")))

(defun brightscript-indent-start-and-end-block-p ()
  "Check if current line is an else statement."
  (looking-at "^[ \t]*\\_<\\(else\\)\\_>"))

(defun brightscript-indent-line-function ()
  "Indent current line as Brightscript code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) (new-indentation 0))
      (if (brightscript-indent-end-of-block-p)
          (save-excursion
            (forward-line -1)
            (setq new-indentation (- (current-indentation) brightscript-indent-offset)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (brightscript-indent-end-of-block-p)
                (progn
                  (if (brightscript-indent-start-and-end-block-p)
                      (setq new-indentation (+ (current-indentation) brightscript-indent-offset))
                    (setq new-indentation (current-indentation)))
                  (setq not-indented nil))
              (if (brightscript-indent-start-of-block-p)
                  (progn
                    (setq new-indentation (+ (current-indentation) brightscript-indent-offset))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil))))))
        (brightscript-indent-perform new-indentation)))))

(provide 'brightscript-indent)

;;; brightscript-indent.el ends here
