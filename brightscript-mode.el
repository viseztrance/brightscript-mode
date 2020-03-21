;;; brightscript-mode.el --- Major mode for editing Brightscript files -*- lexical-binding: t; -*-

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

;; Adds support for the BrightScript programming language from BrightSign and
;; Roku devices.  This language is used by digital signage and media players.

;;; Code:

(require 'brightscript-syntax-highlight)
(require 'brightscript-indent)

(defgroup brightscript nil
  "Major mode for editing BrightScript code."
  :prefix "brightscript-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/viseztrance/brightscript-mode")
  :link '(emacs-commentary-link :tag "Commentary" "brightscript-mode"))

(defvar brightscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

;;;###autoload
(define-derived-mode brightscript-mode prog-mode "BrightScript"
  "Major mode for editing BrightScript code."
  (setq-local indent-line-function 'brightscript-indent-line-function)
  (setq-local font-lock-defaults '(brightscript-syntax-highlight-font-lock nil t))

  (setq-local comment-start "'")
  (setq-local comment-end "")
  (set-syntax-table brightscript-mode-syntax-table))

(provide 'brightscript-mode)

;;; brightscript-mode.el ends here
