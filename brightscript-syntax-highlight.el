;;; brightscript-syntax-highlight.el --- Brightscript syntax highlight -*- lexical-binding: t; -*-

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

;; Highlights function names and their arguments, language keywords and
;; objects

;;; Code:

(defconst brightscript-syntax-highlight-reserved-keywords
  '("and" "as" "dim" "each" "else" "elseif" "end" "end for" "end function" "endif"
    "end sub" "endwhile" "end if" "end while" "exit" "false" "for" "function" "goto" "if"
    "in" "library" "line_num" "m*" "next" "objfun" "or" "pos" "print" "rem" "return" "rnd"
    "step" "stop" "sub" "tab" "then" "to" "true" "type" "while")
  "BrightScript keywords.")

(defconst brightscript-syntax-highlight-primitives
  '("void" "dynamic" "boolean" "integer" "longinteger" "float" "double" "string"
    "object" "interface")
  "BrightScript variable types.")

(defconst brightscript-syntax-highlight-global-functions
  '("CopyFile" "CreateObject" "CreateDirectory" "DeleteFile" "DeleteDirectory"
    "EjectDrive" "EnableZoneSupport" "EnableAudioMixer" "FormatDrive" "GetInterface"
    "GetDefaultDrive" "LCase" "Left" "ListDir" "MatchFiles" "Mid" "MoveFile" "Pi"
    "ReadAsciiFile" "RebootSystem" "Right" "RunGarbageCollector" "SetDefaultDrive"
    "ShutdownSystem" "Sleep" "StringI" "String" "UCase" "UpTime" "Wait" "WriteAsciiFile"
    "abs" "asc" "atn" "cdbl" "chr" "cint" "cos" "csng" "exp" "fix" "instr" "int"
    "len" "log" "rnd" "sgn" "sgnI" "sin" "sqr" "str" "strI" "strtoi" "tan" "val")
  "BrightScript global functions.")

(defconst brightscript-syntax-highlight-objects
  '("roArray" "roAssociativeArray" "roBoolean" "roByteArray" "roDouble"
    "roIntrinsicDouble" "roFunction" "roGlobal" "roInt" "roFloat" "roString"
    "roList" "roRegex" "roXMLElement" "roXMLList" "roAudioEventMx" "roAudioOutput"
    "roAudioPlayer" "roAudioPlayerMx" "roCanvasWidget" "roClockWidget" "roHtmlWidget"
    "roImageBuffer" "roImagePlayer" "roImageWidget" "roRectangle" "roShoutcastStream"
    "roShoutcastStreamEvent" "roIRRemotePress" "roTextField" "roTextWidget"
    "roVideoEvent" "roAudioEvent" "roVideoInput" "roVideoMode" "roVideoPlayer"
    "roSqliteStatement" "roTouchCalibrationEvent" "roTouchEvent" "roTouchScreen"
    "roBrightPackage" "roHashGenerator" "roReadFile" "roCreateFile" "roReadWriteFile"
    "roAppendFile" "roRegistry" "roRegistrySection" "roSqliteDatabase" "roSqliteEvent"
    "roStorageAttached" "roStorageDetached" "roStorageHotplug" "roStorageInfo"
    "roAssetCollection" "roAssetFetcher" "roAssetFetcherEvent"
    "roAssetFetcherProgressEvent" "roAssetPool" "roAssetPoolFiles" "roAssetRealizer"
    "roAssetRealizerEvent" "roDatagramSender" "roDatagramReceiver" "roDatagramSocket"
    "roDatagramEvent" "roHttpEvent" "roHttpServer" "roMimeStream" "roMimeStreamEvent"
    "roNetworkAdvertisement" "roNetworkAttached" "roNetworkDetached"
    "roNetworkConfiguration" "roNetworkHotplug" "roRssParser" "roRssArticle"
    "roRtspStream" "roRtspStreamEvent" "roShoutcastStream" "roShoutcastStreamEvent"
    "roSnmpAgent" "roSnmpEvent" "roStreamByteEvent" "roStreamConnectResultEvent"
    "roStreamEndEvent" "roStreamLineEvent" "roSyncSpec" "roTCPConnectEvent"
    "roTCPServer" "roTCPStream" "roUrlEvent" "roUrlStream" "roUrlTransfer"
    "roCecInterface" "roCecRxFrameEvent" "roCecTxCompleteEvent" "roChannelManager"
    "roControlPort" "roControlUp" "roControlDown" "roGpioControlPort" "roGpioButton"
    "roIRRemote" "roKeyboard" "roKeyboardPress" "roMessagePort" "roSequenceMatcher"
    "roSequenceMatchEvent" "roSerialPort" "roDeviceInfo" "roResourceManager"
    "roSystemLog" "roDateTime" "roSystemTime" "roTimer" "roTimerEvent" "roTimeSpan"
    "roSyncPool" "roSyncPoolEvent" "roSyncPoolFiles" "roSyncPoolProgressEvent")
  "BrightScript objects that can be constructed by the `CreateObject' function.")

(defface brightscript-syntax-highlight-objects-face
  '((t :weight bold))
  "Face for highlight substrings in a text"
  :group 'brightscript-mode)

(defvar brightscript-syntax-highlight-font-lock
  (let
      ((reserved-keywords-regexp
        (regexp-opt brightscript-syntax-highlight-reserved-keywords 'symbols))
       (primitives-regexp
        (regexp-opt brightscript-syntax-highlight-primitives 'symbols))
       (global-functions-regexp
        (regexp-opt brightscript-syntax-highlight-global-functions 'symbols))
       (objects-regexp
        (regexp-opt brightscript-syntax-highlight-objects 'words)))
    `(
      ; Matches function or sub definition
      ("^[ \t]*\\(?:sub\\|function\\)\\(?:[ \t]*\\)?\\([a-z0-9_]+\\)[^( \t\n]*"
       (1 font-lock-function-name-face)
       ;; Matches function arguments
       ("\\([a-z0-9_\$%!#]+\\).*?\\(?:,\\|)\\)"
        (save-excursion
          (goto-char (match-end 0))
          (backward-char)
          (ignore-errors
            (forward-sexp))
          (point))
        (goto-char (match-end 0))
        (1 font-lock-variable-name-face)))
      ;; Matches object function calls
      ("\\.\\([a-z0-9_]*\\)\(" 1 font-lock-function-name-face)
      ;; Matches the `invalid' keyword and `==' which is not legal syntax
      ("\\(==\\|invalid\\)" 1 font-lock-warning-face)
      ;; Matches `goto' calls
      ("goto[ \t]*\\([a-z0-9_]*\\)" 1 font-lock-warning-face)
      ;; Matches the global `m' var
      ("\\_<\\(m\\)\\." 1 font-lock-variable-name-face)
      ;; Matches comments in the `REM' format
      ("^REM.*$" 0 font-lock-comment-face prepend)
      (,objects-regexp 0 'brightscript-syntax-highlight-objects-face append)
      (,primitives-regexp . font-lock-type-face)
      (,reserved-keywords-regexp . font-lock-builtin-face)
      (,global-functions-regexp . font-lock-constant-face))))

(provide 'brightscript-syntax-highlight)

;;; brightscript-syntax-highlight.el ends here
