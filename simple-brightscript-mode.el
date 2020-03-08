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

(defconst simple-brightscript-reserved-keywords
  '("and" "as" "dim" "each" "else" "end" "end for" "end function" "endif" "end sub"
    "endwhile" "end if" "end while" "exit" "false" "for" "function" "goto" "if" "in"
    "line_num" "m*" "next" "objfun" "or" "pos" "print" "rem" "return" "rnd" "step"
    "stop" "sub" "tab" "then" "to" "true" "type" "while"))

(defconst simple-brightscript-primitives
  '("void" "dynamic" "boolean" "integer" "longinteger" "float" "double" "string"
    "object" "interface"))

(defconst simple-brightscript-global-functions
  '("CopyFile" "CreateObject" "CreateDirectory" "DeleteFile" "DeleteDirectory"
    "EjectDrive" "EnableZoneSupport" "EnableAudioMixer" "FormatDrive" "GetInterface"
    "GetDefaultDrive" "LCase" "Left" "ListDir" "MatchFiles" "Mid" "MoveFile" "Pi"
    "ReadAsciiFile" "RebootSystem" "Right" "RunGarbageCollector" "SetDefaultDrive"
    "ShutdownSystem" "Sleep" "StringI" "String" "UCase" "UpTime" "Wait" "WriteAsciiFile"
    "abs" "asc" "atn" "cdbl" "chr" "cint" "cos" "csng" "exp" "fix" "instr" "int"
    "len" "log" "rnd" "sgn" "sgnI" "sin" "sqr" "str" "strI" "strtoi" "tan" "val"))

(defconst simple-brightscript-objects
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
    "roSyncPool" "roSyncPoolEvent" "roSyncPoolFiles" "roSyncPoolProgressEvent"))

(defface simple-brightscript-highlight-face
  '((t :weight bold))
  "Face for highlighting substrings in a text"
  :group 'simple-brightscript-mode)

(defvar simple-brightscript-font-lock
  (let
      ((reserved-keywords-regexp (regexp-opt simple-brightscript-reserved-keywords 'words))
       (primitives-regexp (regexp-opt simple-brightscript-primitives 'words))
       (global-functions-regexp (regexp-opt simple-brightscript-global-functions 'words))
       (objects-regexp (regexp-opt simple-brightscript-objects 'words)))
    `(
      ("^[[:space:]]*?\\(?:sub\\|function\\)\\(?:[[:space:]]*\\)?\\([[:word:]]+\\)\\(?:[^( \t\n]\\)?"
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
      ("^REM\\(.*\\)?$" . font-lock-comment-face)
      (,objects-regexp 0 'simple-brightscript-highlight-face append)
      (,primitives-regexp . font-lock-type-face)
      (,reserved-keywords-regexp . font-lock-builtin-face)
      (,global-functions-regexp . font-lock-constant-face))))

(define-derived-mode simple-brightscript-mode prog-mode "BrightScript"
  "Major mode for editing BrightScript code."
  (setq-local font-lock-defaults
              '(simple-brightscript-font-lock))

  (setq-local comment-start "'")
  (setq-local comment-end "")
  (set-syntax-table simple-brightscript-mode-syntax-table))

(defun case-insensitive-advice ()
  (setq-local font-lock-keywords-case-fold-search t))
(advice-add 'simple-brightscript-mode :after #'case-insensitive-advice)

(provide 'simple-brightscript-mode)
