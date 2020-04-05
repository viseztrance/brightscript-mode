# brightscript-mode

[![MELPA](https://melpa.org/packages/brightscript-mode-badge.svg)](https://melpa.org/#/brightscript-mode)

Major emacs mode for editing brightscript files from Brightsign and Roku devices.

Features include syntax highlighting and indentation.

![Preview](doc/preview.png)

## Installation
The package is available on MELPA. Run the following to set it up:

```
M-x package-install brightscript-mode
```
Then add `(require 'brightscript-mode)` to user emacs init file.

### Manual installation

Add the following lines to your emacs config:

```elisp
(add-to-list 'load-path "/path/to/brightscript-mode.el")

(require 'brightscript-mode)
```

## Configuration

Indentation defaults to two spaces. Change `brightscript-mode-indent-offset` to adjust this value.

## License

This is free software, licensed under GPLv3.
