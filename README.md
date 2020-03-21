# brightscript-mode

Major emacs mode for editing brightscript files from Brightsign and Roku devices.

Features include syntax highlighting and indentation.

![Preview](doc/preview.png)

## Installation
Add the following lines to your emacs config:

```elisp
(add-to-list 'load-path "/path/to/brightscript-mode")

(require 'brightscript-mode)
```

## Configuration

Indentation defaults to two spaces. Change `brightscript-mode-indent-offset` to adjust this value.

## License

This is free software, licensed under GPLv3.
