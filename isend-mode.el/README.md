# isend-mode

`isend-mode` is an Emacs extension allowing interaction with code interpreters in `ansi-term` or
`term` buffers. Some language-specific modes (e.g. `python.el`) already provide similar features;
`isend-mode` does the same in a language-agnostic way.


## Installation

Just clone the repository. For example:

```shell
git clone https://github.com/ffevotte/isend-mode.el.git /path/to/isend-mode
```

Then, add the following lines in your Emacs initialization file (`.emacs` or `.emacs.d/init.el`):

```lisp
(add-to-list 'load-path "/path/to/isend-mode")
(require 'isend)
```


## Basic usage

The following example demonstrates using `isend-mode` to interact with a shell in an `ansi-term`
buffer. Please note that any other interpreter could have been used (e.g. python, perl or anything
else) and `term` would have worked as well.


1. Open an `ansi-term` buffer where the interpeter will live. For example:

   `M-x ansi-term RET /bin/sh RET`


2. Open a buffer with the code you want to execute, and associate it to the interpreter buffer using
   the `isend-associate` command. For example:

   `M-x isend-associate RET *ansi-term* RET`


3. Hitting `C-RET` will send the current line to the interpreter. If a region is active, all lines
   spanned by the region will be sent (i.e. no line will be only partially sent). Point is then
   moved to the next non-empty line (but see configuration variable `isend-skip-empty-lines`).


## Use cases

- **Interactive demo of a text-based program:** you prepare all the commands you want to run in a
  buffer and interactively send them to the interpreter as if you had typed them.

- **Running interpreted code step by step:** this is for example useful if you often run the same
  list of shell commands but don't want to formally handle all possible errors in a script.


## Customization

`isend-mode` can be customized with `M-x customize-group RET isend RET`

The variables which can be set to customize `isend`'s behaviour are:

- `isend-forward-line`: if non-nil (default), `isend` advances to the next line after having sent
  some content using `C-RET`.

- `isend-skip-empty-lines`: if non-nil (default), `isend` will skip empty lines (i.e. lines
  containing only whitespace) and position point on the first following non-empty line. Some
  interpreters (like Python) care about empty lines. In such cases it might be useful to set
  `isend-skip-empty-lines` to nil.

- `isend-strip-empty-lines`: if non-nil, `isend` will remove empty (or whitespace-only) lines from
  the region before sending it to the interpreter. Note that this only works when sending an entire
  region (as opposed to a single line).
  
- `isend-delete-indentation`: if non-nil, `isend` will delete indentation from all lines in the
  region. Note that this only works when sending a region (as opposed to a single line). Relative
  indentation w.r.t the first line is preserved. This is useful e.g. to send Python blocks outside
  of their original context.

- `isend-end-with-empty-line`: if non-nil, `isend` appends an empty line to regions sent. Note that
  this only works when sending an entire region (as opposed to a single line).

- `isend-send-line-function` and `isend-send-region-function`: these are the functions called by
  `isend` to send a line or a region respectively. These functions take as argument the name of a
  buffer containing the text to be sent, and are responsible for copying this text to the
  interpreter buffer.
  
  Possible values include:
  - `insert-buffer-substring` (default) : simply insert the text into the buffer.
  - `isend--ipython-paste` : copy the text to the clipoard, and evaluate `%paste` in the interpreter
    buffer (where an `iPython` process is supposed to be running).
  - `isend--ipython-cpaste` : insert the text within a `%cpaste` command (an `iPython` processes
    is supposed to be running in the associated buffer).


### Setup helpers

A few helpers are provided to help setup `isend` when working with multiple languages:
  
```lisp
;; If you work with shell scripts
(add-hook 'isend-mode-hook 'isend-default-shell-setup)

;; If you work with python scripts using the default python interpreter
(add-hook 'isend-mode-hook 'isend-default-python-setup)

;; If you work with python scripts using iPython
(add-hook 'isend-mode-hook 'isend-default-ipython-setup)
```


## Contributing

If you make improvements to this code or have suggestions, please do not hesitate to fork the
repository or submit bug reports on [github](https://github.com/ffevotte/isend-mode.el). The repository's
URL is:

    https://github.com/ffevotte/isend-mode.el.git


Many thanks go to [James Porter](https://github.com/porterjamesj) for his contributions on empty
lines handling.


## License

Copyright (C) 2012 François Févotte.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not,
see <http://www.gnu.org/licenses/>.
