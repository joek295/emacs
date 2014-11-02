# Fancy-Modeline.el

## Introduction:

Fancy-Modeline is a modeline inspired by Luke Maciak's vim statusline
plugin, [neatstatus](github.com/maciakl/vim-neatstatus).  It aims to
provide a simple, readable UI with the information that matters as an
emacs user.  It is designed for use with evil-mode for emacs, though
it can be used without it.

## Features:

* Evil State Indication: keeps track of whether the user is in normal,
  insert, visual, replace, or emacs state

* Active Window Indication: has a notification for the currently
  active window

* Buffer name

* Current major mode

* Cursor position: line and column number, and percentage of file read

* Modified indicator

* Read-only indicator

* Overwrite indicator

* Indicator for current insert method

## Future Features:

Features which may be implemented in the future include:

* Total number of lines

* Optional un-modified indicator

* File encoding

## Installation:

1. Download fancy-modeline

2. Add to your init.el:

(require 'fancy-modeline)

3. Profit
