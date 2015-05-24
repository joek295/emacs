# Fancy-Modeline.el

## Introduction:

Fancy-Modeline is a modeline inspired by Luke Maciak's vim statusline
plugin, [neatstatus](github.com/maciakl/vim-neatstatus).  It aims to
provide a simple, readable UI with the information that matters as an
emacs user.

Fancy-Modeline is designed for use with evil-mode, and is primarily
helpful in keeping track of which state (analagous to vim's modes)
evil-mode is in.  In principle there is no reason why one could not
use fancy-modeline without evil-mode, though.

## Features:

* Evil State Indication: keeps track of evil-mode's state.  The state
  indicator is coloured for easy reading, although in the case of
  replace and insert state, the colour is the same.  The different
  letter key (I vs. R) and the overwrite indicator (which see below)
  serve to differentiate the two.

* Active Window Indication: the unused portion of the modeline is
  highlighted differently for active and inactive windows.

* Buffer Name

* Current Major Mode

* Cursor position: In the form [Percentage read: line number column number].

* Modified indicator

* Read-only indicator: when a file is read-only, the "[RO]" flag is
  added to the end of the modeline.

* Overwrite indicator: when overwriting is active (for instance, in
  evil's replace state), the OVR flag is shown.  When overwriting is
  off, the flag instead reads INS, for "insert".

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

## Configuration:

Fancy-Modeline is (hopefully) sensibly configured.  The first 30-odd
lines define faces for the modeline.  Between their names and
docstrings, they should be fairly self-explanatory.  

Then we define various parts of the modeline: my-modeline-position,
for instance, is the section which tells us whereabouts in the
document the cursor is.  If you want the modeline to show something
else, it should be reasonably simple to write another similar section.

Note here that the final two variables, my-modeline-active and
my-modeline-input have somewhat inconsistent actions, and so I don't
use them.  Some day, if I am sufficiently missing the information that
they would provide, I will write something which replaces them.

Finally, we tie it all together with setq-default mode-line-format.
This is where you can change which indicators are displayed on the
modeline, and which order they are in.  

## UnLicense:

This code is released into the public domain with the
[unlicense](unlicense.org).  The text of the unlicense can be found in
the file UNLICENSE.
