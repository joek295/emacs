# Introduction:

This directory contains my emacs configuration files.  These include:

* init.el

* the ./lisp folder, containing much of the code called from init.el.

* the ./fancy-modeline folder, containing my modeline configuration,
  which was inspired by Luke Maciak's vim-neatstatus.

# Outline:

Before I switched to emacs, I was a heavy vim user, and my emacs
configuration integrates evil-mode into my workflow wherever possible,
reflecting this preference for modal editing.

The system is also increasingly modular, with configuration split
across multiple files.  What follows is a whirlwind tour of what to
expect to find in each configuration file.

## Setup:

* init.el: sets up configuration.  initialises package-management,
ensures use-package is available, and loads other configuration.

## General Configuration:

* my-config.el: universal configuration.  settings controlling such
things as scrolling behaviour, tab behaviour, and backups.

* my-custom.el: all configuration performed through the `M-x
  customize` menu.  This is being phased out as much as possible.

* functions.el: generic functions.  Includes any advice or function
  which is not specifically for another mode, which would then go in
  the relevant file.

* modes.el: mode-specific configuration, including enabling modes

* keys.el: my personal keybindings

## Configuring specific modes:

* my-evil.el: evil-mode configuration.  This is largely taken up with
  keybindings for evil-mode and evil-leader-mode.  It also defines
  some useful functions, and defines advice for other built-in functions.

* my-magit.el: magit-mode configuration.  Mainly sets the keymap for
  magit in evil-mode, but also sets some options.

* my-ido.el: ido-mode configuration.  Includes configuration for
  ido-ignore-files and ido-ignore-buffers.

* my-elfeed.el: elfeed configuration.  Elfeed is an RSS reader for
  emacs.  This is mainly used for configuring which feeds elfeed
  should watch.

## My Modes:

* fortunate.el: a major mode for files compatible with GNU Fortunes

* nethackrc.el, bindkeys.el, viml.el: three generic-mode based modes,
  providing some level of syntax highlighting respectively for Nethack
  configuration files, XBindkeys configuration files, and viml files.

## Fancy-Modeline:

* fancy-modeline: my modeline, based off of Luke Maciak's
  vim-neatstatus.

# Standards:

Configuration files should be named my-x.el.  Configuration files for
specific modes should be named my-modename.el, while other
configuration files should have some sort of descriptive name,
e.g. my-config.el for general configuration, my-custom.el for M-x
customize configuration etc.

