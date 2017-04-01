# Emacs Configuration

This repo contains the source code to my Emacs configuration.

## Installation

1. `git clone` this repo to your `~/.emacs.d/` directory
2. Start Emacs and let it set up itself

## Adding new packages

1. Add your package in the list `(setq package-list ( .. <your_package_name>))`
2. `M-x [ret] eval-buffer [ret]` will reset the configuration and install any missing packages

Enjoy!
