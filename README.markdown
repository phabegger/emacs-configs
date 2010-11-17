# Emacs Configuration

## Languages


## Installation of emacs und utils

I'm using this configuration on **Mac OS X** with **cocoa emacs**.

*  Install [homebrew](https://github.com/mxcl/homebrew), *the missing package manager for OS X*
*  Install cocoa emacs using homebrew: `brew install emacs --use-git-head --HEAD --cocoa`
*  For jabber support install *gnutls*: `brew install gnutls` (optional)
*  XML Starlet for xml flymake checks: `brew install xmlstarlet` (optional)
*  [contacts](http://gnufoo.org/contacts/) for mail / sms (optional)


## Installation of emacs config

*  Fork my emacs configs on github
*  Clone your forked emacs repo with git into any path you like (I have it under ~/projects/emacs-configs) using git (e.g. `git clone git@github.com:user/emacs-configs.git)
*  Update the submodules: `git submodule init` and `git submodule update`
*  Install emacs configs by symlinking it from `~/.emacs.d` (e.g. `ln -s /Users/username/.emacs.d /Users/username/projects/emacs-configs`)
*  Start emacs from shell using `open -a emacs` or `emacsclient`
*  The first time it will install some stuff from elpa, this could take a while.
