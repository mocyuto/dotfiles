dotfiles
========

This is my preference files.
These are almost dotfiles.

 about emacs on .zshenv
--------
please install emacs-24.3 or later, or comment-region about yasnnipet

# auto-complete
download auto-complete
M-x load-file auto-complete/etc/install.el

# helm
```
git clone https://github.com/emacs-helm/helm.git ~/Github/helm
make
# add .emacs
(add-to-list 'load-path "~/Github/helm")
(require 'helm-config)
```
