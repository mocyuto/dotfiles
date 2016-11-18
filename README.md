dotfiles
========

![Build Status](https://travis-ci.org/moc-yuto/dotfiles.svg?branch=master)


This is my preference files.
These are almost dotfiles.

setting about pyenv
-------
```
git clone https://github.com/yyuu/pyenv.git ~/.pyenv
```

 about emacs on .zshenv
--------
please install emacs-24.3 or later, or comment-region about yasnnipet

# auto-complete
download auto-complete  
`M-x load-file auto-complete/etc/install.el`

# helm
```
git clone https://github.com/emacs-helm/helm.git ~/Github/helm
make
# add .emacs
(add-to-list 'load-path "~/Github/helm")
(require 'helm-config)
```

#haskell-mode
```
git clone https://github.com/haskell/haskell-mode.git ~/Github/haskell-mode
cd ~/Github/haskell-mode
make all
```

#scala-mode
```
git clone git://github.com/hvesalai/scala-mode2.git
```
