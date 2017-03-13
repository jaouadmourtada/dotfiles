### Init script for OS X

# Reformat mac (press r at startup), install fresh OS X, and update to
# latest version. Create account.

# Install command line tools
xcode-select --install

# Get Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install Hombrew formulas
brew install bash ffmpeg wget youtube-dl hunspell git curl ocaml opam python

# OPAM
opam init && eval `opam config env`
opam info merlin && opam install merlin

brew tap homebrew/science && brew install octave

## Download and install Applications

# iTerm (and grab themes)
wget -O iterm.zip https://iterm2.com/downloads/stable/latest
unzip iterm.zip
mv iterm.zip ~/.Trash/
mv iTerm.app/ /Applications

# Firefox (and sync)
wget -O Firefox.dmg "https://download.mozilla.org/?product=firefox-latest&os=osx&lang=fr"
hdiutil mount Firefox.dmg 
sudo cp -R "/Volumes/Firefox/Firefox.app" /Applications
hdiutil unmount "/Volumes/Firefox/"
mv Firefox.dmg ~/.Trash/

# Download and install:
# 1. MacTeX
# 2. Dropbox
# 3. Emacs
# 4. Virtual Box
# 5. VLC
# 6. Skim
# 7. Transmission
# 8. Flux
# 9. Libre Office ?
# 10. DjView

# Copy (from backup or dropbox):
# .bash_profile
# .bash_history
# .latexmkrc
# init.el
# Dictionaries (in ~/Library/Spelling/)

# Setup Emacs : put init.el in ~/.emacs.d/ and install packages from
# MELPA: aggressive-indent, auctex , caml, company,
# company-statistics, exec-path-from, frame-cmds, frame-fns, hlinum,
# ido-sort-mtime, ido-ubiquitous, ido-vertical-mode, moe-theme,
# powerline, smex, tuareg

# Disable wifi popup 
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.captive.control Active -boolean false

# Dowload and install Anaconda
conda update --all

# git config

# Aspell
#brew install aspell --with-lang-en --with-lang-fr
#echo htink | aspell -a --sug-mode=ultra --lang=en_US
