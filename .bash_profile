alias emacs22='/usr/bin/emacs'
alias emacs23='/Applications/Emacs\ 23.app/Contents/MacOS/Emacs -nw'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias Emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias firefox='/Applications/Firefox.app/Contents/MacOS/firefox'
alias maple='/Library/Frameworks/Maple.framework/Versions/12/bin/maple'
# alias octave='/Applications/Octave.app/Contents/Resources/bin/octave'
alias xmaple='/Library/Frameworks/Maple.framework/Versions/12/bin/xmaple'

# Setting PATH for Python 3.4
export PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH="/usr/local/bin:${PATH}"

export HISTFILESIZE=700

export DICTIONARY=fr

#   Change Prompt
#    export PS1="________________________________________________________________________________\n| \w @ \h (\u) \n| => "
#    export PS2="| => "

# export PS1="\h:\W \$ "
# export PS1="\s-\v:\W \$ "
# export PS1="*\s-\v-*:\W \$ "
# export PS1="\h:\W \u\$"     #default prompt on Terminal

export PS1="\[\e[1m\]\h:\W \u\$ \[\e[0m\]"  #bold prompt

# OPAM configuration
. /Users/jaouadmourtada/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# added by Anaconda3 4.2.0 installer
export PATH="/Users/jaouadmourtada/anaconda3/bin:$PATH"
