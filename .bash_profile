alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias Emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias firefox='/Applications/Firefox.app/Contents/MacOS/firefox'

export HISTFILESIZE=700

export DICTIONARY=fr

#   Change Prompt
# export PS1="*\s-\v-*:\W \$ "
# export PS1="\h:\W \u\$"     #default prompt on Terminal

export PS1="\[\e[1m\]\h:\W \u\$ \[\e[0m\]"  #bold prompt

export PATH="/usr/local/bin:$PATH"

# OPAM configuration
. /Users/jaouadmourtada/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# added by Anaconda3 4.3.0 installer
export PATH="/Users/jaouadmourtada/anaconda3/bin:$PATH"

