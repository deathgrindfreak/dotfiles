alias csi='rlwrap -pBlue csi'
#alias racket='racket -il xrepl'
alias ocaml='rlwrap ocaml'
alias sbcl='rlwrap sbcl'
alias clisp='rlwrap clisp'
alias guile='rlwrap guile'
alias guile-ncurses-shell='rlwrap guile-ncurses-shell'
alias weather='curl wttr.in/76513'
alias wt=weather
alias e='emacsclient -c -nw'
alias ec='emacsclient -c &'
alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
alias ccl="rlwrap ccl64"

alias Pgrep='ggrep -P'
alias cat='bat'
alias sed='gsed'
alias awk='gawk'
alias date='gdate'
alias ping='prettyping --nolegend'
alias preview="fzf --preview 'bat --color \"always\" {}'"
alias vim='nvim'
alias top='sudo htop'
alias ncdu="ncdu --color dark -x --exclude .git"
alias help="tldr"
alias ls="exa"

alias cdpo="cd ~/src/provision-ops"
alias k="kubectl"
alias df="duf"
alias tree="tre"

alias color_json=$'jq -R -r \'. as \$line \| try fromjson catch \$line\''
