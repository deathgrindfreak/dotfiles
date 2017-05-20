# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="candy"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

setopt NO_HUP   # Allow zsh to exit without killing backup processes

# Customize to your needs...
export PATH=$PATH:~/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:~/.config/bspwm_bar:/opt:GRADLE_HOME/bin:/var/lib/gems/1.8/bin:~/.cljr/bin/:$HOME/Android/sdk/tools/:$HOME/Android/sdk/platform-tools/:$HOME/eclipse/:$HOME/bin/glassfish4/bin/:/usr/lib/postgresql/9.1/bin:~/bin/jdk1.8.0_45/bin:$HOME/build/stack/:$HOME/.local/bin:$MAVEN_HOME/bin

# pkg-config path
export PKG_CONFIG_PATH=/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/i386-linux-gnu/pkgconfig/:/usr/include/

# default browser
export BROWSER=chrome

# default editor
export EDITOR=emacs

# chicken scheme
export CSC_OPTIONS="-I/usr/include/iup"

alias urxvt='urxvt -e tmux'
alias mocscrob='python3 ~/.mocpscrob/mocp-scrobbler.py -d'
alias csi='rlwrap -pBlue csi'
alias zathura='tabbed zathura -e'
alias st='rlwrap st'
alias racket='racket -il xrepl'
alias ocaml='rlwrap ocaml'
alias sbcl='rlwrap sbcl'
alias clisp='rlwrap clisp'
alias guile='rlwrap guile'
alias mit-scheme='rlwrap mit-scheme'
alias guile-ncurses-shell='rlwrap guile-ncurses-shell'
# alias python='ipython'
# alias vim='nvim'
alias weather='curl wttr.in/76513'
alias e='emacsclient -c'
alias kill-qemu='vagrant ssh -c "killall qemu-system-i386"'
alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'

export EQUIS_DIR=$HOME/.equis
x() { source "/usr/local/bin/equis/" "$@"; }

# get rid of that gnome-keyring bullshit
unset GNOME_KEYRING_CONTROL

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Add terraform plan and apply
export PATH="$PATH:$HOME/bin/provision-ops/bin"

export SUDO_USER=deathgrindfreak

export PANEL_HEIGHT=18
export PANEL_FIFO="/tmp/panel-fifo"
export PANEL_FONT_FAMILY="-*-terminus-medium-r-normal-*-12-*-*-*-c-*-*-1"

# AWS variables
export AWS_PROFILE=vixprod
export AWS_REGION=us-east-1

export VIX_API_URL=https://mrip32r4d8.execute-api.us-east-1.amazonaws.com/latest

# AWS credential switcher
creds(){
    case "$1" in
        prod)
            export AWS_PROFILE=vixprod
            ;;
        dev)
            export AWS_PROFILE=vixdev
            ;;
        *)
            echo "Expected one of [dev,prod]"
            ;;
    esac
}

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# OPAM configuration
. /home/deathgrindfreak/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Emacs stuff
if [ -n "$INSIDE_EMACS" ]; then
    chpwd() { print -P "\033AnSiTc %d" }
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
fi
