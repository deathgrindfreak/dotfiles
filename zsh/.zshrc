# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="avit"

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
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew aws docker github httpie node npm yarn tmux zsh-autosuggestions kubectl)

source $ZSH/oh-my-zsh.sh

set -o vi
setopt NO_HUP   # Allow zsh to exit without killing backup processes

# Customize to your needs...
export PATH=$PATH:~/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/opt:/var/lib/gems/1.8/bin:~/.cljr/bin/:/usr/lib/postgresql/9.1/bin:~/bin/jdk1.8.0_45/bin:$HOME/build/stack/:$HOME/.local/bin:$HOME/Library/Haskell/bin:$HOME/.cabal/bin

export PATH="/usr/local/opt/openjdk/bin:$PATH"

# OPAM configuration
. /Users/jcbell/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Emacs stuff
if [ -n "$INSIDE_EMACS" ]; then
    chpwd() { print -P "\033AnSiTc %d" }
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# Source all scripts in bashrc.d
for FN in $HOME/Library/bashrc.d/*.sh; do
    source "$FN"
done

#[ -f "/Users/jcbell/.ghcup/env" ] && source "/Users/jcbell/.ghcup/env" # ghcup-env

[ -f "/Users/b377114/.ghcup/env" ] && source "/Users/b377114/.ghcup/env" # ghcup-env
# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/b377114/build/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/b377114/build/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/b377114/build/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/b377114/build/google-cloud-sdk/completion.zsh.inc'; fi
