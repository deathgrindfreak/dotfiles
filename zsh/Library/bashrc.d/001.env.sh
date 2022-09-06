export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# ConTeXt install path
export PATH="$PATH:/Users/jcbell/context/tex/texmf-osx-64/bin"

# Add terraform plan and apply
export PATH="$PATH:$HOME/src/provision-ops/bin"

# brew Cellar
export PATH="$PATH:/usr/local/Cellar/"

export PATH="$PATH:/Users/jcbell/.emacs.d/bin"

export PATH="$PATH:/Applications/Emacs.app/"

# J language
export PATH="$PATH:/Applications/j903/bin/"

# z command
. $HOME/bin/z/z.sh

# default browser
export BROWSER=firefox

# default editor
export EDITOR='emacsclient -c -nw'

# pkg-config path
export PKG_CONFIG_PATH=/usr/lib/pkgconfig:/usr/share/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/i386-linux-gnu/pkgconfig/:/usr/include/

# chicken scheme
export CSC_OPTIONS="-I/usr/include/iup"

# add support for ctrl+o to open selected file in VS Code
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(code {})+abort'"

export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"

# lisp programs
export PATH="$PATH:$HOME/dotfiles/lisp/bin"
export MANPATH="$MANPATH:$HOME/dotfiles/lisp/man"

# AWS variables
export AWS_PROFILE=vixprod
export AWS_REGION=us-east-1
export VIX_REGION=ue1
export VIX_ENV=dev

export PROMPT_COMMAND='if [ "$(id -u)" -ne 0 ]; then echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $(history 1)" >> ~/.logs/bash-history-$(date "+%Y-%m-%d").log; fi'

export BLACKDUCK_API='Y2JiYzFiZTQtMmRlMS00MGY3LTg3NjItZDBiYjNlOTdjNzA2OjNkNTgwZWJkLTgzMmQtNDU1YS05NTRhLTJkOGU3MWQzMDg0Ng=='

export GTAGSLABEL=pygments

complete -W 'singularity logging entity' open_db_windows
complete -W 'singularity logging entity ops' vix_psql

export NODE_EXTRA_CA_CERTS="$HOME/.vix/veracity-root-ca.crt"
