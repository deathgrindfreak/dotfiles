# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/jcbell/.fzf/bin* ]]; then
  export PATH="$PATH:/Users/jcbell/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/jcbell/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/Users/jcbell/.fzf/shell/key-bindings.zsh"

