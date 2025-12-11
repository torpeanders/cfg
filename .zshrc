# ==============================================================================
# Environment Variables & Path
# ==============================================================================

# Set USER if not already set
if [ -z "$USER" ]; then
    USER=$LOGNAME
fi

# Default editor
export EDITOR='emacs -nw'

# FZF configuration
export FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'
export FZF_DEFAULT_OPTS='--height 15 --no-reverse'
#export FZF_DEFAULT_COMMAND="fd --hidden --exclude .git"
#export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#export FZF_ALT_C_COMMAND="fd --type d --hidden --exclude .git"

#export FZF_DEFAULT_OPTS="
#  --height=90%
#  --layout=reverse
#  --border
#  --preview 'bat --style=numbers --color=always {}'
#"

# Ripgrep configuration file
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# NVM (Node Version Manager) directory
export NVM_DIR="$HOME/.nvm"

# For Android Debug Bridge (ADB)
export ADB_LIBUSB="0"

# For ZSH Autosuggestions
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#696969"

# For ZSH Title
DISABLE_AUTO_TITLE="true"

# bat
#export BAT_THEME="TwoDark"
export BAT_PAGER="less -RF"
export PAGER="bat"

# eza
export EZA_ICONS=1

# Helper function to prepend a directory to the PATH
append_to_path() {
  if [ -d "$1" ] ; then
    export PATH="$1:$PATH"
  fi
}

# Prepend various directories to PATH
append_to_path "$HOME/.bin"
append_to_path "$HOME/.cargo/bin"
append_to_path "$HOME/.config/emacs/bin"
append_to_path "$HOME/.fzf/bin"
append_to_path "$HOME/.scripts"
append_to_path "$HOME/bin"

# Set the list of directories that Zsh searches for programs.
path=(
    /usr/local/{bin,sbin}
    $path
)

# ==============================================================================
# Shell Options (setopt)
# ==============================================================================

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_all_dups   # ignore even more dups
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
#setopt share_history          # share command history data

# ==============================================================================
# Custom Functions
# ==============================================================================

# Wrapper for ssh if ssh-wrapper exists
if type ssh-wrapper > /dev/null 2>&1; then
    ssh() {
        ssh-wrapper "$@"
    }
fi

# A gentle reminder instead of a destructive command
reboot() { echo  "I'm pretty sure you screwed up..."; }

# ==============================================================================
# Plugin & Tool Initialization
# ==============================================================================

# --- Antigen (Zsh Plugin Manager) ---
# Load antigen from standard locations
if [ -f /usr/local/share/antigen/antigen.zsh ]; then
    source /usr/local/share/antigen/antigen.zsh
elif [ -f /usr/share/zsh-antigen/antigen.zsh ]; then
    source /usr/share/zsh-antigen/antigen.zsh
else
    # If not found, download it
    if [ ! -f $HOME/antigen.zsh ]; then
        curl -L git.io/antigen > $HOME/antigen.zsh
    fi
    source $HOME/antigen.zsh
fi

# Use oh-my-zsh library
antigen use oh-my-zsh

# Load Antigen bundles
antigen bundle git
antigen bundle pip
antigen bundle python
antigen bundle command-not-found
#antigen bundle common-aliases
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle mafredri/zsh-async
# antigen bundle jeffreytse/zsh-vi-mode

# OS-specific bundles
case `uname` in
  Linux)
    antigen bundle zsh-users/zsh-autosuggestions
  ;;
  Darwin)
      antigen bundle brew
      antigen bundle brew-cask
      antigen bundle osx
      ;;
esac

# Apply Antigen changes
antigen apply

# --- Zoxide (Smart Directory Changer) ---
eval "$(zoxide init zsh)"

# --- FZF (Fuzzy Finder) ---
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# --- Starship (Prompt) ---
eval "$(starship init zsh)"

# --- NVM (Node Version Manager) ---
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# --- ZVM ---
# Append a command directly
zvm_after_init_commands+=('[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh')

# ==============================================================================
# Zsh Completion System
# ==============================================================================

# Misc zsh setup
zstyle ':completion:*' special-dirs true
zstyle ':completion:*:*' ignored-patterns '*ORIG_HEAD'

# ==============================================================================
# Aliases
# ==============================================================================

alias asu="adb shell su root"
alias cat="bat"
#alias cd="z"
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias du='dust'
alias em="emacs -nw"
alias g='gitui'
alias la="eza -a"
alias ll="eza -alF --icons"
alias ls="eza"
alias ps='procs'
alias rm='rm -i'
alias top='btop'
alias v="nvim"
alias vim="nvim"

# ==============================================================================
# Local/Personal Configuration
# ==============================================================================

# Source local files for overrides and additions
[[ ! -f ~/.zshrc-local.zsh ]] || source ~/.zshrc-local.zsh
[ -f ~/.local_shell_stuff ] && source ~/.local_shell_stuff
