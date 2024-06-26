# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# append various stuff to PATH
append_to_path() {
  if [ -d "$1" ] ; then
    export PATH="$1:$PATH"
  fi
}

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

# Load antigen from somewhere
if [ -f /usr/local/share/antigen/antigen.zsh ]; then
    source /usr/local/share/antigen/antigen.zsh
elif [ -f /usr/share/zsh-antigen/antigen.zsh ]; then
    source /usr/share/zsh-antigen/antigen.zsh
else
    if [ ! -f $HOME/antigen.zsh ]; then
        curl -L git.io/antigen > $HOME/antigen.zsh
    fi
    source $HOME/antigen.zsh
fi

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
case `uname` in
  Linux)
    antigen bundle zsh-users/zsh-autosuggestions
  ;;
esac
antigen bundle git
antigen bundle pip
antigen bundle python
antigen bundle command-not-found
#antigen bundle common-aliases
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle mafredri/zsh-async
# antigen bundle jeffreytse/zsh-vi-mode
antigen theme romkatv/powerlevel10k
case `uname` in
  Darwin)
      antigen bundle brew
      antigen bundle brew-cask
      antigen bundle osx
      ;;
esac

# Tell Antigen that you're done.
antigen apply

# FZF
#export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Environment
if [ -z "$USER" ]; then
    USER=$LOGNAME
fi

# Functions
fvim () {
    vim $(find -type f -and -not -path "*_build*" -name "$1*")
}

gvim () {
    vim -q <(git grep $1)
}

rgvim() {
    vim -q <(rg --line-number --column --no-heading --fixed-strings --ignore-case --no-ignore --hidden $1)
}

if type ssh-wrapper > /dev/null 2>&1; then
    ssh() {
        ssh-wrapper "$@"
    }
fi

alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

eval "$(zoxide init zsh)"

alias rm='rm -i'

alias fuck='sudo $(fc -ln -1)'

alias vim="nvim"
alias vi="nvim"


if [ -f "$HOME/.config/emacs/bin/doom" ]; then
    alias em="emacs -nw"
else
    alias em="emacsclient -nw"
fi

alias asu="adb shell su root"

reboot() { echo  "I'm pretty sure you screwed up..."; }

# Misc zsh setup
zstyle ':completion:*' special-dirs true
zstyle ':completion:*:*' ignored-patterns '*ORIG_HEAD'

DISABLE_AUTO_TITLE="true"

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_all_dups   # ignore even more dups
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
#setopt share_history          # share command history data

export EDITOR=vim

export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#696969"

# Append a command directly
zvm_after_init_commands+=('[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh')

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Any local stuff
[[ ! -f ~/.zshrc-local.zsh ]] || source ~/.zshrc-local.zsh

[ -f ~/.local_shell_stuff ] && source ~/.local_shell_stuff
