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
    antigen bundle fasd
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
export FZF_DEFAULT_OPTS='--height 15 --no-reverse'

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

case `uname` in
  Linux)
      # FASD
      eval "$(fasd --init posix-alias zsh-hook)"
      # Aliases
      alias a='fasd -a'        # any
      alias s='fasd -si'       # show / search / select
      alias d='fasd -d'        # directory
      alias f='fasd -f'        # file
      alias sd='fasd -sid'     # interactive directory selection
      alias sf='fasd -sif'     # interactive file selection
      alias z='fasd_cd -d'     # cd, same functionality as j in autojump
      alias zz='fasd_cd -d -i' # cd with interactive selection
      ;;
esac

case `uname` in
  Darwin)
    eval "$(zoxide init zsh)"
    ;;
esac

alias rm='rm -i'

alias fuck='sudo $(fc -ln -1)'

alias vim="nvim"
alias vi="nvim"
alias em="emacs -nw"
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

export EDITOR='emacs -nw'

export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#696969"

# Append a command directly
zvm_after_init_commands+=('[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh')

eval "$(starship init zsh)"

# Any local stuff
[[ ! -f ~/.zshrc-local.zsh ]] || source ~/.zshrc-local.zsh

[ -f ~/.local_shell_stuff ] && source ~/.local_shell_stuff

export ADB_LIBUSB="0"
