PATH="$PATH":/usr/local/bin/
export PATH

autoload -U compinit
compinit

## Environment variable configuration
#
# LANG
#
export LANG=ja_JP.UTF-8

## Default shell configuration
#
# set prompt
#
PROMPT="%n%% "
RPROMPT="[%~]"

# set terminal title including current directory

case "${TERM}" in
kterm*|xterm)
    precmd() {
        echo -ne "\033]0;${USER}:${PWD}\007"
    }
    ;;
esac

## Command history configuration

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data

## Keybind configuration
#
# emacs like keybind (e.x. Ctrl-a goes to head of a line and Ctrl-e goes 
#   to end of it)
#
bindkey -e
 
# historical backward/forward search with linehead string binded to ^P/^N

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward
bindkey "^[p" history-beginning-search-backward-end
bindkey "^[n" history-beginning-search-forward-end

# homebrew補完設定
fpath=($HOME/.zsh/functions $fpath)

alias g='git'
alias gi='git'
alias ls='ls -l'
alias gx='gitx'
alias r='rails'

# export EDITOR=emacsclient
# export VISUAL=emacsclient
export JAVA_HOME=/Library/Java/JavaVirtualMachines/1.6.0_26-b03-384.jdk/Contents/Home
export GROOVY_HOME=/usr/local/Cellar/groovy/1.8.5/libexec
[[ -s "/Users/kuni/.rvm/scripts/rvm" ]] && source "/Users/kuni/.rvm/scripts/rvm"
export PATH=$PATH:~/.gem/ruby/1.8/bin/

