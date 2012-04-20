PATH="$PATH":/usr/local/bin/
export PATH

autoload -U compinit
compinit

zstyle ':completion:*' list-colors 'di=;34;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'

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

# カラーなls
export LSCOLORS=ExFxCxdxBxegedabagacad

# homebrew補完設定
fpath=($HOME/.zsh/functions $fpath)

alias g='git'
alias ls='ls -lG'
alias gx='gitx'
alias r='rails'
alias ll='ls -lG'

# export EDITOR=emacsclient
# export VISUAL=emacsclient
export JAVA_HOME=/Library/Java/Home
export GROOVY_HOME=/usr/local/Cellar/groovy/1.8.6/libexec
[[ -s "/Users/kuni/.rvm/scripts/rvm" ]] && source "/Users/kuni/.rvm/scripts/rvm"
export PATH=$PATH:~/.gem/ruby/1.8/bin/
export GRAILS_HOME=/usr/local/bin/grails


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
