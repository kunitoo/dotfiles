autoload -U compinit
compinit

zstyle ':completion:*' list-colors 'di=;34;1' 'ln=;35;1' 'so=;32;1' 'ex=31;1' 'bd=46;34' 'cd=43;34'

## Default shell configuration
#
# set prompt
#
autoload colors; colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b](%a)'

precmd () {
    psvar=()
    vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
PROMPT=$'%B%F{green}%n%f %F{blue}%~%f%b%1(v| %F{green}%1v%f|)\n%B%F{blue}$%f%b '

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

zmodload -i zsh/complist
autoload -Uz compinit && compinit
zstyle ':completion:*:default' menu select=2

# 補完時にhjklで選択
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char

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

# completion
fpath=($HOME/.zsh/functions $fpath)
autoload -U compinit; compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

alias g='git'
alias gg='git grep'
alias gs='git status'
alias ls='ls -lh --color'
alias r='rails'
alias ll='ls'
alias rs='bundle exec rspec'

alias hub-pr='hub pull-request -o'

alias be='bundle exec'
alias bi='bundle install'
alias bil='bundle --local'

alias dc='docker-compose'

alias mux='tmuxinator'
# source ~/src/github.com/tmuxinator/tmuxinator/completion/tmuxinator.zsh

alias peco='TERM=xterm peco'

export GTK_IM_MODULE=uim
export QT_IM_MODULE=uim
export XMODIFIERS=@im=uim

. $HOME/.asdf/asdf.sh
# eval "$(rbenv init -)"
# eval "$(direnv hook zsh)"
unset RUBYOPT

export TERM="xterm-256color"
export EDITOR='vim'
export GOPATH=$HOME
export PATH=$HOME/bin:$PATH

# Android
export ANDROID_HOME=$HOME/bin/android-sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools

function peco-src () {
    local selected_dir=$(ghq list --full-path | peco --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-src
bindkey '^]' peco-src
