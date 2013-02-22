PATH="$PATH":/usr/local/bin/:~/bin
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

# z

. `brew --prefix`/etc/profile.d/z.sh
function precmd () {
  z --add "$(pwd -P)"
}

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

# completion
fpath=($HOME/.zsh/functions $fpath)
autoload -U compinit; compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

alias g='git'
alias ls='ls -lGh'
alias gx='gitx'
alias r='rails'
alias ll='ls'

alias be='bundle exec'

alias ctags=`brew --prefix`/bin/ctags

# export EDITOR=emacsclient
# export VISUAL=emacsclient
export JAVA_HOME=/Library/Java/Home
export GROOVY_HOME=/usr/local/Cellar/groovy/1.8.6/libexec
export GRAILS_HOME=/usr/local/bin/grails
export ANDROID_HOME=/Applications/adt-bundle-mac-x86_64/sdk/
export PATH=$PATH:/Applications/adt-bundle-mac-x86_64/sdk/tools:/Applications/adt-bundle-mac-x86_64/sdk/platform-tools # Android

