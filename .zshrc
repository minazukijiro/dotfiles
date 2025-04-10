if (( ${ZPROF:-0} )); then
    zmodload zsh/zprof; zprof
fi

if (( $+commands[tmux] && ! $+TMUX && $+SSH_CONNECTION )); then
    tmux has -t ssh && exec tmux attach -t ssh
    exec tmux new -s ssh
fi

bindkey -e

ttyctl -f

reset_broken_terminal() {
    printf '%b' '\e[0m\e(B\e)0\017\e[?5l\e7\e[0;0r\e8'
}

precmd_functions+=(reset_broken_terminal)

clear-screen-and-scrollback() {
    printf '\x1Bc'; zle clear-screen
}

zle -N clear-screen-and-scrollback
bindkey  clear-screen-and-scrollback

typeset -U path
path+=(~/bin(N-/) ~/.local/bin(N-/))

typeset -U cdpath
cdpath+=(~)
:

if [[ "$TERM" == 'linux' ]]; then
    return 0
fi

: keybinding

bindkey b backward-word
bindkey f forward-word

: completion

setopt COMPLETE_ALIASES
setopt COMPLETE_IN_WORD
setopt GLOB_COMPLETE

autoload -Uz compinit

() {
    if [[ $1(#qNmh-20) ]]; then
        compinit -C -d "$1"
    else
        mkdir -p "$1:h"
        compinit -i -d "$1"
        touch "$1"
    fi
} "${XDG_CACHE_HOME:-$HOME/.cache}/zcompdump"

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zcompcache"

zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}' 'm:{[:upper:]}={[:lower:]}'  'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3>7?7:($#PREFIX+$#SUFFIX)/3))numeric)'

zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${=${=${${(f)"$(cat {/etc/ssh/ssh_,~/.ssh/}known_hosts(|2)(N) 2> /dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2> /dev/null))"}%%(\#${_etc_host_ignores:+|${(j:|:)~_etc_host_ignores}})*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2> /dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

zstyle '*' single-ignored show

: rm

zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

: kill

zstyle ':completion:*:*:*:*:processes' command 'ps -u $LOGNAME -o pid,user,command -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

: man

zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

: ssh/scp/rsync

zstyle ':completion:*:(ssh|scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

:



: glob

setopt EXTENDED_GLOB
setopt NULL_GLOB

: history

setopt EXTENDED_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_LEX_WORDS
setopt HIST_NO_FUNCTIONS
setopt HIST_NO_STORE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt SHARE_HISTORY

export HISTFILE=~/.zsh_history
export SAVEHIST=100000
export HISTSIZE=$((SAVEHIST + 1))

: do not add failed commands to history

zshaddhistory() { return $? }

bindkey  history-incremental-pattern-search-backward
bindkey  history-incremental-pattern-search-forward
bindkey  history-beginning-search-backward
bindkey  history-beginning-search-forward

: znap

if [[ ! -f ~/.znap/znap.zsh ]]; then
    rm -rf ~/.znap
    git clone --depth 1 https://github.com/marlonrichert/zsh-snap.git ~/.znap
fi

source ~/.znap/znap.zsh

if [[ ! -d ~/.znap/repos ]]; then
    mkdir -p ~/.znap/repos
fi

zstyle ':znap:*' repos-dir ~/.znap/repos

PURE_PROMPT_SYMBOL='›'
PURE_PROMPT_VICMD_SYMBOL='‹'

znap prompt minazukijiro/pure

znap source zsh-users/zaw

znap source zsh-users/zsh-autosuggestions

znap source zsh-users/zsh-completions

# znap source zsh-users/zsh-history-substring-search
# bindkey  history-substring-search-up
# bindkey  history-substring-search-down

znap source zsh-users/zsh-syntax-highlighting

#znap source sorin-ionescu/prezto modules/{command-not-found,completion}

: load other files

() {
    local src=$1 zwc=$1.zwc
    if [[ ! -f $zwc || $src -nt $zwc ]]; then
        zcompile $src
    fi
    source $src
} ~/.zshrc.*~*.zwc~*\~

: function and alias

export DOTFILES_GIT_DIR=~/.dotfiles
export DOTFILES_WORK_TREE=~

alias dotfiles='git --git-dir "$DOTFILES_GIT_DIR" --work-tree "$DOTFILES_WORK_TREE"'
compdef dotfiles=git

if [[ ! -d "$DOTFILES_GIT_DIR" ]]; then
    dotfiles init
    dotfiles remote add origin https://github.com/minazukijiro/dotfiles.git
    dotfiles fetch
    dotfiles reset --hard origin/main
fi

mkcd() { install -Dd "$1" && cd "$1" }

alias relogin='exec $SHELL -l'
alias ls='ls -Xv --color=auto --group-directories-first'

:

export GPG_TTY=$(tty)

: 3rd party tools

if (( $+commands[emacsclient] )); then
    alias emacs='emacsclient -a emacs -t'
fi

if (( $+commands[nnn] )); then
    export NNN_OPTS='aBdEfHoS'

    typeset -TUx NNN_PLUG nnn_plug ';'

    export NNN_FCOLORS='c1e2272e006033f7c6d6abc4'

    if [[ ! -f ~/.config/nnn/plugins/getplugs ]]; then
        rm -rf ~/.config/nnn/plugins
        curl -fsS https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | zsh
    fi
fi

if (( $+commands[pass] )); then
    export PASSWORD_STORE_ENABLE_EXTENSIONS=true
fi

if (( $+commands[syncthing] )); then
    znap eval syncthing 'syncthing install-completions'
fi

if (( $+commands[cargo] )); then
    path+=(~/.cargo/bin(N-/))
fi

if (( $ZPROF )); then
    zprof
fi
