# -*- mode: shell-script; -*-

if (( ${ZPROF:-0} )); then
    zmodload zsh/zprof; zprof
fi

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=0

typeset -U PATH path
path=(~/bin(N-/) ~/.local/bin(N-/) ~/.cargo/bin(N-/) $path)

typeset -U CDPATH cdpath
cdpath=(~)

typeset -U FPATH fpath
fpath=(~/.zsh.d/functions(N-/) $fpath)

[[ $TERM != linux ]] || return 0

# If not working
# echo 'AcceptEnv TMUX' | sudo tee /etc/ssh/sshd_config.d/90-tmux.conf
if (( $+commands[tmux] && ! $+TMUX && $+SSH_CONNECTION )); then
    tmux has && exec tmux attach
    exec tmux new
fi

ttyctl -f

reset_broken_terminal () {
    printf '%b' '\e[0m\e(B\e)0\017\e[?5l\e7\e[0;0r\e8'
}

precmd_functions+=(reset_broken_terminal)

clear-screen-and-scrollback() {
    printf '\x1Bc'; zle clear-screen
}

zle -N clear-screen-and-scrollback
bindkey ^L clear-screen-and-scrollback

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic
zstyle ':bracketed-paste-magic' paste-init backward-extend-paste

autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars ' -/?=:@'
zstyle ':zle:*' word-style unspecified

alias relogin='exec $SHELL -l'

setopt EXTENDED_GLOB
setopt NULL_GLOB

setopt BANG_HIST
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
setopt SHARE_HISTORY

SAVEHIST=$((100000 * 365))

# autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
# zle -N up-line-or-beginning-search
# bindkey  up-line-or-beginning-search
# zle -N down-line-or-beginning-search
# bindkey  down-line-or-beginning-search

alias history='fc -lDi'

typeset -TUx HISTORY_IGNORE history_ignore '|'
history_ignore+=('history' 'history *')

my_zshaddhistory() {
    [[ ${1%%$'\n'} != ${~ZSH_HISTORY_IGNORE} ]]
}

zshaddhistory_functions+=(my_zshaddhistory)

if (( $+commands[direnv] )); then
    eval "$(direnv hook zsh)" >/dev/null 2>&1
fi

dotfiles() {
    git --git-dir ~/.dotfiles --work-tree ~ "$@"
}

[[ -d ~/.dotfiles ]] || {
    dotfiles init
    dotfiles remote add origin git@github.com:minazukijiro/dotfiles.git
    dotfiles pull origin main
}

dotfiles-add() {
    dotfiles add "$@"
    dotfiles-commit "$@"
}

dotfiles-commit() {
    local v xy f parts  msg commit

    (
        cd ~

        IFS=$'\n'
        set -- $(dotfiles status -s -uno)

        while (( $# )); do
            v="$1"
            xy="$v[1,2]"
            f="$v[4,-1]"
            parts=(${(@s/ -> /)f})
            msg= commit=0
            case "$xy" in
                M[' 'MD]  ) commit=1 msg="update $f" ;;
                A[' 'MD]  ) commit=1 msg="add $f" ;;
                D' '      ) commit=0 msg='deleted from index' ;;
                R[' 'MD]  ) commit=1 msg="renamed $f" ;;
                C[' 'MD]  ) commit=0 msg='copied in index' ;;
                [MARC]' ' ) commit=0 msg='index and work tree matches' ;;
                [' 'MARC]M) commit=1 msg="update $f" ;;
                [' 'MARC]D) commit=0 msg='deleted in work tree' ;;
                [' 'D]R   ) commit=0 msg='renamed in work tree' ;;
                [' 'D]C   ) commit=0 msg='copied in work tree' ;;
                DD        ) commit=0 msg='unmerged, both deleted' ;;
                AU        ) commit=0 msg='unmerged, added by us' ;;
                UD        ) commit=0 msg='unmerged, deleted by them' ;;
                UA        ) commit=0 msg='unmerged, added by them' ;;
                DU        ) commit=0 msg='unmerged, deleted by us' ;;
                AA        ) commit=0 msg='unmerged, both added' ;;
                UU        ) commit=0 msg='unmerged, both modified' ;;
            esac
            if (( $commit )); then
                dotfiles commit -m "$msg" "$parts[@]"
            elif (( $#msg )); then
                echo dotfiles commit -m "$msg" "$parts[@]"
                # else
                #     echo "xy:$xy f:$f"
            fi
            shift
        done
    )
}

dotfiles-pull() {
    dotfiles fetch
    dotfiles reset --hard origin/main
}

emacs-or-client() {
    local c=()
    if emacsclient -e '(server-running-p)' >/dev/null 2>&1; then
        emacsclient -t "$@"
    else
        emacs "$@"
    fi
}

alias e='emacs-or-client'
alias emacs='emacs-or-client'

alias grep='grep --color=auto'

export GPG_TTY=$(tty)

export LESS='-RS'

if (( $+commands[lesspipe.sh] )); then
    export LESSOPEN='|lesspipe.sh %s'
fi

alias ls='ls -Xv --color=auto --group-directories-first'

mkcd() { install -Dd "$1" && cd "$1" }
mkcp() { (( $# > 1 )) && install -Dd "$@[-1]" && cp "$@" }
mkmv() { (( $# > 1 )) && install -Dd "$@[-1]" && mv "$@" }

if (( $+commands[nnn] )); then
    export NNN_OPTS='aBdfoS'
    export NNN_PLUG='f:finder;e:trash-empty'
    export NNN_FCOLORS='c1e2272e006033f7c6d6abc4'

    if [[ ! -f ~/.config/nnn/plugins/getplugs ]]; then
        curl -fsS https://raw.githubusercontent.com/jarun/nnn/master/plugins/getplugs | zsh
    fi

    if (( $+commands[trash-put] )); then
        export NNN_TRASH=1
        nnn() { command nnn "$@"; trash-empty -fv; }
    elif (( $+commands[gio] )); then
        export NNN_TRASH=2
        nnn() { command nnn "$@"; gio trash --empty; }
    fi
fi

[[ -f ~/.ssh-agent-thing ]] \
    || pkill -u $USER ssh-agent >/dev/null 2>&1

pgrep -u $USER ssh-agent >/dev/null 2>&1 \
    || ssh-agent > ~/.ssh-agent-thing

(( $+SSH_AGENT_PID )) \
    || eval $(< ~/.ssh-agent-thing) >/dev/null

ssh-add -l >/dev/null \
    || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'

# znap
() {
    znapdir=~/.znap znapzsh=~/.znap/znap.zsh
    [[ -r $znapzsh ]] || {
        rm -rf $znapdir
        git clone --depth 1 https://github.com/marlonrichert/zsh-snap.git $znapdir
    }
    source $znapzsh
    zstyle ':znap:*' repos-dir $znapdir/repos
}

PURE_PROMPT_SYMBOL='›'
PURE_PROMPT_VICMD_SYMBOL='‹'
znap prompt minazukijiro/pure

znap source zsh-users/zaw

znap source zsh-users/zsh-autosuggestions

znap source zsh-users/zsh-completions

znap source zsh-users/zsh-history-substring-search
bindkey  history-substring-search-up
bindkey  history-substring-search-down

znap source zsh-users/zsh-syntax-highlighting

znap source sorin-ionescu/prezto modules/{command-not-found,completion}

() {
    local src zwc
    while (( $# )); do
        src=$1 zwc=$1.zwc
        if [[ ! -f $zwc || $src -nt $zwc ]]; then
            zcompile $src
        fi
        source $src
        shift
    done
} ~/.zshrc.*~*.zwc~*~

if (( $ZPROF )); then
    zprof
fi
