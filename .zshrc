# -*- mode: shell-script; -*-

if (( ${ZPROF:-0} )); then
    zmodload zsh/zprof; zprof
fi

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=0

typeset -U PATH path
path=(~/bin(N-/) ~/.local/bin(N-/) $path)

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
zstyle :bracketed-paste-magic paste-init backward-extend-paste

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

alias dotfiles='git --git-dir ~/.dotfiles --work-tree ~'

autoload -Uz compinit
compinit

compdef dotfiles=git

[[ -d ~/.dotfiles ]] || {
    dotfiles init
    dotfiles remote add origin https://github.com/minazukijiro/dotfiles.git
    dotfiles fetch
    dotfiles reset --hard origin/main
}

alias e='emacsclient -a emacs -t'
alias emacs='emacsclient -a emacs -t'

alias grep='grep --color=auto'

export GPG_TTY=$(tty)

# unset SSH_AGENT_PID
# if [[ "${gnupg_SSH_AUTH_SOCK_by:-0}" != $$ ]]; then
#     export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
# fi

# ssh-add -l >/dev/null \
#     || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'

export LESS='-RS'

if (( $+commands[lesspipe.sh] )); then
    export LESSOPEN='|lesspipe.sh %s'
fi

alias ls='ls -Xv --color=auto --group-directories-first'

mkcd() { install -Dd "$1" && cd "$1" }
mkcp() { (( $# > 1 )) && install -Dd "$@[-1]" && cp "$@" }
mkmv() { (( $# > 1 )) && install -Dd "$@[-1]" && mv "$@" }

urlencode() {
    LC_COLLATE=C
    local l=$#1 i c
    for (( i = 0; i <= l; i++ )); do
        c="${1:$i:1}"
        case "$c" in
            [a-zA-Z0-9.~_-]) printf '%c' "$c" ;;
            *) printf '%%%02X' "'$c" ;;
        esac
    done
}

urldecode() { : "${*//+/ }"; echo -e "${_//\%/\\x}" }

export EDITOR='vi'

alias cp='cp -v'
alias mv='mv -v'

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

if (( $+commands[pass] )); then
    export PASSWORD_STORE_ENABLE_EXTENSIONS=true
fi

# znap
() {
    znapdir=~/.znap znapzsh=~/.znap/znap.zsh
    [[ -r $znapzsh ]] || {
        rm -rf $znapdir
        git clone --depth 1 https://github.com/marlonrichert/zsh-snap.git $znapdir
    }
    source $znapzsh
    zstyle ':znap:*' repos-dir $znapdir/repos
    zstyle ':znap:*:*' git-maintenance off
}

PURE_PROMPT_SYMBOL='›'
PURE_PROMPT_VICMD_SYMBOL='‹'
znap prompt minazukijiro/pure

znap source zsh-users/zaw

znap source zsh-users/zsh-autosuggestions

znap install zsh-users/zsh-completions

znap source zsh-users/zsh-history-substring-search
bindkey  history-substring-search-up
bindkey  history-substring-search-down

znap source zsh-users/zsh-syntax-highlighting

znap source sorin-ionescu/prezto modules/{command-not-found,completion}

znap source asdf-vm/asdf asdf.sh
fpath+=(~[asdf]/completions)

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

if (( $+commands[mise] )); then
    znap eval mise 'mise activate zsh'
fi

if (( $+commands[aws] )); then
    zstyle ":prompt:pure:aws" show yes

    autoload bashcompinit && bashcompinit
    autoload -Uz compinit && compinit
    complete -C $(command -v aws_completer) aws
fi

if (( $+commands[aws-vault] && $+commands[pass] )); then
    export AWS_VAULT_BACKEND=pass
    export AWS_VAULT_PASS_PASSWORD_STORE_DIR=~/.password-store
    export AWS_VAULT_PASS_PREFIX=aws-vault
fi

    export AWS_VAULT_BACKEND=pass
    export AWS_VAULT_PASS_PASSWORD_STORE_DIR=~/.password-store
    export AWS_VAULT_PASS_PREFIX=aws-vault


if (( $+commands[kubectl] )); then
    zstyle ":prompt:pure:k8s" show yes

    znap fpath _kubectl 'kubectl completion zsh'

    zaw-src-kubectl-get-context() {
        candidates=(${(@f)"$(kubectl config get-contexts --no-headers -o name)"})
        actions=(zaw-callback-kubectl-use-context)
        act_descriptions=("kubectl use-context for zaw")
    }

    zaw-callback-kubectl-use-context() {
        kubectl config use-context "$1"
        zle accept-line
    }

    zaw-register-src -n kubectl-get-context zaw-src-kubectl-get-context

    bindkey  zaw-kubectl-get-context
fi

if (( $+commands[helm] )); then
    znap fpath _helm 'helm completion zsh'
fi

if (( $+commands[hugo] )); then
    znap fpath _hugo 'hugo completion zsh'
fi

if (( $+commands[gh] )); then
    znap fpath _gh 'gh completion -s zsh'
fi

if (( $+commands[syncthing] )); then
    znap eval syncthing 'syncthing install-completions'
fi

if (( $+commands[cargo] )); then
    path+=(~/.cargo/bin(N-/))
fi

if (( $+commands[gcloud] )); then
    if (( $+commands[mise] )); then
        source ~/.local/share/mise/installs/gcloud/latest/completion.zsh.inc
    fi
fi

if (( $+commands[clusteradm] )); then
    znap eval clusteradm 'clusteradm completion zsh'
fi

if (( $ZPROF )); then
    zprof
fi
