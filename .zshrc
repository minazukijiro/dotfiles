if (( ${DEBUG:-0} )); then
    ZPROF=1
    set -x
fi

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

: load prezto/comp

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

znap install zsh-users/zsh-completions

# znap source zsh-users/zsh-history-substring-search
# bindkey  history-substring-search-up
# bindkey  history-substring-search-down

znap source zsh-users/zsh-syntax-highlighting

znap source sorin-ionescu/prezto modules/{command-not-found,completion}

#if (( ! $+commands[asdf] )); then
#    curl -fsL https://github.com/asdf-vm/asdf/releases/download/v0.16.7/asdf-v0.16.7-$(uname -s | tr A-Z a-z)-$(arch).tar.gz | tar xzf - -C ~/bin asdf
#fi

#znap fpath _asdf 'command asdf completion zsh'
#path=(${ASDF_DATA_DIR:-$HOME/.asdf}/shims $path)

: load other files

() {
    local src=$1 zwc=$1.zwc
    [[ -n $src ]] || return 0
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
    set +x
    zprof
fi
