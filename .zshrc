# -*- mode: shell-script; -*-

if (( ${ZPROF:-0} )); then
    zmodload zsh/zprof; zprof
fi

HISTFILE=/dev/null

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

# command not found
() {
    local handler
    if (( $+commands[pkgfile] )) \
	   && [[ -s ${handler::=/usr/share/doc/pkgfile/command-not-found.zsh} ]]; then
	source $handler
    elif (( $+commands[brew] )) \
	     && [[ -s ${handler::=${commands[brew]:A:h:h}/Library/Taps/homebrew/homebrew-command-not-found/handler.sh} ]]; then
	source $handler
    fi
}

alias dotfiles='git --git-dir ~/.dotfiles --work-tree ~'

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
    local v xy f msg commit

    IFS=$'\n'
    set -- $(dotfiles status -s -uno)
    
    while (( $# )); do
	v="$1"; xy="$v[1,2]" f="$v[4,-1]"
	msg= commit=0
	case "$xy" in
	    M[' 'MD]  ) commit=0 msg="update $f" ;;
	    A[' 'MD]  ) commit=1 msg="add $f" ;;
	    D' '      ) commit=0 msg='deleted from index' ;;
	    R[' 'MD]  ) commit=0 msg='renamed in index' ;;
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
	    dotfiles commit -m "$msg" "$f"
	elif (( $#msg )); then
	    echo "$msg"
	# else
	#     echo "xy:$xy f:$f"
	fi
	shift
    done
}

dotfiles-force-pull() {
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

mkcd() { install -Dd "$1" && cd "$1" }
mkcp() { (( $# > 1 )) && install -Dd "$@[-1]" && cp "$@" }
mkmv() { (( $# > 1 )) && install -Dd "$@[-1]" && mv "$@" }

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

znap source zsh-users/zaw

znap source zsh-users/zsh-autosuggestions

znap source zsh-users/zsh-completions

znap source zsh-users/zsh-history-substring-search
bindkey  history-substring-search-up
bindkey  history-substring-search-down

znap source zsh-users/zsh-syntax-highlighting

PURE_PROMPT_SYMBOL='›'
PURE_PROMPT_VICMD_SYMBOL='‹'
znap prompt sindresorhus/pure

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
} ~/.zshrc.*~*.zwc
