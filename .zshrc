# -*- mode: shell-script; -*-
   
alias relogin='exec $SHELL -l'

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
    set -- $(dotfiles status -s)
    
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

alias e='emacsclient -t'
