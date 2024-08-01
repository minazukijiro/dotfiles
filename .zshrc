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
    local x y f msg commit
    
    git --git-dir ~/.dotfiles --work-tree ~ status -s | while read -r line; do
	x="$line[1]" y="$line[2]" f="$line[4,-1]"
	if [[ "$x$y" == '??' || "$x$y" == '!!' ]]; then
	    continue
	fi
	msg= commit=0
	case "$x$y" in
	    M[' 'MD]  ) commit=0 msg='updated in index' ;;
	    A[' 'MD]  ) commit=1 msg="add $f" ;;
	    D' '      ) commit=0 msg='deleted from index' ;;
	    R[' 'MD]  ) commit=0 msg='renamed in index' ;;
	    C[' 'MD]  ) commit=0 msg='copied in index' ;;
	    [MARC]' ' ) commit=0 msg='index and work tree matches' ;;
	    [' 'MARC]M) commit=0 msg='work tree changed since index' ;;
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
	fi
    done
}

alias e='emacsclient -t'
