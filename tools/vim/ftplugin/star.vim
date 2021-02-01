" Vim filetype plugin file
" Language: Star

if exists("b:did_ftplugin")
	finish
endif
let b:did_ftplugin = 1

let s:save_cpo = &cpo
set cpo-=C

setlocal suffixesadd=.star

let b:star_vim_loaded = 1

let b:undo_ftplugin = "setlocal suffixesadd< | unlet! b:star_vim_loaded"

let &cpo = s:save_cpo
unlet s:save_cpo
