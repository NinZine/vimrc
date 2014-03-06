set nocompatible              " be iMproved
filetype off                  " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" Bundles
Bundle 'scrooloose/syntastic'
Bundle 'mjbrownie/browser.vim'
Bundle 'hkrish/vimxcode'
Bundle 'oplatek/Conque-Shell'
" Bundle 'tomasr/molokai'
Bundle 'tpope/vim-fugitive'
" Bundle 'danchoi/vmail'
Bundle 'git://repo.or.cz/vcscommand.git'
Bundle 'jdonaldson/vaxe'

" vaxe
let g:vaxe_cache_server = 1
let g:vaxe_cache_server_enable = 1
set autowrite

if v:version > 703
	Bundle 'Valloric/YouCompleteMe'
	Bundle 'kien/ctrlp.vim'

	let g:ycm_confirm_extra_conf = 0
	let g:ycm_register_as_syntastic_checker = 1
	let g:syntastic_check_on_open = 1
	let g:syntastic_javascript_checkers = ['jshint']

	nnoremap <Leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
	nnoremap <Leader>n :lne<CR>
	nnoremap <Leader>p :lprevious<CR>

	" CtrlP
	let g:ctrlp_map = '<Leader>t'
	let g:ctrlp_show_hidden = 1
	let g:ctrlp_max_files = 0

	" Conque-Shell
	let g:ConqueTerm_Color = 2
	let g:ConqueTerm_FastMode = 0
	let g:ConqueTerm_TERM = 'xterm-256color'
endif


" Other settings
filetype plugin indent on
"filetype plugin on

"set lines=47
"set columns=80
set tw=80
set ts=4
set smarttab
set softtabstop=4
set noexpandtab
set ai
set bg=dark
set nonumber
set ruler
set showcmd
set laststatus=2
set guioptions=gmrLtT
"set formatoptions=tcqan
set formatoptions=clrq
set shiftwidth=4
set ignorecase
set autoindent
set smartindent
set nowrap
set smartcase

autocmd FileType c set omnifunc=ccomplete#Complete
"set fuoptions=maxvert,maxhorz

colorscheme molokai
if has('gui_running')
	set gfn=Monaco:h10.00
endif

let filetype_m='objc'

au BufNewFile,BufRead *.mxml set filetype=mxml
au BufNewFile,BufRead *.as set filetype=actionscript

au BufNewFile,BufRead *.ftl set filetype=html
"au BufNewFile,BufRead *.css set filetype=less
autocmd BufNewFile,BufRead *.json set ft=javascript

"
"autocmd FileType java setlocal omnifunc=javacomplete#Complete
"autocmd FileType java setlocal completefunc=javacomplete#CompleteParamsInfo
"set makeprg=ant\ -f\ build.xml


syntax on
"command CDC cd %:p:h
