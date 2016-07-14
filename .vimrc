set nocompatible              " be iMproved
filetype off                  " required!

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
" required! 
Plugin 'VundleVim/Vundle.vim'

" Plugins
Plugin 'scrooloose/syntastic'
Plugin 'mjbrownie/browser.vim'
"Plugin 'hkrish/vimxcode'
"Plugin 'oplatek/Conque-Shell'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-fugitive'
" Plugin 'danchoi/vmail'
"Plugin 'git://repo.or.cz/vcscommand.git'
"Plugin 'jdonaldson/vaxe'
"Plugin 'godlygeek/csapprox'
Plugin 'junegunn/goyo.vim'
Plugin 'reedes/vim-pencil'
Plugin 'OmniSharp/omnisharp-vim'
Plugin 'tpope/vim-dispatch'

" vaxe
"let g:vaxe_cache_server = 1
"let g:vaxe_cache_server_enable = 1
set autowrite

if v:version > 703
	if !has('gui_running')
		Plugin 'Valloric/YouCompleteMe'
	end
	Plugin 'kien/ctrlp.vim'

	let g:ycm_confirm_extra_conf = 0
	let g:ycm_register_as_syntastic_checker = 1
	let g:syntastic_check_on_open = 1
	let g:syntastic_javascript_checkers = ['jshint']

	nnoremap <Leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
	nnoremap <Leader>n :lne<CR>
	nnoremap <Leader>p :lprevious<CR>

	nnoremap <Leader>t :tabnew<CR>
	nnoremap <Leader>v :vsp<CR>
	nnoremap <Leader>s :sp<CR>

	" CtrlP
	" let g:ctrlp_map = '<c-p>'
	let g:ctrlp_show_hidden = 1
	let g:ctrlp_max_files = 0

	" Conque-Shell
	let g:ConqueTerm_Color = 2
	let g:ConqueTerm_FastMode = 0
	let g:ConqueTerm_TERM = 'xterm-256color'
endif

call vundle#end()


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
set autoindent
set smartindent
set nowrap
set smartcase
set ignorecase
set backspace=2

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

" Pencil
let g:pencil#wrapModeDefault = 'soft'
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
  autocmd FileType text         call pencil#init()
augroup END
