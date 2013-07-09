set number

set nocompatible
filetype off

source $VIMRUNTIME/menu.vim 
set wildmenu                        
set cpoptions-=<
set wildcharm=<C-Z>
map <F4> :emenu <C-Z>

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" my bundles
Bundle 'rainbow_parentheses.vim'
Bundle 'slimv.vim'
Bundle 'ScrollColors'
Bundle 'plasticboy/vim-markdown'

filetype plugin indent on
syntax on

set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

" Toggle RainbowParenthesis on .lisp

augroup lisp
  autocmd BufNewFile,BufRead *.lisp,*.lsp,*.asd,*.hylas RainbowParenthesesToggle
  autocmd BufNewFile,BufRead *.lisp,*.lsp,*.asd,*.hylas set tabstop=4
  autocmd BufNewFile,BufRead *.lisp,*.lsp,*.asd,*.hylas set shiftwidth=4
augroup END

" Slimv

let g:slimv_swank_cmd = "! xterm -e sbcl --load /home/eudoxia/.vim/bundle/slimv.vim/slime/start-swank.lisp &"
let g:lisp_rainbow=1

