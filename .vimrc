set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'wting/rust.vim'
Plugin 'dagwieers/asciidoc-vim'

call vundle#end()

filetype plugin indent on
syntax on

au BufNewFile,BufRead *.doc set filetype=asciidoc

