"dein Scripts-----------------------------                                           
if &compatible                                                                       
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.dein.vim/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state(expand('~/.dein.vim'))
  call dein#begin(expand('~/.dein.vim'))

  " Let dein manage dein
  " Required:
  call dein#add('Shougo/dein.vim')

  " Add or remove your plugins here:                                                 
  call dein#add('Shougo/neosnippet.vim')                                             
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('kristijanhusak/vim-hybrid-material')

  " You can specify revision/branch/tag.
  call dein#add('Shougo/vimshell', { 'rev': '3787e5' })                              
                                                                                     
  " Required:                                                                        
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable
                                                                                     
" If you want to install not installed plugins on startup.                           
if dein#check_install()
  call dein#install()                                                               
endif                                                                               

"End dein Scripts-------------------------

" Vim Settings

set t_Co=256
set background=dark
