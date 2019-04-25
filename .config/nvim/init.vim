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

  " general vim
  call dein#add('Shougo/unite.vim')
  call dein#add('Shougo/neosnippet.vim')                                             
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('Shougo/vimfiler.vim')
  call dein#add('itchyny/lightline.vim')
  call dein#add('junegunn/fzf', { 'build': './install --all', 'merged': 0 }) 
  call dein#add('junegunn/fzf.vim', { 'depends': 'fzf' })
  call dein#add('Shougo/vimshell', { 'rev': '3787e5' })                              
  call dein#add('Shougo/deoplete.nvim')
  call deoplete#enable()
  call dein#add('roxma/nvim-yarp')
  call dein#add('roxma/vim-hug-neovim-rpc')
  " language support
  call dein#add('Quramy/tsuquyomi')
  call dein#add('leafgarland/typescript-vim')
  " markdown editing
  call dein#add('reedes/vim-pencil')
  call dein#add('reedes/vim-colors-pencil')
  call dein#add('reedes/vim-lexical')
  call dein#add('reedes/vim-litecorrect')
  call dein#add('reedes/vim-textobj-quote')
  call dein#add('reedes/vim-textobj-sentence')
  call dein#add('junegunn/goyo.vim')
  call dein#add('junegunn/limelight.vim')
  " color themes
  call dein#add('NLKNguyen/papercolor-theme')
  call dein#add('kristijanhusak/vim-hybrid-material')
  call dein#add('arcticicestudio/nord-vim')
  call dein#add('lambdalisue/vim-fullscreen')
  call dein#add('itchyny/vim-gitbranch')
                                                                                     
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

" Personal Settings

let mapleader = ','

execute "set t_8f=\e[38;2;%lu;%lu;%lum"
execute "set t_8b=\e[48;2;%lu;%lu;%lum"
set t_Co=256
set background=dark
colorscheme nord

set tabstop=4       " The width of a TAB is set to 4.
                    " Still it is a \t. It is just that
                    " Vim will interpret it to be having
                    " a width of 4.

set shiftwidth=4    " Indents will have a width of 4

set softtabstop=4   " Sets the number of columns for a TAB

set expandtab       " Expand TABs to spaces

" vimfiler as default
let g:vimfiler_as_default_explorer = 1

" Lightline
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }

" Pencil
let g:pencil#conceallevel = 0
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init({'wrap': 'soft'})
                            \ | call litecorrect#init()
                            \ | setl spell spl=en_us fdl=4 noru nonu nornu
                            \ | setl fdo+=search
  autocmd Filetype git,gitsendemail,*commit*,*COMMIT*
                            \   call pencil#init({'wrap': 'hard', 'textwidth': 72})
                            \ | call litecorrect#init()
                            \ | setl spell spl=en_us et sw=2 ts=2 noai
  autocmd Filetype mail         call pencil#init({'wrap': 'hard', 'textwidth': 60})
                            \ | call litecorrect#init()
                            \ | setl spell spl=en_us et sw=2 ts=2 noai nonu nornu
  autocmd Filetype html,xml     call pencil#init({'wrap': 'soft'})
                            \ | call litecorrect#init()
                            \ | setl spell spl=en_us et sw=2 ts=2
augroup END

" Stop folding
set nofoldenable

" Goyo
function! s:goyo_enter()
  colorscheme pencil
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
endfunction

function! s:goyo_leave()
  colorscheme hybrid_reverse
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
endfunction


let g:goyo_margin_top=1
let g:goyo_margin_bottom=1

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" Clipboard

set clipboard+=unnamedplus

" Neovim Terminal

:tnoremap <Esc> <C-\><C-n>

" Add format option 'w' to add trailing white space, indicating that paragraph
" continues on next line. This is to be used with mutt's 'text_flowed' option.
augroup mail_trailing_whitespace " {
    autocmd!
    autocmd FileType mail setlocal formatoptions+=w
augroup END " }
