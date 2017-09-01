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
  call dein#add('reedes/vim-pencil')
  call dein#add('reedes/vim-colors-pencil')
  call dein#add('reedes/vim-lexical')
  call dein#add('reedes/vim-litecorrect')
  call dein#add('reedes/vim-textobj-quote')
  call dein#add('reedes/vim-textobj-sentence')
  call dein#add('junegunn/goyo.vim')
  call dein#add('junegunn/limelight.vim')
  call dein#add('itchyny/lightline.vim')
  call dein#add('itchyny/vim-gitbranch')

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

" Neovim Settings

execute "set t_8f=\e[38;2;%lu;%lu;%lum"
execute "set t_8b=\e[48;2;%lu;%lu;%lum"
set t_Co=256
set background=dark

" Lightline
let g:lightline = {
      \ 'colorscheme': 'solarized',
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
  autocmd FileType markdown,mkd call pencil#init()
                            \ | call litecorrect#init()
                            \ | setl spell spl=en_us fdl=4 noru nonu nornu
                            \ | setl spellfile=$HOME/.janus/en.utf-8.add
                            \ | setl fdo+=search
  autocmd FileType text         call pencil#init()
augroup END

" Stop folding
set nofoldenable

" Goyo
function! s:goyo_enter()
  if has('gui_running')
    set fullscreen
    set linespace=7
  endif
  colorscheme pencil
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
endfunction

function! s:goyo_leave()
  if has('gui_running')
    set nofullscreen
    set linespace=0
  endif
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
endfunction

let g:goyo_margin_top=1
let g:goyo_margin_bottom=1

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()
