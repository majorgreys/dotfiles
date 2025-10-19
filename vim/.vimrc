" Minimal vim configuration

" Compatibility
if &compatible
  set nocompatible
endif

" Enable filetype detection and syntax
filetype plugin indent on
syntax enable

" Vim Settings

set t_Co=256
set background=dark

" Add format option 'w' to add trailing white space, indicating that paragraph
" continues on next line. This is to be used with mutt's 'text_flowed' option.
augroup mail_trailing_whitespace " {
    autocmd!
    autocmd FileType mail setlocal formatoptions+=w
augroup END " }
