set nocompatible
filetype off

let g:os = substitute(system('uname'), '\n', '', '')

" Install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" vim-plug init code
call plug#begin('~/.vim/plugged')
Plug 'itchyny/lightline.vim'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive', { 'on': 'Gstatus' }
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'embear/vim-localvimrc'
Plug 'christoomey/vim-tmux-navigator'
Plug 'rafi/awesome-vim-colorschemes'
call plug#end()

set breakindent
set autoindent
set tabstop=4
set shiftwidth=4
set expandtab
set background=dark
set noshowmode
set autowrite
set hlsearch
"set incsearch
syntax on
set wildmode=longest:full,full
set wildmenu

let mapleader=","

" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

" To fix that lightline doesn't show up
set laststatus=2

let g:sierra_Twilight = 1
colorscheme sierra

highlight ColorColumn ctermbg=red
call matchadd('ColorColumn', '\%81v', 100)

nnoremap <silent> <expr> <Leader><Leader> (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"
nnoremap <silent> <Leader><Enter> :Buffers<CR>

let g:localvimrc_ask = 0

let g:tmux_navigator_disable_when_zoomed = 1
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <left> :TmuxNavigateLeft<cr>
nnoremap <silent> <down> :TmuxNavigateDown<cr>
nnoremap <silent> <up> :TmuxNavigateUp<cr>
nnoremap <silent> <right> :TmuxNavigateRight<cr>

nnoremap <C-p> :Files<Cr>
nnoremap <C-t> :Files<Cr>

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --hidden --ignore-case --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview({'options': '--delimiter : --nth 4..'}, 'up:60%')
  \           : fzf#vim#with_preview({'options': '--delimiter : --nth 4..'}, 'right:50%:hidden', '?'),
  \   <bang>0)

nnoremap <C-f> :Rg<Cr>
nnoremap <C-g> :Rg<Cr>

autocmd VimResized * wincmd =
