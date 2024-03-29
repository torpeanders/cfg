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
Plug 'ap/vim-buftabline'
Plug 'christoomey/vim-tmux-navigator'
Plug 'embear/vim-localvimrc'
Plug 'itchyny/lightline.vim'
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'scrooloose/nerdcommenter'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
Plug 'easymotion/vim-easymotion'
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
set splitbelow
set splitright

let mapleader=","

highlight ColorColumn ctermbg=red
call matchadd('ColorColumn', '\%81v', 100)

let g:localvimrc_ask = 0

" To fix that lightline doesn't show up
set laststatus=2

" vim-buftabline
let g:buftabline_show = 2       " Always show
let g:buftabline_numbers = 2    " Ordinal from left-to-right

" color scheme
colorscheme twilight256

" tmux
let g:tmux_navigator_disable_when_zoomed = 1
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <left> :TmuxNavigateLeft<cr>
nnoremap <silent> <down> :TmuxNavigateDown<cr>
nnoremap <silent> <up> :TmuxNavigateUp<cr>
nnoremap <silent> <right> :TmuxNavigateRight<cr>

" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

" Navigate tabs
nnoremap <C-N> :bnext<CR>
nnoremap <C-P> :bprev<CR>

" Navigate splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

nnoremap <silent> <C-Right> <c-w>l
nnoremap <silent> <C-Left> <c-w>h
nnoremap <silent> <C-Up> <c-w>k
nnoremap <silent> <C-Down> <c-w>j

""" fzf
nnoremap <silent> <C-f> :Files<Cr>
nnoremap <silent> <Leader>f :Rg<Cr>
nnoremap <silent> <Leader>s :BLines<Cr>
"nnoremap <silent> <Leader>g :Find<Cr>
nnoremap <silent> <Leader>c :Commits<Cr>
nnoremap <silent> <Leader>b :Buffers<CR>

""" EasyMotion
let g:EasyMotion_do_mapping = 0 " Disable default mappings

" `s{char}{char}{label}`
nmap s <Plug>(easymotion-overwin-f2)

" Turn on case-insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
""" misc
autocmd VimResized * wincmd =

""" show trailing whitespaces
"let &colorcolumn="80"
"set list
"set listchars=tab:▸\ ,trail:¬,nbsp:.,precedes:«,extends:»
"augroup ListChars2
"    au!
"    autocmd filetype go set listchars+=tab:\ \
"    autocmd ColorScheme * hi! link SpecialKey Normal
"augroup END
