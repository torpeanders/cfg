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
Plug 'easymotion/vim-easymotion'
Plug 'embear/vim-localvimrc'
Plug 'itchyny/lightline.vim'
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'scrooloose/nerdcommenter'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
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
nnoremap <silent> <Leader>g :Find<Cr>
nnoremap <silent> <Leader>c :Commits<Cr>
nnoremap <silent> <Leader>b :Buffers<CR>

""" easymotion

" Move to line
map <Leader><Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader><Leader>L <Plug>(easymotion-overwin-line)

" Gif config
map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

" These `n` & `N` mappings are options. You do not have to map `n` & `N` to EasyMotion.
" Without these mappings, `n` & `N` works fine. (These mappings just provide
" different highlight method and have some other features )
map  n <Plug>(easymotion-next)
map  N <Plug>(easymotion-prev)

" Gif config
nmap s <Plug>(easymotion-s2)
nmap t <Plug>(easymotion-t2)

""" misc
autocmd VimResized * wincmd =

""" show trailing whitespaces
let &colorcolumn="80"
set list
set listchars=tab:▸\ ,trail:¬,nbsp:.,precedes:«,extends:»
augroup ListChars2
    au!
    autocmd filetype go set listchars+=tab:\ \ 
    autocmd ColorScheme * hi! link SpecialKey Normal
augroup END
