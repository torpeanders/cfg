" Basic Vim Configuration
set nocompatible             " Disable compatibility with old Vim versions
filetype off                 " Disable file type detection initially

" Determine the operating system
let g:os = substitute(system('uname'), '\n', '', '')

" Install vim-plug if it's not already installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" vim-plug initialization
call plug#begin('~/.vim/plugged')        " Start plugin section
Plug 'airblade/vim-gitgutter'            " Git change indicators
Plug 'ap/vim-buftabline'                 " Buffer tab line
Plug 'easymotion/vim-easymotion'         " Quick navigation
Plug 'itchyny/lightline.vim'             " Lightweight status line
Plug 'junegunn/fzf.vim'                  " Fuzzy finder integrations
Plug 'rafi/awesome-vim-colorschemes'     " Color schemes
Plug 'scrooloose/nerdcommenter'          " Commenting utility
Plug 'tpope/vim-fugitive'                " Git integration
Plug '~/.fzf'                            " Fuzzy finder core
call plug#end()                          " End plugin section

" General Settings
set breakindent                   " Enable break indent for better readability
set autoindent                    " Enable automatic indentation
set tabstop=4                     " Set tab width to 4 spaces
set shiftwidth=4                  " Indent by 4 spaces when using '>>' or '<<'
set expandtab                     " Convert tabs to spaces
set background=dark               " Set background color to dark
set noshowmode                    " Hide mode indicator
set autowrite                     " Automatically save when switching buffers
set hlsearch                      " Highlight search results
set wildmode=longest:full,full    " Improve command-line completion
set wildmenu                      " Enable command-line completion menu
set splitbelow                    " New horizontal splits open below
set splitright                    " New vertical splits open to the right
syntax on                         " Enable syntax highlighting

" Leader Key Configuration
let mapleader=","

" Set working directory to current file's directory
nnoremap <leader>. :lcd %:p:h<CR>

" Highlight current column for better visibility
highlight ColorColumn ctermbg=red    " Set background color for ColorColumn
call matchadd('ColorColumn', '\%81v', 100)

" Set color scheme
colorscheme twilight256

" vim-buftabline Settings
let g:buftabline_show = 2
let g:buftabline_numbers = 2

" lightline Settings
set laststatus=2

" EasyMotion Configuration
let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1
nmap s <Plug>(easymotion-overwin-f2)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" fzf Key Mappings
nnoremap <silent> <Leader>ff :Files<CR>
nnoremap <silent> <Leader>fg :Rg<CR>
nnoremap <silent> <Leader>fb :Buffers<CR>
nnoremap <silent> <Leader>fs :BLines<CR>
nnoremap <silent> <Leader>fc :Commits<CR>

" Adjust window sizes on resize
autocmd VimResized * wincmd =
