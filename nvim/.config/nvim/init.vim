call plug#begin('~/.vim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'dylanaraps/wal.vim'
Plug 'itchyny/lightline.vim'
Plug 'mhinz/vim-signify'
Plug 'kovetskiy/sxhkd-vim'
Plug 'ObserverOfTime/coloresque.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'vimwiki/vimwiki'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'ying17zi/vim-live-latex-preview'
Plug 'alvan/vim-closetag'
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'alaviss/nim.nvim'
Plug 'ChesleyTan/wordCount.vim'
Plug 'tpope/vim-fugitive'
call plug#end()

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0

autocmd BufNewFile,BufRead *.rasi set syntax=css

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif

set mouse=a
set tabstop=4
set shiftwidth=4
set expandtab
set updatetime=100
set number
set relativenumber
set clipboard=unnamedplus
set noshowmode

colorscheme wal
let g:lightline = {
      \ 'colorscheme': 'wal',
      \ }

highlight Normal ctermbg=none
