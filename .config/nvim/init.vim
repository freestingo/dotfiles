let mapleader = " "

source $HOME/.config/nvim/vim-plug/plugins.vim
source $HOME/.config/nvim/plug-config/coc.vim
source $HOME/.config/nvim/plug-config/nvcode-color-schemes.vim
" source $HOME/.config/nvim/plug-config/gruvbox.vim
" source $HOME/.config/nvim/plug-config/ayu-vim.vim
source $HOME/.config/nvim/plug-config/telescope.vim
source $HOME/.config/nvim/plug-config/nvim-colorizer.vim
source $HOME/.config/nvim/plug-config/vim-airline.vim
source $HOME/.config/nvim/plug-config/indentLine.vim
source $HOME/.config/nvim/plug-config/vim-fugitive.vim
source $HOME/.config/nvim/plug-config/tig-explorer.vim
source $HOME/.config/nvim/plug-config/vim-highlightedyank.vim
source $HOME/.config/nvim/plug-config/vim-visual-multi.vim
source $HOME/.config/nvim/plug-config/language-client-neovim.vim
" enabling lspconfig.vim makes treesitter's syntax highlighting not work
" source $HOME/.config/nvim/plug-config/lspconfig.vim
luafile $HOME/.config/nvim/lua/freestingotreesitter.lua

" Sets

set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab
set smartindent
set exrc
set relativenumber
set nu
set nohlsearch
set hidden
set ignorecase
set smartcase
set noerrorbells
set nowrap
set incsearch
set termguicolors
set scrolloff=4
set noshowmode
set completeopt=menuone,noinsert,noselect
set signcolumn=yes
set cmdheight=2 "Give more space for displaying messages
set clipboard+=unnamedplus
set guicursor+=a:blinkon1
set nrformats=bin,hex,alpha
set lazyredraw
" set cursorline
filetype on

" https://www.reddit.com/r/neovim/comments/7kispa/how_does_gclipboard_work/
let g:clipboard = {
      \   'name': 'freestingoClipboard',
      \   'copy': {
      \      '+': 'xclip -selection clipboard',
      \      '*': 'xclip -selection clipboard',
      \   },
      \   'paste': {
      \      '+': 'xclip -selection clipboard -o',
      \      '*': 'xclip -selection clipboard -o',
      \   },
      \   'cache_enabled': 1,
      \ }

" Remaps

inoremap kj <ESC>
inoremap <C-j> <Esc>A;<Esc>o
inoremap <C-k><C-j> <Esc>A;<Esc>j^
inoremap <tab> <C-r>=SmartJumpToEnd()<CR>
nnoremap <C-j> o<Esc>
nnoremap <F4> :lua package.loaded.freestingotelescope = nil<CR>:lua package.loaded.freestingotreesitter =  nil<CR>:source ~/.config/nvim/init.vim<CR>
nnoremap <leader>ex :wincmd v <bar> :Ex <bar> :vertical resize 40<CR>
nnoremap <leader>sh :wincmd s <bar> :wincmd w <bar> :terminal<CR>
nnoremap <leader>ghci :wincmd s <bar> :wincmd w <bar> :terminal<CR>stack ghci<CR>
nnoremap <leader>bp :bprevious<CR>
nnoremap <leader>bn :bnext<CR>
nnoremap <leader>cl :so ~/.config/nvim/vim-plug/plugins.vim <bar> :PlugClean<CR>
nnoremap <leader>in :so ~/.config/nvim/vim-plug/plugins.vim <bar> :PlugInstall<CR>

nnoremap <leader>tj :call GotoBuffer(0)<CR>
nnoremap <leader>tk :call GotoBuffer(1)<CR>
nnoremap <leader>tl :call GotoBuffer(2)<CR>
nnoremap <leader>tò :call GotoBuffer(3)<CR>
nnoremap <leader>tsj :call SetBuffer(0)<CR>
nnoremap <leader>tsk :call SetBuffer(1)<CR>
nnoremap <leader>tsl :call SetBuffer(2)<CR>
nnoremap <leader>tsò :call SetBuffer(3)<CR>

nnoremap <leader>now "=strftime("%d-%m-%Y - %R")<CR>p

tnoremap shq <C-\><C-n>:bd!<CR>
tnoremap ghq :q<C-\><C-n>:bd!<CR>
tnoremap shw <C-\><C-n><C-o>

" Telescope remaps

source $HOME/.config/nvim/plug-config/telescope.vim

" Auto-commands

autocmd TermOpen * startinsert
autocmd FileType java setlocal tabstop=4 softtabstop=4 shiftwidth=4

source $HOME/.config/nvim/vim-functions.vim

augroup FREESTINGO
    autocmd!
    autocmd BufWritePre * :call TrimWhitespace()
augroup END
