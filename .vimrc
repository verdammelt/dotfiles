set nocompatible    " IMproved!

source ~/.vim/vundle.vim

let mapleader=","
let maplocalleader=","

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
    set mouse=a
endif

set hidden " allow hidding buffers which are not saved

" Store temporary files in a central spot
set backupdir=~/.vim-tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,/var/tmp,/tmp

set shellslash  " change all slashes in completion to forward slash

set number          " line numbers
set numberwidth=4   " number of columns for line numbers

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set mousehide "hide the mouse while typeing
set scrolloff=5

set history=500		" keep 500 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" dealing with tabs - 4 spaces - expand them.
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab

" want to keep from writing lines which are too long no matter what I am doing
set textwidth=78

set formatoptions+=o
set autoindent " o and O auto indent
set smartindent " figure out proper indenting

set laststatus=2 " always show the status bar
set statusline=%<%f\ %-8(%y%m%r%)%=\ 0x%B\ %-20(%P\ %3l,%02c%03V%)
set showmatch " flash matching paren/bracket/etc

set title " show the title

set fileformat=unix

" go wild!
set wildmenu
set wildmode=longest,list
set wildignore+=*~
set wildignore+=*.class
set wildignore+=*.jar
set wildignore+=dist
set wildignore+=node_modules

" ignore case in patterns unless explicit capital used
set ignorecase
set smartcase
set infercase

" when changing put $ at end of changed area and overwrite as i type
set cpoptions+=$

set winwidth=80
" must set winheight before setting winminheight and it must be bigger than
" winminheight.  But setting it right away to 999 causes winminheight setting
" to fail.
set winheight=10
set winminheight=5
"set winheight=999 " current windo should fill 'most' of the space
autocmd VimEnter set winheight=999

set exrc
set secure

set shortmess=aTI

set complete+=kspell
set completeopt+=longest
set dictionary+=/usr/share/dict/words

" Switch syntax highlighting on, when the terminal has colors.
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
    set cursorline
    set colorcolumn=+1
    set background=light
    "colorscheme solarized
    "colorscheme desert
endif

if has("autocmd")
    filetype plugin indent on

    augroup vimrcEx
        au!

        autocmd BufRead,BufNewFile *.markdown set filetype=octopress
        autocmd BufRead,BufNewFile *.md set filetype=octopress
        autocmd BufRead,BufNewFile *.applescript set filetype=applescript

        " For all text files set 'textwidth' to 78 characters.
        autocmd FileType text setlocal spell
        autocmd FileType tex setlocal spell
        autocmd FileType markdown setlocal spell
        autocmd FileType gitcommit setlocal spell

        autocmd FileType help
            \ setlocal nospell |
            \ nmap <buffer> <CR> <C-]> |
            \ nmap <buffer> <BS> <C-T> 

        autocmd BufReadPost *
            \ if line("'\"") > 1 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

        " automatically source .vimrc when it is saved.
        autocmd BufWritePost .vimrc source $MYVIMRC
        autocmd BufWritePost vundle.vim source $MYVIMRC

    augroup END
endif " has("autocmd")

" %% defined to be absolute path to current file
cnoremap %% <C-R>=expand('%:p:h').'/'<cr>

map Q gq " Don't use Ex mode, use Q for formatting
map <leader><Space> :nohlsearch<cr>
map <leader>e :edit %%
map <leader>f :CtrlP<CR>
map <leader>b :CtrlPBuffer<CR>
map <leader>gb :Gblame<CR>
map <leader>gc :Gcommit<CR>
map <leader>gd :Gdiff<CR>
map <leader>gs :Gstatus<CR>
map <leader>r :wa<CR>:!rake<CR>
map <leader>v :edit $MYVIMRC<CR>
noremap \ ,
noremap <leader>y "*y
noremap <leader>yy "*Y
noremap <leader>p :set paste<CR>:put *<CR>:set nopaste<CR>
nnoremap <leader><leader> <c-^>

map <f1> <NOP>

" because I am INSANE!
nnoremap <leader>h <Esc>:call ToggleHardMode()<CR>

"function! CleverTab()
"	if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
"		return "\<Tab>"
"	else
"		return "\<C-N>"
"	endif
"endfunction
"inoremap <Tab> <C-R>=CleverTab()<CR>
"inoremap <s-tab> <C-P>

