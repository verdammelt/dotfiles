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

"set clipboard=unnamed " work with OS clipboard 

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

" dealing with tabs - 4 spaces - exapnd them.
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

" ignore case in patterns unless explicit capital used
set ignorecase
set smartcase

" when changing put $ at end of changed area and overwrite as i type
set cpoptions+=$

set winwidth=80
" must set winheight before setting winminheight and it must be bigger than
" winminheight.  But setting it right away to 999 causes winminheight setting
" to fail.
set winheight=10
set winminheight=5
set winheight=999 " current windo should fill 'most' of the space

" Don't use Ex mode, use Q for formatting
map Q gq

" Switch syntax highlighting on, when the terminal has colors.
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
    set cursorline
    set colorcolumn=+1
    set background=dark
    colorscheme grb256
    " colorscheme solarized
    "colorscheme zenburn
endif

function! mjs:textish_mode()
    setlocal spell spelllang=en_us
endfunction

" Only do this part when compiled with support for autocommands.
if has("autocmd")
    " Enable file type detection.
    " Use the default filetype settings, so that mail gets 'tw' set to 72,
    " 'cindent' is on in C files, etc.
    " Also load indent files, to automatically do language-dependent
    " indenting.
    filetype plugin indent on

    " Put these in an autocmd group, so that we can delete them easily.
    augroup vimrcEx
        au!

        autocmd BufRead,BufNewFile *.markdown set filetype=octopress
        autocmd BufRead,BufNewFile *.md set filetype=octopress

        " For all text files set 'textwidth' to 78 characters.
        autocmd FileType text call mjs:textish_mode()
        autocmd FileType tex call mjs:textish_mode()
        autocmd FileType markdown call mjs:textish_mode()

        autocmd FileType help
            \ setlocal nospell |
            \ nmap <buffer> <CR> <C-]> |
            \ nmap <buffer> <BS> <C-T> 

        " When editing a file, always jump to the last known cursor position.
        " Don't do it when the position is invalid or when inside an event
        " handler (happens when dropping a file on gvim).  Also don't do it
        " when the mark is in the first line, that is the default position
        " when opening a file.
        autocmd BufReadPost *
            \ if line("'\"") > 1 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif

        " automatically source .vimrc when it is saved.
        autocmd BufWritePost .vimrc source $MYVIMRC

    augroup END
endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.  Only define it when not
" defined already.
if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
                \ | wincmd p | diffthis
endif

" %% defined to be absolute path to current file
cnoremap %% <C-R>=expand('%:p:h').'/'<cr>

nnoremap <CR> :nohlsearch<cr>
map <leader>e :edit %%
map <leader>f :CommandT<CR>
map <leader>F :CommandTFlush<CR>
map <leader>gb :Gblame<CR>
map <leader>gc :Gcommit<CR>
map <leader>gd :Gdiff<CR>
map <leader>gs :Gstatus<CR>
map <leader>r :wa<CR>:!rake<CR>
map <leader>v :edit $MYVIMRC<CR>
nnoremap <leader><leader> <c-^>

" because I am INSANE!
nnoremap <leader>h <Esc>:call ToggleHardMode()<CR>

function! g:ToggleNuMode() 
    if(&rnu == 1) 
        set nu 
    else 
        set rnu 
    endif 
endfunc 
nnoremap <C-L> :call g:ToggleNuMode()<cr> 

function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

set exrc
set secure

set shortmess=aTI
