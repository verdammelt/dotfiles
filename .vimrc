call pathogen#infect()
call pathogen#helptags()

let mapleader=","

" Store temporary files in a central spot
set backupdir=~/.vim-tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,/var/tmp,/tmp

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

set shellslash

set clipboard=unnamed

set number
set numberwidth=5

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set visualbell
set mousehide
set scrolloff=5

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif

set history=500		" keep 500 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching
set cmdwinheight=5
set cmdheight=1

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set autoindent

set laststatus=2
set showmatch

set hidden

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  set cursorline
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  au BufRead,BufNewFile *.txt		setfiletype text
  autocmd FileType text 
    \ setlocal textwidth=78 |
    \ setlocal spell spelllang=en_us

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  " automatically source .vimrc when it is saved.
  autocmd BufWritePost .vimrc source $MYVIMRC

  augroup END

else
    set autoindent		" always set autoindenting on
endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

set wildmenu
set wildignore+=*~
set wildmode=longest,list
set ignorecase
set smartcase

set statusline=%<%f\ (%{&ft})\ %-8(%m%r%)%=%-19(%3l,%02c%03V%)

set t_Co=256
set background=dark
"colorscheme solarized
"colorscheme zenburn
set cursorline

set cpoptions+=$


:nnoremap <CR> :nohlsearch<cr>

" Remap the tab key to do autocompletion or indentation depending on the
" context (from http://www.vim.org/tips/tip.php?tip_id=102)
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

augroup myfiletypes
    "clear old autocmds in group
    autocmd!
    "for ruby, autoindent with two spaces, always expand tabs
    autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,cucumber set ai sw=2 sts=2 et
augroup END

" Map ,e and ,v to open files in the same directory as the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>
nmap <silent> <Leader>t :CommandT<CR>

map <leader>e :edit %%
map <leader>f :CommandT<CR>
map <leader>gb :Gblame<CR>
map <leader>gc :Gcommit<CR>
map <leader>gd :Gdiff<CR>
map <leader>gs :Gstatus<CR>
map <leader>r :wa<CR>:!rake<CR>
"map <leader>t :wa<CR>:!./script/test %<CR>
map <leader>v :edit $MYVIMRC<CR>

set winwidth=84
" must set winheight before setting winminheight and it must be bigger than
" winminheight.  But setting it right away to 999 causes winminheight setting
" to fail.
set winheight=5
set winminheight=5
set winheight=999 " current windo should fill 'most' of the space

nnoremap <leader><leader> <c-^>

set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

function! g:ToggleNuMode() 
  if(&rnu == 1) 
    set nu 
  else 
    set rnu 
  endif 
endfunc 

nnoremap <C-L> :call g:ToggleNuMode()<cr> 

let ruby_space_errors = 1
let ruby_operators = 1

function! RunTests(filename)
    " Write the file and run tests for the given filename
    :w
    :silent !echo;echo;echo;echo;echo
    if match(a:filename, '\.feature$') != -1
        exec ":!bundle exec cucumber " . a:filename
    else
        if filereadable("script/test")
            exec ":!script/test " . a:filename
        else
            exec ":!bundle exec rspec " . a:filename
        end
    end
endfunction

function! SetTestFile()
    " Set the spec file that tests will be run for.
    let t:grb_test_file=@%
endfunction

function! RunTestFile(...)
    if a:0
        let command_suffix = a:1
    else
        let command_suffix = ""
    endif

    " Run the tests for the previously-marked file.
    let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\)$') != -1
    if in_test_file
        call SetTestFile()
    elseif !exists("t:grb_test_file")
        return
    end
    call RunTests(t:grb_test_file . command_suffix)
endfunction

function! RunNearestTest()
    let spec_line_number = line('.')
    call RunTestFile(":" . spec_line_number)
endfunction

map <leader>t :call RunTestFile()<cr>
map <leader>T :call RunNearestTest()<cr>
map <leader>a :call RunTests('spec')<cr>
map <leader>c :w\|:!cucumber<cr>
map <leader>C :w\|:!cucumber --profile wip<cr>

set title
set smartindent
set smarttab
set magic
set bs=indent,eol,start
set fileformat=unix

