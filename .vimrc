" Use Vim settings, rather then Vi settings.
" This must be first, because it changes other options and side effects.
set nocompatible

" Automatic syntax recognition
syntax enable

filetype off "required for Vundle

set noswapfile
set relativenumber
"set number

" ----------------------------------------------------------
" Plugin manager
" ----------------------------------------------------------
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'tmhedberg/SimpylFold'
Plugin 'vim-scripts/indentpython.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" ----------------------------------------------------------
" Python PEP 8
" ----------------------------------------------------------
au BufNewFile,BufRead *.py
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix 

" Flagging unecessary white-spaces
au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/
highlight BadWhitespace ctermbg=red

" ----------------------------------------------------------
" MAP LEADER
" ----------------------------------------------------------
"let mapleader = ","
let mapleader = "\<Space>"
noremap <leader>w :w<cr>

nnoremap ; :

" Bind 'jj' to <esc> to jump out of insert mode
inoremap jj <ESC>

" Font, encoding and colorscheme
set background=dark
"colorscheme solarized
set encoding=utf-8
"set guifont=Anonymous\ Pro:h11

nmap <F3> i<C-R>=strftime("%d-%m-%Y %a %H:%M")<CR><Esc>
imap <F3> <C-R>=strftime("%d-%m-%Y %a %H:%M")<CR>

" ----------------------------------------------------------
" enable folding like in ide
" ----------------------------------------------------------
set foldmethod=indent
set foldlevel=99
" Enables folding with spacebar
nnoremap <space> za

" ----------------------------------------------------------
" Commenting blocks of code.
" ----------------------------------------------------------
augroup commenting_blocks_of_code
  autocmd!
  autocmd FileType c,cpp,java,scala let b:comment_leader = '// '
  autocmd FileType sh,ruby,python   let b:comment_leader = '# '
  autocmd FileType conf,fstab       let b:comment_leader = '# '
  autocmd FileType tex              let b:comment_leader = '% '
  autocmd FileType mail             let b:comment_leader = '> '
  autocmd FileType vim              let b:comment_leader = '" '
augroup END
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cv :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>

" ----------------------------------------------------------
" Set 'Yank' to go in systm Clipboard and Ctrl-C to Vim 'p' command
" ----------------------------------------------------------
set clipboard=unnamed

" WSL2 clipboard support
" ???


" ----------------------------------------------------------
" Tmux & Vim panel switching integration
" https://www.codeography.com/2013/06/19/navigating-vim-and-tmux-splits
" ----------------------------------------------------------
if exists('$TMUX')
function! TmuxOrSplitSwitch(wincmd, tmuxdir)
let previous_winnr = winnr()
silent! execute "wincmd " . a:wincmd
if previous_winnr == winnr()
call system("tmux select-pane -" . a:tmuxdir)
redraw!
endif
endfunction

let previous_title = substitute(system("tmux display-message -p '#{pane_title}'"), '\n', '', '')
let &t_ti = "\<Esc>]2;vim\<Esc>\\" . &t_ti
let &t_te = "\<Esc>]2;". previous_title . "\<Esc>\\" . &t_te

nnoremap <silent> <C-h> :call TmuxOrSplitSwitch('h', 'L')<cr>
nnoremap <silent> <C-j> :call TmuxOrSplitSwitch('j', 'D')<cr>
nnoremap <silent> <C-k> :call TmuxOrSplitSwitch('k', 'U')<cr>
nnoremap <silent> <C-l> :call TmuxOrSplitSwitch('l', 'R')<cr>
else
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
endif
