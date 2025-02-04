" Use Vim settings, rather then Vi settings.
" This must be first, because it changes other options and side effects.
set nocompatible

" Automatic syntax recognition
syntax enable

" Using vim with psql
au BufRead /tmp/psql.edit* set syntax=sql

" Font, encoding and colorscheme
set t_Co=256
set term=xterm
" set t_ut=""
set background=dark
"colorscheme solarized
set encoding=utf-8
"set guifont=Anonymous\ Pro:h11

" Tabulators
set softtabstop=4
set shiftwidth=4
"set expandtab

set noswapfile
"set relativenumber
set number

" ----------------------------------------------------------
" Status-line
" ----------------------------------------------------------
set laststatus=2        " Always display a statusline.
set noruler             " Since I'm using a statusline, disable ruler.
set statusline=%<%f                          " Path to the file in the buffer.
set statusline+=\ %h%w%m%r%k                 " Flags (e.g. [+], [RO]).
set statusline+=\ [%{(&fenc\ ==\"\"?&enc:&fenc).(&bomb?\",BOM\":\"\")},%{&ff}] " Encoding and line endings.
set statusline+=\ %y                         " File type.
set statusline+=\ [\%03.3b,0x\%02.2B,U+%04B] " Codes of the character under cursor.
set statusline+=\ [%l/%L\ (%p%%),%v]         " Line and column numbers.

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
" au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/
" highlight BadWhitespace ctermbg=red

" ----------------------------------------------------------
" C
" ----------------------------------------------------------
au BufNewFile,BufRead *.c,*.h
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix 

" Flagging unecessary white-spaces
" au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/
" highlight BadWhitespace ctermbg=red

" ----------------------------------------------------------
" MAP LEADER & KEYBOARD MAPPING
" ----------------------------------------------------------
"let mapleader = ","
let mapleader = "\<Space>"
noremap <leader>w :w<cr>


" Bind 'jj' to <esc> to jump out of insert mode
inoremap jj <ESC>


nmap <F3> i<C-R>=strftime("%d-%m-%Y %a %H:%M")<CR><Esc>
imap <F3> <C-R>=strftime("%d-%m-%Y %a %H:%M")<CR>

" QWERTY and Czech-chars

" delay with double-tap setup
set timeoutlen=240

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

"--------------------------------------------------------
" netrw: Network oriented reading, writing, and browsing.
"--------------------------------------------------------
" Disable the top banner.
let g:netrw_banner=0
" Tree-like view.
let g:netrw_liststyle=3
" Open splits to the right.
let g:netrw_altv=1
" Use a smaller window.
let g:netrw_winsize=25
" Do not perform any magic during sorting (like putting .h files together),
" except for listing directories first.
let g:netrw_sort_sequence='[\/]$'

