" Use Vim settings, rather then Vi settings.
" This must be first, because it changes other options and side effects.
set nocompatible

" Automatic syntax recognition
syntax enable

filetype plugin indent on

set noswapfile
set relativenumber
"set number

" MAP LEADER
"let mapleader = ","
let mapleader = "\<Space>"
noremap <leader>w :w<cr>

nnoremap ; :

" Bind 'jj' to <esc> to jump out of insert mode
inoremap jj <ESC>

" Font, encoding and colorscheme
set background=dark
colorscheme solarized
set encoding=utf-8
"set guifont=Anonymous\ Pro:h11

nmap <F3> i<C-R>=strftime("%d-%m-%Y %a %H:%M")<CR><Esc>
imap <F3> <C-R>=strftime("%d-%m-%Y %a %H:%M")<CR>

" Commenting blocks of code.
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

" Set 'Yank' to go in systm Clipboard and Ctrl-C to Vim 'p' command
"set clipboard=unnamed

" Remaping CAPSLOCK to END at start of VIM --UNIX ONLY
"au VimEnter * !xmodmap -e 'clear Lock' -e 'keycode 0x42 = End'
"au VimLeave * !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Caps_Lock'

" Tmux & Vim panel switching integration
" https://www.codeography.com/2013/06/19/navigating-vim-and-tmux-splits
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
