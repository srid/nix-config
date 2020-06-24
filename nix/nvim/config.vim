set nocompatible

"Standard vimrc stuff
"-------------------------
filetype plugin indent on
set backspace=indent,eol,start
set dir=~/.vim/.swp//
set encoding=utf-8
set expandtab
set exrc
set history=50
set hlsearch
set incsearch
set laststatus=2
set nocompatible
set number
set ruler
set shiftwidth=2
set showcmd
set showmatch
set autoindent
set nocindent
set smartindent
set softtabstop=2
set t_Co=256
set ts=2
"set lazyredraw
"set ttyfast
syntax enable

"Get rid of annoyances
set noswapfile
set nobackup
set nowritebackup

"Convenience
"-------------------------
"Make ";" synonymous with ":" to enter commands
nmap ; :

"C-x as a shortcut for exiting Goyo, save the file and exit Vim altogether
:map <C-X> <ESC>:x<CR>:x<CR>

"Mouse
"-------------------------
set mouse=a
if !has('nvim')
  set ttymouse=sgr
endif

"Escape
"-------------------------
if has('nvim')
  set ttimeoutlen=10
endif

"Colorscheme
"-------------------------
set background=dark
"let base16colorspace=256
"colorscheme base16-mexico-light
colorscheme PaperColor
"Adjust theme search colors
hi Search ctermbg=250 ctermfg=240
hi Comment ctermfg=245

"Clipboard (C-c / C-v)
"-------------------------
if has("xterm_clipboard")
  vnoremap <C-c> "+y
  inoremap <C-v> <Esc>"+p i
elseif executable('xclip')
  vnoremap <C-c> :!copy <CR>
  inoremap <C-v> <Esc>:r!paste <CR>
endif

"NERDTree
"-------------------------
map <leader>\ :NERDTreeToggle<CR>
let NERDTreeIgnore = [ '\.js_dyn_o', '\.js_hi', '\.js_o', '\.js_dyn_hi', '\.dyn_hi', '\.dyn_o', '\.hi', '\.o', '\.p_hi', '\.p_o' ]
"Automatically close if NERDTree is the only buffer left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"Saving	
"-------------------------	
" If the current buffer has never been saved, it will have no name,	
" call the file browser to save it, otherwise just save it.	
command -nargs=0 -bar Update if &modified	
                          \|    if empty(bufname('%'))	
                          \|        browse confirm write	
                          \|    else	
                          \|        confirm write	
                          \|    endif	
                          \|endif
"<C-s> to save
nnoremap <silent> <C-s> :<C-u>Update<CR>
inoremap <C-s> <C-o>:Update<CR>

"TODO
"-------------------------
" Add TODO highlighting for all filetypes
augroup HiglightTODO
    autocmd!
        autocmd WinEnter,VimEnter * :silent! call matchadd('Todo', 'TODO', -1)
        augroup END

"Dhall
let g:dhall_format=1

"ormolu
let g:ormolu_options=["-o -XTypeApplications"]

"But not for work projects
autocmd BufRead,BufNewFile */work/* let b:ormolu_disable=1
"Or for obelisk deps
autocmd BufRead,BufNewFile */dep/* let b:ormolu_disable=1


"Ctrl-O/P to open files
"-------------------------
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'Files'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_lazy_update = 10
nnoremap <C-o> :CtrlPBuffer<CR>
inoremap <C-o> <Esc>:CtrlPBuffer<CR>
"And to open by searching file content
nnoremap <C-l> :Rg<CR> title: 


" vim-which-key
let g:mapleader = "\<Space>"
let g:maplocaleader = ','
" By default timeoutlen is 1000 ms
set timeoutlen=500

nnoremap <Leader>a<Tab> :echom "Hello, World"<cr>
nnoremap <Leader>1 :echom "THis is one"<cr>

"neuron.vim
nnoremap <Leader>zz :call ZettelSearch()<cr>
nnoremap <Leader>zl :call ZettelSearchInsert()<cr>
nnoremap <Leader>zo :call ZettelOpenUnderCursor()<cr>
nnoremap <Leader>zu :call ZettelOpenLast()<cr>

let g:which_key_map = {}

let g:which_key_map.a = {
            \ 'name':"Test",
            \ '<Tab>':"Hello world"
            \}

let g:which_key_map.1 = "One"
let g:which_key_map.w = {
      \ 'name' : '+windows' ,
      \ 'w' : ['<C-W>w'     , 'other-window']          ,
      \ 'd' : ['<C-W>c'     , 'delete-window']         ,
      \ '-' : ['<C-W>s'     , 'split-window-below']    ,
      \ '|' : ['<C-W>v'     , 'split-window-right']    ,
      \ '2' : ['<C-W>v'     , 'layout-double-columns'] ,
      \ 'h' : ['<C-W>h'     , 'window-left']           ,
      \ 'j' : ['<C-W>j'     , 'window-below']          ,
      \ 'l' : ['<C-W>l'     , 'window-right']          ,
      \ 'k' : ['<C-W>k'     , 'window-up']             ,
      \ 'H' : ['<C-W>5<'    , 'expand-window-left']    ,
      \ 'J' : ['resize +5'  , 'expand-window-below']   ,
      \ 'L' : ['<C-W>5>'    , 'expand-window-right']   ,
      \ 'K' : ['resize -5'  , 'expand-window-up']      ,
      \ '=' : ['<C-W>='     , 'balance-window']        ,
      \ 's' : ['<C-W>s'     , 'split-window-below']    ,
      \ 'v' : ['<C-W>v'     , 'split-window-below']    ,
      \ '?' : ['Windows'    , 'fzf-window']            ,
      \ }

call which_key#register('<Space>', "g:which_key_map")

nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
