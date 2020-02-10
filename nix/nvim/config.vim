set nocompatible

"Standard vimrc stuff
"-------------------------
filetype plugin indent on
set backspace=indent,eol,start
"set backup
"set backupdir=~/.vim/.backup//
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

"Convenience
"-------------------------
"Make ";" synonymous with ":" to enter commands
nmap ; :
"Open tag in vertical split
map <leader>] :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
map <leader>w :%s/\s\+$//e<CR>

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

"Clipboard
"-------------------------
if has("xterm_clipboard")
  vnoremap <C-c> "+y
  inoremap <C-v> <Esc>"+p i
elseif executable('xclip')
  vnoremap <C-c> :!xclip -f -sel clip <CR>
  inoremap <C-v> <Esc>:r!xclip -o -sel clip <CR>
endif

"Ctrl-O/P to open files
"-------------------------
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'Files'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_lazy_update = 10
nnoremap <C-o> :CtrlPBuffer<CR>
inoremap <C-o> <Esc>:CtrlPBuffer<CR>

"NERDTree
"-------------------------
map <leader>\ :NERDTreeToggle<CR>
let NERDTreeIgnore = [ '\.js_dyn_o', '\.js_hi', '\.js_o', '\.js_dyn_hi', '\.dyn_hi', '\.dyn_o', '\.hi', '\.o', '\.p_hi', '\.p_o' ]
"Automatically close if NERDTree is the only buffer left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"Haskell
"-------------------------
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <F3> :HdevtoolsInfo<CR>

"coc.vim
"Copy pasted from https://github.com/neoclide/coc.nvim

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
" set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
" set statusline+=%{coc#status()} "%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

"end coc.vim

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

"Dhall
let g:dhall_format=1

"ALE
"-------------------------
"let g:ale_linters = { 'haskell': ['hlint'] }
"let g:ale_lint_on_save = 1
"let g:ale_lint_on_text_changed = 0
"nmap <silent> <C-j> <Plug>(ale_next_wrap)

"Scripts
"-------------------------
"Sort haskell imports ignoring the word 'qualified'
vnoremap <leader>m :<C-u>*s/qualified \(.*\)/\1 [qualified]/g <CR> gv :<C-u>*sort <CR> gv :<C-u>*s/import \(.*\) \[qualified\]/import qualified \1/g"<CR>

"Convert CSS to Clay
vnoremap <leader>c :s/-\(.\)/\U\1/g<CR> gv :s/: / /g <CR> gv :s/\(\d\+\)px/(px \1)/g<CR> gv :s/;//g<CR> gv :s/\(\#.*\>\)/"\1"/g<CR> gv :s/\(\d\+\)%/(pct \1)/g<CR>

"Format JSON
map <leader>jt !python -m json.tool<CR>
"Format JavaScript
map <leader>jf <Esc>:let @c = line('.')<CR>:let @d = col('.')<CR>:w<CR>:%!~/.vim/jsbeautify.py -i 2 -n %<CR>:w<CR>:call cursor (@c, @d)<CR>
let g:ctrlp_working_path_mode = 'rw'

"Run HLint
nnoremap <C-h> :!hlint %<CR>

"Slime
"-------------------------
nmap <leader>+ <Plug>SetTmuxVars
nmap <C-i> :Tmux ./build<CR>
let g:tslime_always_current_session = 1
let g:tslime_always_current_window = 1

"Coq
"-------------------------
" Maps Coquille commands to <F2> (Undo), <F3> (Next), <F4> (ToCursor)
au FileType coq call coquille#FNMapping()

"TODO
"-------------------------
" Add TODO highlighting for all filetypes
augroup HiglightTODO
    autocmd!
        autocmd WinEnter,VimEnter * :silent! call matchadd('Todo', 'TODO', -1)
        augroup END

