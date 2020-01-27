# Based on [Ali Abrar](https://github.com/ali-abrar)'s Vim configuration.

{ pkgs ? import <nixpkgs> {}, ... }:

let
  # cf. https://nixos.wiki/wiki/Vim#Adding_new_plugins 

  customPlugins = {
    neovim-ghcid = pkgs.vimUtils.buildVimPlugin {
      name = "ghcid";
      src = (pkgs.fetchFromGitHub { 
        owner = "ndmitchell";
        repo = "ghcid";
        rev = "5d7f859bc6dd553bdf93e6453391353cf310e232";
        sha256 = "1gyasmk6k2yqlkny27wnc1fn2khphgv400apfh1m59pzd9mdgsc2";
      }) + "/plugins/nvim";
    };

    papercolor-theme = pkgs.vimUtils.buildVimPlugin {
      name = "papercolor-theme";
      src = pkgs.fetchFromGitHub {
        owner = "NLKNguyen";
        repo = "papercolor-theme";
        rev = "ddd0986";
        sha256 = "1dhbnd99xs6l5alqhn9m1nynmr9sbvrqj2137l23ysisprl3rgmr";
      };
    };

    indenthaskell = pkgs.vimUtils.buildVimPlugin {
      name = "indenthaskell";
      src = pkgs.fetchFromGitHub {
        owner = "vim-scripts";
        repo = "indenthaskell.vim";
        rev = "17380713774ea4f3ca5da1de455126fa1cce82f7";
        sha256 = "1cs9qkn40fk3c8a9kvbdm3d6izf944awiagpmllkvlb4ci9m6lk7";
      };
    };

    nerdtree = pkgs.vimUtils.buildVimPlugin {
      name = "nerdtree";
      src = pkgs.fetchFromGitHub {
        owner = "scrooloose";
        repo = "nerdtree";
        rev = "e47e588705bd7d205a3b5a60ac7090c9a2504ba2";
        sha256 = "15ai00k7w0brbjvmsj920hpnqy4iz1y3b0pw04m3mlcx20pkfy9s";
      };
    };

    lastpos = pkgs.vimUtils.buildVimPlugin {
      name = "lastpos";
      src = pkgs.fetchFromGitHub {
        owner = "vim-scripts";
        repo = "lastpos.vim";
        rev = "21a22ce4a11117cae8a0017c1cd9a9094fe5adf2";
        sha256 = "0b4xd87a8pxhdf6g8digvjc1a83y572qk4qfdccda2r5m4knidm4";
      };
    };

    ag = pkgs.vimUtils.buildVimPlugin {
      name = "ag";
      src = pkgs.fetchFromGitHub {
        owner = "rking";
        repo = "ag.vim";
        rev = "4a0dd6e190f446e5a016b44fdaa2feafc582918e";
        sha256 = "1dz7rmqv3xw31090qms05hwbdfdn0qd1q68mazyb715cg25r85r2";
      };
    };

    neocomplcache = pkgs.vimUtils.buildVimPlugin {
      name = "neocomplcache";
      src = pkgs.fetchFromGitHub {
        owner = "Shougo";
        repo = "neocomplcache.vim";
        rev = "778181767467b8f8016828898779a646074d883a";
        sha256 = "080h24fqv9gsv9ny33gxzsy03w9wyx1xw8f1xwqyll9c6hw62ygy";
      };
    };
  };
in
  with pkgs; neovim.override {
    configure = {
      # Builtin packaging
      # List of plugins: nix-env -qaP -A nixos.vimPlugins
      packages.myVimPackage = with pkgs.vimPlugins; {
        # Loaded on launch
        start = [ ];
        # Manually loadable by calling `:packadd $plugin-name
        opt = [ ];
      };

      # VAM
      vam.knownPlugins = pkgs.vimPlugins // customPlugins;
      vam.pluginDictionaries = [
        { name = "goyo"; }  # Distraction free writing
        { name = "vim-auto-save"; }
        { name = "vim-nix"; }
        { name = "haskell-vim"; }
        { name = "vim-gitgutter"; }
        { name = "ctrlp"; }
        { name = "ctrlp-py-matcher"; }
        { name = "papercolor-theme"; }
        { name = "indenthaskell"; }
        { name = "nerdtree"; }
        { name = "lastpos"; }
        { name = "ag"; }
        { name = "vim-nix"; }
        { name = "fugitive"; }
        { name = "tslime"; }
        { name = "neocomplete"; }
        { name = "neocomplcache"; }
        { name = "fzf-vim"; }
        { name = "fzfWrapper"; }
        { name = "neovim-ghcid"; }
        { name = "coc-nvim"; }
        # { name = "vim-stylish-haskell"; }
      ];

      customRC = ''
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

      "Use ag/silver-searcher for faster and more configurable search
      if executable('ag')
        set grepprg=ag\ --nogroup\ --nocolor\ --smart-case
        let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden --smart-case
                                    \ --ignore .git
                                    \ --ignore .svn
                                    \ --ignore .hg
                                    \ --ignore amazonka
                                    \ --ignore="*.dyn_hi"
                                    \ --ignore="*.dyn_o"
                                    \ --ignore="*.p_hi"
                                    \ --ignore="*.p_o"
                                    \ --ignore="*.hi"
                                    \ --ignore="*.o"
                                    \ -g ""'
      endif

      "Use a better relevance algorithm
      if !has('python')
        echo 'In order to use pymatcher plugin, you need +python compiled vim'
      else
        let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
      endif

      "Autocomplete
      "-------------------------
      let g:neocomplcache_enable_at_startup = 1
      let g:neocomplcache_tags_caching_limit_file_size = 10000000

      "Use a custom <CR> handler
      inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
      function! s:my_cr_function()
        return neocomplcache#smart_close_popup() . "\<CR>"
        " For no inserting <CR> key.
        "return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
      endfunction

      "<TAB>: completion.
      inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
      "Close popup
      inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
      inoremap <expr><C-y>  neocomplcache#close_popup()
      inoremap <expr><C-e>  neocomplcache#cancel_popup()
      "<BS>: delete backword char
      inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"

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

      '';
    };
  }

