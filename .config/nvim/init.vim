set clipboard+=unnamedplus

"let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" Or if you have Neovim >= 0.1.5
"if (has("termguicolors"))
"  set termguicolors
"endif

" Theme
"syntax enable
"colorscheme solarized
"set background=dark

let g:airline_powerline_fonts = 1
let g:airline_theme='oceanicnext'

call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'mhartington/oceanic-next'
Plug 'vim-airline/vim-airline'

" Loaded when clojure file is opened
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Multiple file types
Plug 'kovisoft/paredit', { 'for': ['clojure', 'scheme'] }

" On-demand loading on both conditions
Plug 'junegunn/vader.vim',  { 'on': 'Vader', 'for': 'vader' }

" Code to execute when the plugin is lazily loaded on demand
Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
autocmd! User goyo.vim echom 'Goyo is now loaded!'

call plug#end()
