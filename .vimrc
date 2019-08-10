set laststatus=2 " Always display the statusline in all windows
set showtabline=2 " Always display the tabline, even if there is only one tab
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set clipboard=unnamedplus
 
let g:airline_powerline_fonts = 1


call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'powerline/powerline', { 'rtp': 'powerline/bindings/vim' }
call plug#end()
