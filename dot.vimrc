set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'Shougo/unite.vim'

Plugin 'EnhCommentify.vim'
Plugin 'Rename'
Plugin 'matchit.zip'
Plugin 'twilight256.vim'
Plugin 'vimwiki'

Plugin 'duskhacker/sweet-rspec-vim'
Plugin 'ecomba/vim-ruby-refactoring'
Plugin 'ervandew/supertab'
Plugin 'h1mesuke/vim-alignta'
Plugin 'jpo/vim-railscasts-theme'
Plugin 'kana/vim-textobj-entire'
Plugin 'kana/vim-textobj-indent'
Plugin 'kana/vim-textobj-jabraces'
Plugin 'kana/vim-textobj-user'
Plugin 'kien/ctrlp.vim'
Plugin 'msanders/snipmate.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'neo4j-contrib/cypher-vim-syntax'
Plugin 'ngmy/vim-rubocop'
Plugin 'plasticboy/vim-markdown'
Plugin 'rking/ag.vim'
Plugin 'szw/vim-tags'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-rake'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'vim-ruby/vim-ruby'
Plugin 'vim-scripts/rd.vim'

call vundle#end()

filetype plugin indent on

syntax enable
colorscheme twilight256

set encoding=utf-8
set fileencodings=utf-8,cp932,eucjp,iso2022jp,utf-16
set fileformats=unix,dos,mac

set ambiwidth=double
set autoindent
set autoread
set background=dark
set backspace=indent,eol,start
set cursorline
set directory-=.
set display=lastline
set hidden
set ignorecase
set incsearch
set laststatus=2
set list
set listchars=tab:»\ 
set mouse=a
set nobackup
set nohlsearch
set number
set ruler
set showcmd
set showmatch
set showmode
set smartcase
set smartindent
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%=%l,%c%v%8p
set expandtab tabstop=2 shiftwidth=2
set t_Co=256
set ttimeoutlen=0
set virtualedit=block
set visualbell t_vb=
set wildmode=list:longest,list:full
set expandtab tabstop=2 shiftwidth=2
set clipboard=unnamedplus

set shell=$SHELL

noremap ; :
noremap : ;
noremap <silent> j gj
noremap <silent> k gk
noremap <silent> gj j
noremap <silent> gk k

nnoremap + <C-w>+
nnoremap - <C-w>-

cnoremap <C-a> <Home>
cnoremap <C-x> <C-r>=expand('%:p:h')<CR>/
cnoremap <expr> / getcmdtype() == '/' ? '\/' : '/'

nmap <F7> <ESC>a<C-r>=strftime("%H:%M:%S")<CR><ESC>
nmap <F8> <ESC>a<C-r>=strftime("%Y-%m-%d")<CR><ESC>

highlight Pmenu ctermbg=LightGray ctermfg=Black guibg=LightGray guifg=Black
highlight PmenuSel ctermbg=Blue guibg=RoyalBlue
highlight PmenuSbar ctermbg=LightGray guibg=LightGray
highlight PmenuThumb ctermbg=White guibg=White

let g:ctrlp_map = '<Esc>t'
let g:netrw_altv = 1
let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
set tags+=./tags,tags;

augroup MyAutoCmd
  autocmd!

  " 挿入モード時、paste オプションを解除する
  autocmd InsertLeave * set nopaste

  " 以前開いていたときのカーソル位置を復元する
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

  " 挿入モード時、ステータスラインの色を変える
  autocmd InsertEnter * highlight StatusLine ctermfg=red
  autocmd InsertLeave * highlight StatusLine ctermfg=white

  " 自動的に QuickFix リストを表示する
  autocmd QuickFixCmdPost make,grep,grepadd,vimgrep,vimgrepadd cwin
  autocmd QuickFixCmdPost lmake,lgrep,lgrepadd,lvimgrep,lvimgrepadd lwin

  if !has('gui_running') && !(has('win32') || has('win64'))
    " .vimrcの再読込時にも色が変化するようにする
    autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
  else
    " .vimrcの再読込時にも色が変化するようにする
    autocmd BufWritePost $MYVIMRC source $MYVIMRC | if has('gui_running') | source $MYGVIMRC
    autocmd BufWritePost $MYGVIMRC if has('gui_running') | source $MYGVIMRC
  endif
augroup END

function! s:SetupSpeCuke()
  command! RunTestFile exe '!sc ' . expand('%:p')
  command! RunTestCase exe '!sc --line ' . line('.') . ' ' . expand('%:p')

  nnoremap -tf :RunTestFile<CR>
  nnoremap -tc :RunTestCase<CR>
endfunction

" Unite
nnoremap <Space>f :Unite file_rec<CR>
nnoremap <Space>F :Unite file<CR>
nnoremap <Space>r :UniteWithBufferDir file<CR>
nnoremap <Space>b :Unite buffer<CR>
nnoremap <Space>B :Unite file_mru<CR>
nnoremap <Space>l :Unite outline<CR>
nnoremap <Space>c :Unite stepdefs<CR>
nnoremap <Space>g :Unite grep:.:-S:
nnoremap <Space>G :Unite grep:.:-wS:
nnoremap <Space>t :Unite turnip<CR>

au BufRead,BufNewFile *_spec.rb,*.feature call s:SetupSpeCuke()
au FileType gitcommit syntax match gitcommitComment /^\^.*/
