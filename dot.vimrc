set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'gmarik/vundle'

Plugin 'Shougo/unite.vim'
Plugin 'mtsmfm/unite-turnip'

Plugin 'EnhCommentify.vim'
Plugin 'Rename'
Plugin 'matchit.zip'
Plugin 'twilight256.vim'
Plugin 'vimwiki'

Plugin 'csexton/trailertrash.vim'
Plugin 'duskhacker/sweet-rspec-vim'
Plugin 'ecomba/vim-ruby-refactoring'
Plugin 'ervandew/supertab'
Plugin 'h1mesuke/vim-alignta'
Plugin 'jceb/vim-orgmode'
Plugin 'jpo/vim-railscasts-theme'
Plugin 'kana/vim-fakeclip'
Plugin 'kana/vim-textobj-entire'
Plugin 'kana/vim-textobj-indent'
Plugin 'kana/vim-textobj-jabraces'
Plugin 'kana/vim-textobj-user'
Plugin 'kchmck/vim-coffee-script'
Plugin 'kenchan/rubyblue'
Plugin 'kien/ctrlp.vim'
Plugin 'msanders/snipmate.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'pangloss/vim-javascript'
Plugin 'plasticboy/vim-markdown'
Plugin 'repos-scala/scala-vundle'
Plugin 'scrooloose/syntastic'
Plugin 'seebi/semweb.vim'
Plugin 'skalnik/vim-vroom'
Plugin 'szw/vim-tags'
Plugin 'thinca/vim-ref'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-cucumber'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-ragtag'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-rake'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-speeddating'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'vim-ruby/vim-ruby'

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
set clipboard=unnamed
set paste

if $SHELL =~ '/fish$'
  set shell=zsh
endif

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

highlight Pmenu ctermbg=LightGray ctermfg=Black guibg=LightGray guifg=Black
highlight PmenuSel ctermbg=Blue guibg=RoyalBlue
highlight PmenuSbar ctermbg=LightGray guibg=LightGray
highlight PmenuThumb ctermbg=White guibg=White

let g:ctrlp_map = '<Esc>t'
let g:netrw_altv = 1
let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
set tags+=tags,Gemfile.lock.tags

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
