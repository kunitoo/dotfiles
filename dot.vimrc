noremap ; :
noremap : ;

set completeopt=menu,menuone,noinsert
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : (col('.') == 1 \|\| getline('.')[col('.') - 2] =~ '\s') ? "\<Tab>" : "\<C-n>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"
