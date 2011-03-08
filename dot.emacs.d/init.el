;; load-pathの追加
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
 (add-to-list 'load-path default-directory)
 (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
     (normal-top-level-add-subdirs-to-load-path)))
;; 言語を日本語にする
(set-language-environment 'Japanese)
;; 極力UTF-8とする
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
;; Control + h でバックスペース
(global-set-key "\C-h" 'delete-backward-char)
;; フォントの設定
(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
                  'unicode
                  (font-spec :family "Hiragino Kaku Gothic ProN" :size 16)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
;; ドラッグドロップで新たにファイルを開く
(define-key global-map [ns-drag-file] 'ns-find-file)
;; フレームの透過設定
;;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;; C-j の機能を別のキーに割り当て
(global-set-key (kbd "C-m") 'newline-and-indent)
;; C-\ でも SKK に切り替えられるように設定
(setq default-input-method "japanese-skk")
;; 送り仮名が厳密に正しい候補を優先して表示
(setq skk-henkan-strict-okuri-precedence t)
;;漢字登録時、送り仮名が厳密に正しいかをチェック
(setq skk-check-okurigana-on-touroku t)
