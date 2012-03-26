;; load-pathの追加
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
 (add-to-list 'load-path default-directory)
 (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
     (normal-top-level-add-subdirs-to-load-path)))
;; auto-install追加
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/lisp/auto-install")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保
;; anythig startup
(require 'anything-startup)
;; 言語を日本語にする
(set-language-environment 'Japanese)
;; 極力UTF-8とする
;;(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
;; Control + h でバックスペース
(global-set-key "\C-h" 'delete-backward-char)
;; Control + t でother-window
(global-set-key "\C-t" 'other-window)
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
(set-frame-parameter (selected-frame) 'alpha '(80 80))
;; C-j の機能を別のキーに割り当て
(global-set-key "\C-m" 'newline-and-indent)
;; C-\ でも SKK に切り替えられるように設定
;; 送り仮名が厳密に正しい候補を優先して表示
(setq skk-henkan-strict-okuri-precedence t)
;;漢字登録時、送り仮名が厳密に正しいかをチェック
(setq skk-check-okurigana-on-touroku t)

;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/sw/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
;; PATH と exec-path に同じ物を追加します
 (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;; MANPATHの設定
(setenv "MANPATH" (concat "/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/share/man" (getenv "MANPATH")))

;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

(cond
 ((or (eq window-system 'mac) (eq window-system 'ns))
  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は NFD (の様な物)で扱うため以下の設定をする必要がある
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (or (eq system-type 'cygwin) (eq system-type 'windows-nt)
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  ;; もしコマンドプロンプトを利用するなら sjis にする
  ;; (setq file-name-coding-system 'sjis)
  ;; (setq locale-coding-system 'sjis)
  ;; 古い Cygwin だと EUC-JP にする
  ;; (setq file-name-coding-system 'euc-jp)
  ;; (setq locale-coding-system 'euc-jp)
  )
 (t
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)))

;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)

;; multi-termの設定
(require 'multi-term)
(setq multi-term-program shell-file-name)
(add-hook 'term-mode-hook
         '(lambda ()
            ;; C-h を term 内文字削除にする
            (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
            ;; C-y を term 内ペーストにする
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            ))
;; multi-term の呼び出しキー割当
(global-set-key (kbd "C-c t") '(lambda ()
                                (interactive)
                                (multi-term)))
;; キー起動時に複数起動でなく既存のバッファを選択する設定
;; (global-set-key (kbd "C-t") '(lambda ()
;;                                (interactive)
;;                                (if (get-buffer "*terminal<1>*")
;;                                    (switch-to-buffer "*terminal<1>*")
;;                                (multi-term))))
;; 複数の Shell のバッファのみを切り替える
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)

;;; simplenoteの設定
(require 'simplenote)

;; color-themeの設定
(require 'color-theme)
(color-theme-initialize)
(color-theme-arjen)
;; (color-theme-Vim_colors)
;;(unless (zenburn-format-spec-works-p)
;;  (zenburn-define-format-spec))


;; タブ数の指定
(setq-default tab-width 2)
;; タブをスペースに変更
(setq-default indent-tabs-mode nil)

;; color-moccur
(require 'color-moccur)
(setq moccur-split-word t)
;; moccur-edit
(require 'moccur-edit)
;; wdired の設定
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; auto-complete の設定
(require 'auto-complete)
(global-auto-complete-mode 1)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;; Don't start completion automatically
;; ------------------------------------
;; (setq ac-auto-start nil)
(global-set-key "\M-/" 'ac-start)
;;
;; start completion when entered 3 characters
(setq ac-auto-start 1)


;====================================
;;jaspace.el を使った全角空白、タブ、改行表示モード
;;切り替えは M-x jaspace-mode-on or -off
;====================================
(require 'jaspace)
;; 全角空白を表示させる。
(setq jaspace-alternate-jaspace-string "□")
;; 改行記号を表示させる。
(setq jaspace-alternate-eol-string "↓\n")
;; タブ記号を表示。
(setq jaspace-highlight-tabs t)  ; highlight tabs

;; EXPERIMENTAL: On Emacs 21.3.50.1 (as of June 2004) or 22.0.5.1, a tab
;; character may also be shown as the alternate character if
;; font-lock-mode is enabled.
;; タブ記号を表示。
;;(setq jaspace-highlight-tabs ?&gt;) ; use ^ as a tab marker
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; (save-window-excursion (shell-command (format "emacs-test -l %s %s &" buffer-file-name buffer-file-name)))
(add-to-list 'load-path "~/.emacs.d/el-get/el-get/")
(require 'el-get)
(load "2010-12-09-095707.el-get-ext.el")
;; 初期化ファイルのワイルドカードを指定する
;;(setq el-get-init-files-pattern "~/emacs/init.d/[0-9]*.el")
;;(setq el-get-sources (el-get:packages))
;;(setq el-get-sources (el-get:zenburn))
;;(el-get)

;; popwinの設定
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;;; 現在行に色をつける
;;(global-hl-line-mode 1)
;; 色
;;(set-face-background 'hl-line "darkolivegreen")
;;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
;;; ファイル内のカーソル位置を記憶する
(setq-default save-place t)
(require 'saveplace)
;;; 対応する括弧を表示される
(show-paren-mode 1)
;;; モードラインに時刻を表示する
(display-time)
;;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)
;;; リージョンに色をつける
(transient-mark-mode 1)
;;; GCを減らして軽くする(デフォルトの10倍)
;;; 現在のマシンパワーではもっと大きくしてもよい
(setq gc-cons-threshold (* 10 gc-cons-threshold))
;;; ログの記録行数を増やす
(setq message-log-max 10000)
;;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)
;;; ダイアログボックスを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)
;;; 履歴をたくさん表示する
(setq history-length 1000)
;;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)
;;; 大きいファイルを開こうとしたときに警告を発生させる
;;; デフォルトでは10MBなので25MBに拡張する
(setq large-file-warning-threshold (* 25 1024 1024))
;;; ミニバッファで入力を取り消しても履歴に残す
;;; 誤って取り消して入力が失われるのを防ぐため
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))
;;; yesと入力するのは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)
;;; ツールバーを消す
(tool-bar-mode -1)
;;; スクロールバーは消さない
(scroll-bar-mode 1)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; ffap.el 現在位置のファイル・URLを開く
(ffap-bindings)

;;; uniquify.el ファイル名がかぶった場合にバッファ名をわかりやすくする
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; * で囲まれたバッファ名は対象外にする
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;; iswitchb.el バッファ 切り替えを強化する
(iswitchb-mode 1)
;; バッファ読み取り関数を iswitchb にする
(setq read-buffer-function 'iswitchb-read-buffer)
;; 部分文字列の代わりに正規表現を使う場合は t に設定する
(setq iswitchb-regexp nil)
;; 新しいバッファを作成するときにいちいち聞いてこない
(setq iswitchb-prompt-newbuffer nil)

;;; JavaScript major mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; htmlize
(load "htmlize.el")

;;; 現在時刻の挿入
(defun insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M")))
;;; 始業時間用
(defun start-work ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d,%H:%M")))

;;; fullscreen の設定
(require 'fullscreen)

;;; DDSKKの設定
;;(require 'skk-autoloads)
(require 'skk-setup)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)
(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)

;; emacsclient シェルから現在のEmacsにアクセスする
;;(server-start)
;;(defun iconify-emacs-when-server-is-done ()
;;  (unless server-clients (iconify-frame)))
;; 編集が終了したらEmacsをアイコン化する(好みに応じて)
;; (add-hook 'server-done-hook 'iconify-emacs-when-server-is-done)
;; C-x C-c に割り当てる(好みに応じて)
;; (global-set-key "\C-x\C-c" 'server-edit)
;; M-x exit でEmacsを終了できるよいうにする
;;(defalias 'exit 'save-buffers-kill-emacs)

;; migemo.el
;;(require 'migemo)

;;; haskell-mode
(load "~/.emacs.d/lisp/haskell-mode/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;; magitの設定
(add-to-list 'load-path "/usr/local/Cellar/magit/1.0.0/share/emacs/site-lisp")
(require 'magit)

;;; org-modeの設定
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate)
(setq org-directory "~/memo/")
(setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ))
(require 'org)
(add-hook 'org-mode-hook 'howm-mode)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
;;(setq howm-view-title-header "*") ;; ← howm のロードより前に書くこと
;; インデントされたアウトラインの見出しを隠す
(setq org-hide-leading-stars 1)

;;; howm
(setq howm-menu-lang 'ja)
(require 'howm-mode)
;; キー割当の重複を避ける (お好みで)
;;(setq howm-prefix "\C-z") ;; howm のキーを「C-c , □」から「C-z □」に変更

;;; M-x kmacro-save キーボードマクロをコマンド化する
(defvar kmacro-save-file "~/.emacs.d/init.el")
(defun kmacro-save (symbol)
  (interactive "SName for last kbd macro: ") ; 定義するコマンド名を入力
  (name-last-kbd-macro symbol)               ; 最後に定義したマクロに名前をつける
  (with-current-buffer (find-file-noselect kmacro-save-file)
    (goto-char (point-max))                  ; .emacs.d/init.el の末尾に
    (insert-kbd-macro symbol)                ; マクロの定義を挿入して
    (basic-save-buffer)))                    ; 保存する
;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; codingの自動挿入をやめる
;; from http://d.hatena.ne.jp/akm/20080605#1212644489
(require 'ruby-mode)
(defun ruby-mode-set-encoding () ())

;; Rinari
(add-to-list 'load-path "~/.emacs.d/lisp/rinari")
(require 'rinari)
;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/lisp/rhtml")
(require 'rhtml-mode)

;; 行番号を表示
(require 'wb-line-number)
;; (wb-line-number-toggle)

;;;
;;; Put the following code in your .emacs file:
;;;
(autoload 'html-mode "html" "HTML major mode." t)
(or (assoc "¥¥.html$" auto-mode-alist)
    (setq auto-mode-alist (cons '("¥¥.html$" . html-mode)
                                auto-mode-alist)))
;; html-helper-mode
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(defvar html-helper-new-buffer-template
  '("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n"
    "<html lang=\"ja\">\n"
    "<head>\n"
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=EUC-JP\">\n"
    "<title>" p "</title>\n"
    "<script type=\"text/javascript\"></script>\n"
    "</head>\n"
    "<body>\n\n<h1>" p "</h1>\n\n" p
    "\n</body>\n</html>\n")
  "*Template for new buffers, inserted by html-helper-insert-new-buffer-strings if
html-helper-build-new-buffer is set to t")

;; SGML
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; (setq auto-mode-alist
;;     (append (list (cons "\\.html\\'" 'xml-mode))
;;              auto-mode-alist))

;;(setq auto-mode-alist
;;     (append (list (cons "\\.shtml\\'" 'xml-mode))
;;              auto-mode-alist))

;;(setq auto-mode-alist
;;      (append (list (cons "\\.xml\\'" 'xml-mode))
;;              auto-mode-alist))

(load-library "php-mode")
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.ctp$" . php-mode))

(add-hook 'php-mode-user-hook
'(lambda ()
(setq tab-width 2)
(setq indent-tabs-mode nil))
)
;; 以下キーボードマクロが書かれえる
