;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-

(require 'cl)

(add-to-list 'load-path "~/.emacs.d")

;; パッケージ管理
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; 言語設定は環境変数に依存
(set-language-environment nil)

;; dired などでマルチバイト文字が化ける
(setq default-file-name-coding-system 'utf-8-unix)

;; meadow 向けの設定
; (if (string-equal system-type "windows-nt") (let () ))

;; 自動保存機能
(setq auto-save-default t
      auto-save-list-file-name "~/.emacs-auto-save-list" ;; 自動保存に関する情報
      auto-save-intrval 50  ;; 自動保存する打鍵回数
      auto-save-timeout 10)  ;; 自動保存する時間

;; フォントロックモード (強調表示等) を有効にする
;; (global-font-lock-mode t)

;; 対応する括弧をハイライト
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; バッファ末尾に余計な改行コードを防ぐための設定
(setq next-line-add-newlines nil)

;; 一行で表示しきれない時の挙動 (nil/t)
(setq truncate-partial-width-windows t)

;; インデントの際にタブを用いるか否か
(setq-default indent-tabs-mode nil)

;; メニューバーの表示
(menu-bar-mode (if window-system 1 0))

;; シンボリックリンク先がバージョンコントロール化にある時の
;; プロンプトを表示しない
(setq vc-follow-symlinks t)

;; "The local variables list in .emacs" と言われるのを抑止
(add-to-list 'ignored-local-variables 'syntax)

;; 列数表示
(column-number-mode 1)

;; スプラッシュ画面を表示しない
(setq inhibit-splash-screen t)

;; BS でマーク範囲を消す
(delete-selection-mode 1)

;; Emacs のフレームの横幅最小値（文字数で指定）
(defvar kui/min-colmun-number 80) ;; 80 文字

;; Emacs のフレームの横幅最大値（文字数で指定）
(defvar kui/max-colmun-number (/ 1000 (frame-char-width))) ;; 1000 px

;; マーク範囲をハイライト
(setq-default transient-mark-mode t)

;; 現在の行をハイライト
(global-hl-line-mode)

;; 保存前に末尾空白の削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -------------------------------------------------------------------------
;; グローバルキーバインド変更

;; 不要なキーバインド削除
(global-unset-key "\C-\\") ;; 入力モード切り替え
(global-unset-key "\C-t")  ;; 文字入れ替え

;; C-h でカーソルの左にある文字を消す
(global-set-key "\C-h" 'delete-backward-char)

;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(global-set-key "\C-x\C-h" 'help-command)

;; tag のキーバインド
(global-set-key "\M-t" nil)
(global-set-key "\M-tt" 'find-tag)
(global-set-key "\M-t\M-t" 'find-tag)
(global-set-key "\M-tn" 'next-tag)
(global-set-key "\M-tp" 'pop-tag-mark)
;; (global-set-key "\M-o" 'list-tags)

;; goto-line を実行
(define-key ctl-x-map "l" 'goto-line)

;; -------------------------------------------------------------------------
;; 自作関数

(defun kui/vbutlast (vector &optional n)
  "Return a copy of VECTOR with the last N elements removed."
  (vconcat (butlast (append vector nil) n)))

;; インストールされていないパッケージを require した時に、
;; 自動でインストールしたあとに require してくれる
(defun kui/package-require (feature &optional filename packagename noerror)
  "If PACKAGENAME(or FEATURE) was installed, execute `require' as:

	(require FEATURE &optional FILENAME NOERROR)

If PACKAGENAME(or FEATURE) not installed, execute `package-install' with
PACKAGENAME(or FEATURE) and then execute `require'.
If NOERROR is non-nil, then return nil if PACKAGENAME(or FEATURE) is not
available package."
  (unless package--initialized (package-initialize t))
  (unless package-archive-contents (package-refresh-contents))
  (let ((pname (or packagename feature)))
    (if (assq pname package-archive-contents)
        (let nil
          (unless (package-installed-p pname) (package-install pname))
          (or (require feature filename t)
              (if noerror nil
                (error "Package `%s' does not provide the feature `%s'"
                       (symbol-name pname) (symbol-name feature)))))
      (if noerror nil
        (error "Package `%s' is not available for installation"
               (symbol-name feature))))))

;; C-w をもう少し賢く
(defun kui/backward-kill-word-or-kill-region ()
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key "\C-w" 'kui/backward-kill-word-or-kill-region)

;; インデント先頭に移動
;; インデント先頭時は行頭移動
;; 行頭時は何もしない （要するに eclipse 風）
(defun kui/move-beginning-of-line ()
  "back-to-indentation but move-beginning-of-line if point is in indentation
or nothing if point is in BoL"
  (interactive)
  (unless (= (point) (point-at-bol))
    (set 'old-point (point))
    (back-to-indentation)
    (if (= old-point (point))
        (move-beginning-of-line nil))))
(global-set-key "\C-a" 'kui/move-beginning-of-line)

;; require の代わりに使う
(defun kui/autoload-if-exist (function file &optional docstring interactive type)
  "do `(autoload FUNCTION FILE)` if FILE exist"
  (if (locate-library file)
      (let () (autoload function file docstring interactive type) t)))

;; 現在の行をコメントアウト
(defun kui/comment-or-uncomment-current-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

;; region ある時は、そのリージョンをコメントアウト
;; region ない時は、現在行をコメントアウト
(defun kui/comment-or-uncomment ()
  "comment or uncomment region, but if region is not active, comment or \
uncomment the current line"
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (comment-or-uncomment-region (region-beginning) (region-end))
    (kui/comment-or-uncomment-current-line)))
(global-set-key (kbd "C-;") 'kui/comment-or-uncomment)

;; マジックコメント挿入
(defun kui/insert-magic-comment ()
  "insert magic comment with current coding & major-mode"
  (interactive)
  (let* ((coding (if buffer-file-coding-system
                     (symbol-name buffer-file-coding-system)))
         (mode (if major-mode
                   (replace-regexp-in-string "-mode\\'" ""
                                             (symbol-name major-mode))))

         (magic-comment (format "-*- %s%s-*-"
                                (if coding (format "coding:%s; " coding) "")
                                (if mode (format "mode:%s; " mode) ""))))
    (if (or coding mode)
        (let ()
          (goto-char (point-min))
          (if (looking-at "^#!") (beginning-of-line 2))
          (insert magic-comment)
          (comment-region (line-beginning-position) (line-end-position))
          (newline))
      (message "Error: both current coding and major-mode are nil."))))

;; 確認なしでバッファの削除
(defun kui/kill-buffer-without-interaction ()
  "Kill the current buffer without interaction"
  (interactive)
  (kill-buffer nil))
(global-set-key "\C-xk" 'kui/kill-buffer-without-interaction)

;; フルスクリーン状態をトグル
(defun kui/toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (cond
   ((eq window-system 'x) ;; when x window system
    (set-frame-parameter
     nil 'fullscreen
     (if (not (frame-parameter nil 'fullscreen)) 'fullboth)))
   (t ;; default
    (message "window-system:%s not supported" (symbol-name window-system)))))
(global-set-key [M-return] 'kui/toggle-fullscreen)
(global-set-key [f11] 'kui/toggle-fullscreen)

;; 実行可能なコマンドを返す
(defun kui/find-if-executable (seq)
  "Find and Return first executable command in SEQ."
  (find-if (lambda (cmd) (executable-find cmd))
           seq))

;; *scratch* バッファに切り替え（消してしまっていたら作成）
(defun kui/switch-to-scratch-buffer ()
  "switch to *scratch*.
create *scratch* if it did not exists"
  (interactive)
  (switch-to-buffer "*scratch*")
  (insert initial-scratch-message))

;; -------------------------------------------------------------------------
;; 便利な感じのマイナーモード

;; popup
(when (kui/package-require 'popup nil nil t))

;; popwin
(when (kui/package-require 'popwin nil nil t)
  (setq
   ;; display-buffer の置き換え
   display-buffer-function 'popwin:display-buffer

   ;; popwin がでてくる場所のデフォルト値
   ;; popwin:popup-window-position 'right
   )
  (set 'popwin:special-display-config
       (append
        '(("*anything imenu*" :position :right)
          )
        popwin:special-display-config))
  )

;; guide-key
(when (kui/package-require 'guide-key nil nil t)
  (defvar guide-key/popup-if-super-key-sequence nil)
  ;; "C-x" に設定しても "C-x C-h" でも起動するように、起動条件を再定期
  (defun guide-key/popup-guide-buffer-p (key-seq)
    "Return t if guide buffer should be popped up."
    (and (> (length key-seq) 0)
         (or (member key-seq (mapcar 'guide-key/convert-key-sequence-to-vector
                                     guide-key/guide-key-sequence))
             (and guide-key/popup-if-super-key-sequence
                  (guide-key/popup-guide-buffer-p (kui/vbutlast key-seq))))))

  (setq
   guide-key/guide-key-sequence '("M-t" "C-c" "C-x RET" "C-x C-h" "C-x r")
   guide-key/popup-window-position 'bottom
   guide-key/polling-time 0.5
   guide-key/popup-if-super-key-sequence t
   )

  (guide-key-mode 1))

;; auto-complete-mode
(when (kui/package-require 'auto-complete-config nil 'auto-complete t)
  (ac-config-default)

  ;; ac-modes に登録されてるメジャーモード時に ac 発動
  (global-auto-complete-mode t)

  ;; *候補間を移動
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-/" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  ;; *補完停止
  (define-key ac-complete-mode-map "\C-[" 'ac-stop)

  ;; *補完リスト表示開始
  ;; (global-set-key "\C-o" 'ac-start)
  (global-set-key "\M-/" 'ac-start)

  ;; *補完リスト表示自動開始文字数（nil だと自動表示されない）
  (setq ac-auto-start 2)

  ;; *大文字・小文字の区別
  ;; nil:        区別しない
  ;; t:  区別する
  ;; 'smart: 補完対象に大文字が含まれる場合のみ区別する
  (setq ac-ignore-case 'smart)

  ;; 直ちに補完メニューを表示する
  ;; (ac-show-menu-immediately-on-auto-complete t)
  )

;; tabbar-mode
(when (kui/package-require 'tabbar nil nil t)

  ;; tabbar のタブのグループの仕方
  ;;   デフォルト: 一部を除き major-mode ごとにタブをグループ化
  ;; see http://www.emacswiki.org/emacs/TabBarMode
  (setq tabbar-buffer-groups-function (lambda () (list "Buffers")))

  ;; 表示するタブのフィルタリング
  ;;   * で始まるバッファはタブに表示しない
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda (b)
             (and (string-match "^ ?\\*" (buffer-name b))
                  (not (string-equal (buffer-name (current-buffer))
                                     (buffer-name b)))))
           (buffer-list))))

  ;; 左に表示されるボタンを消す
  (dolist (button '(tabbar-buffer-home-button
                    tabbar-scroll-left-button
                    tabbar-scroll-right-button))
    (set button (cons (cons "" nil) (cons "" nil))))

  ;; 色とか
  (set-face-attribute 'tabbar-selected nil
                      :foreground "white"
                      :background nil
                      :box nil
                      :inherit nil
                      :height 1.0
                      )
  (set-face-attribute 'tabbar-unselected nil
                      :height 1.0
                      :foreground "#dedede")

  ;; ウィンドウシステムを使っていないとき
  (when (not window-system)

    ;; タブの間に挟む文字
    (setq tabbar-separator-value "/")

    ;; faces
    (set-face-attribute 'tabbar-default nil
                        :background "#333333"
                        :foreground "black"
                        :underline t
                        :box nil)
    (set-face-attribute 'tabbar-selected nil
                        :background "black"
                        :foreground "white"
                        :underline nil
                        :box nil)
    (set-face-attribute 'tabbar-unselected nil
                        :background "white"
                        :foreground "black"
                        :underline t
                        :box nil)
    )

  ;; Ctrl+Tab でタブ切り替え
  (global-set-key "\M-[1;5i" 'tabbar-forward)  ;; for mintty
  (global-set-key "\M-[1;6i" 'tabbar-backward) ;; for mintty
  (global-set-key [(control tab)] 'tabbar-forward)
  (global-set-key [(control shift tab)] 'tabbar-backward)
  (global-set-key [(control shift iso-lefttab)] 'tabbar-backward) ;; for x window system
  (global-set-key "\C-xn" 'tabbar-forward)
  (global-set-key "\C-xp" 'tabbar-backward)

  (tabbar-mode)
  )

;; flymake 使うとき
(eval-after-load "flymake"
  '(let nil
     (defvar flymake-display-err-delay 1
       "delay to display flymake error message ")
     (defvar flymake-display-err-timer nil
       "timer for flymake-display-err-menu-for-current-line")
     (defvar flymake-display-err-before-line nil)
     (defvar flymake-display-err-before-colmun nil)

     (when (require 'popup nil t)

       ;; flymake 現在行のエラーをpopup.elのツールチップで表示する
       ;; https://gist.github.com/292827
       (defun flymake-display-err-menu-for-current-line ()
         (interactive)
         (let* ((line-no (flymake-current-line-no))
                (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info
                                                                  line-no))))
           (when (and (flymake-display-err-check-moved line-no (current-column))
                      line-err-info-list)
             (setq flymake-display-err-before-line-no line-no)
             (let* ((count (length line-err-info-list))
                    (menu-item-text nil))
               (while (> count 0)
                 (setq menu-item-text
                       (flymake-ler-text (nth (1- count) line-err-info-list)))
                 (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
                        (line (flymake-ler-line (nth (1- count) line-err-info-list))))
                   (if file
                       (setq menu-item-text
                             (concat menu-item-text " - " file "(" (format "%d" line) ")"))))
                 (setq count (1- count))
                 (if (> count 0) (setq menu-item-text (concat menu-item-text "\n")))
                 )
               (popup-tip menu-item-text)))))

       (defun flymake-display-err-check-moved (cur-line cur-col)
         (let* ((is-not-moved (and flymake-display-err-before-line
                                   flymake-display-err-before-colmun
                                   (= cur-line flymake-display-err-before-line)
                                   (= cur-col flymake-display-err-before-colmun))))
           (setq flymake-display-err-before-line cur-line
                 flymake-display-err-before-colmun cur-col)
           (not is-not-moved)))

       (global-set-key "\M-e"
                       '(lambda ()
                          (interactive)
                          (let ()
                            (message "next error")
                            (flymake-goto-next-error)
                          (flymake-display-err-menu-for-current-line))))
     (global-set-key "\M-E"
                     '(lambda ()
                        (interactive)
                        (let ()
                          (message "prev error")
                          (flymake-goto-prev-error)
                          (flymake-display-err-menu-for-current-line))))

     (unless flymake-display-err-timer
       (setq flymake-display-err-timer
             (run-with-idle-timer flymake-display-err-delay
                                  t
                                  'flymake-display-err-menu-for-current-line)))

     ;; (defvar flymake-display-err-delay 0.5
     ;;  "delay to display flymake error message ")
     ;; (defvar flymake-display-err-timer nil
     ;;  "timer for flymake-display-err-menu-for-current-line")

     ;; (defun flymake-display-err-set-timer ()
     ;;  (unless flymake-display-err-timer
     ;;  (setq flymake-display-err-timer
     ;;        (run-with-idle-timer flymake-display-err-delay
     ;;                          nil
     ;;                          'flymake-display-err-menu-for-current-line))))

     ;; (defun flymake-display-err-cancel-timer ()
     ;;  (when (timerp flymake-display-err-timer)
     ;;  (cancel-timer flymake-display-err-timer)
     ;;  (setq flymake-display-err-timer nil)))
     )))

;; anything
(when (and (kui/package-require 'anything nil nil t)
           (kui/package-require 'anything-obsolete nil nil t)
           (kui/package-require 'anything-config nil nil t)
           (kui/package-require 'anything-match-plugin nil nil t)
           (kui/package-require 'anything-complete nil nil t))
  (global-set-key "\C-xa" 'anything-apropos)
  (global-set-key "\C-x\C-f" 'anything-find-file)
  (global-set-key "\C-xb" 'anything-buffers+)
  (global-set-key "\C-o" 'anything-occur)
  (global-set-key "\M-i" 'anything-imenu)
  (global-set-key "\M-x" 'anything-M-x)
  )

;; whitespace-mode
(when (require 'whitespace nil t)
  ;; n 列以上はハイライトで警告
  ;; (setq whitespace-line-column 90)

  (setq whitespace-style
        '(face ;; faceを使って視覚化する。
          ;; 行末の空白
          trailing
          ;; 長すぎる行のうち whitespace-line-column 以降部分をハイライト
          ;; lines-tail
          ;; タブ
          tabs
          tab-mark
          ;; タブの前にあるスペース
          ;; space-before-tab
          ;; タブの後にあるスペース
          ;; space-after-tab
          ))

  ;; デフォルトで有効にする。
  (global-whitespace-mode 1))

;; gnu global (gtags)
(when (require 'gtags nil t)
  (global-set-key "\M-tg" nil)
  (global-set-key "\M-tgt" 'gtags-find-tag)
  (global-set-key "\M-tgr" 'gtags-find-rtag)
  (global-set-key "\M-tgs" 'gtags-find-symbol)
  (global-set-key "\M-tgv" 'gtags-find-symbol)
  (global-set-key "\M-tgf" 'gtags-find-file)
  (global-set-key "\M-tgb" 'gtags-pop-stack)
  (global-set-key "\M-tgp" 'gtags-pop-stack)
  )

;; ctag-update.el 自動で TAGS アップデートしてくれる
(when (kui/package-require 'ctags-update nil nil t)
  (set 'ctags-update-command
       (kui/find-if-executable '("ctags-exuberant"
                                 "exuberant-ctags"
                                 "ctags")))
  )

;; -------------------------------------------------------------------------
;; メジャーモードの設定や読み込み

;; markdown-mode
;; 読み込めたら *scratch* に使うから kui/autoload-if-exist じゃなくて require
(when (kui/package-require 'markdown-mode nil nil t)

  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  ;;(define-key markdown-mode-map "\C-i" 'indent-for-tab-command)
  ;;(define-key markdown-mode-map "TAB" 'indent-for-tab-command)

  (defun kui/markdown-init-set-values ()
    (set (make-variable-buffer-local 'indent-tabs-mode) t)
    (set (make-variable-buffer-local 'tab-width) 4)
    (remove-hook 'before-save-hook
                 'delete-trailing-whitespace t)
    (set (make-variable-buffer-local 'whitespace-style)
         '(;; faceを使って視覚化する。
           face
           ;; タブ
           tabs
           tab-mark
           ;; タブの前にあるスペース
           space-before-tab
           ;; タブの後にあるスペース
           space-after-tab
           )))
  (add-hook 'markdown-mode-hook 'kui/markdown-init-set-values)

  ;; *scratch* 関連を更新
  (setq
   ;; *scratch* の major-mode
   initial-major-mode 'markdown-mode

   ;; *scratch* の初期文字列
   initial-scratch-message "Scratch\n========\n\n")
  )

;; yaml-mode
(when (kui/autoload-if-exist 'yaml-mode "yaml-mode"
                         "Major mode for editing yaml files" t)

  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (eval-after-load "yaml-mode"
    '(let ()
      ;; yaml-mode 読み込まれた時に評価される
      )))

;; ruby-mode
(when (kui/autoload-if-exist 'ruby-mode "ruby-mode")

  (add-to-list 'auto-mode-alist '("/Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("/config\\.ru\\'" . ruby-mode))

  (eval-after-load "ruby-mode"
    '(let nil
       (setq ruby-deep-indent-paren nil)

       (when (require 'flymake-ruby nil t)
         (add-hook 'ruby-mode-hook 'flymake-ruby-load))

       (let ((rhome (expand-file-name "~/.settings/src/rsense-0.3")))
         (when (file-directory-p rhome)
           (setq rsense-home rhome)
           (add-to-list 'load-path (concat rsense-home "/etc"))
           (message "try to load rsense")
           (when (require 'rsense nil t)
             (message "done (rsense)")
             (add-hook 'ruby-mode-hook
                       (lambda ()
                         (add-to-list 'ac-sources 'ac-source-rsense-method)
                         (add-to-list 'ac-sources 'ac-source-rsense-constant))))))
       (message "load ruby-mode")
       ))
  )

;; coffee-mode
(when (kui/autoload-if-exist 'coffee-mode "coffee-mode"
                             "Major mode for editing coffescript files" t)

  (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
  (add-to-list 'auto-mode-alist '("/Cakefile\\'" . coffee-mode))

  (add-hook 'coffee-mode-hook
            (lambda ()
              (when (require 'col-highlight nil t)
                (column-highlight-mode))
              ))

  (eval-after-load "coffee"
    '(let* ((coffee-command "coffee"))
       ;; coffee-mode が読み込まれた時に評価される

       (add-to-list 'ac-modes 'coffee-mode)

       ;; タブ幅
       (setq coffee-tab-width 2)

       ;; flymake
       (when (and (require 'flymake nil t)
                  (require 'flymake-coffeescript nil t)
                  (executable-find flymake-coffeescript-command))
         (add-hook 'coffee-mode-hook 'flymake-coffeescript-load))

       (setq coffee-debug-mode t)

       ;; 独自インデント
       ;; インデントの先頭に移動してからじゃないと、
       ;; insert-tab しない
       (defun kui/coffee-indent-line ()
         "Indent current line as CoffeeScript."
         (interactive)
         (let ((old-point nil)
               (new-point nil))
           (save-excursion
             (set 'old-point (point))
             (back-to-indentation)
             (set 'new-point (point)))

           (if (< old-point new-point)
               (back-to-indentation)
             (coffee-indent-line))
           ))
       (add-hook 'coffee-mode-hook
                 '(lambda ()
                    (set (make-local-variable 'indent-line-function)
                         'kui/coffee-indent-line)))
       )))

;; css-mode
(setq css-indent-offset 2)

;; scss-mode
(when (kui/autoload-if-exist 'scss-mode "scss-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (eval-after-load "scss-mode"
    '(let nil
       (setq scss-compile-at-save nil))))

;; js-mode
(when (kui/autoload-if-exist 'js-mode "js")
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
  (eval-after-load "js"
    '(setq js-indent-level 2)))

;; -------------------------------------------------------------------------
;; 色とか
(when (kui/package-require 'color-theme nil nil t)
  (color-theme-initialize)

  (when (kui/package-require 'color-theme-sanityinc-tomorrow)
    (color-theme-sanityinc-tomorrow-night)
    (set-face-attribute 'show-paren-match nil
                        :inverse-video nil
                        :bold t
                        :foreground nil
                        :background "#000000"
                        :underline t)
    (set-face-attribute 'anything-header nil
                        :inverse-video t
                        :bold t
                        :height 1.2)
    (set-face-attribute 'highlight nil
                        :inverse-video t
                        :foreground "#81a2be"
                        :background "#1d1f21")
    (set-face-attribute 'hl-line nil
                        :background "#113333"
                        :inherit nil)
    (unless window-system
      (set-face-attribute 'mode-line nil
                          :background "#444444")
      (set-face-attribute 'hl-line nil
                          :background "#262626"
                          :inherit nil)))

  (when nil ;(locate-library "color-theme-twilight")
    (load-library "color-theme-twilight")
    (color-theme-twilight)
    (when (require 'anything nil t)
      (set-face-attribute 'highlight nil
                          :background "#191970"
                          :bold t)
      (set-face-attribute 'anything-header nil
                          :height 1.3
                          :foreground "white"
                          :background "#4169e1"
                          :bold t)
      (set-face-attribute 'anything-match nil
                          :foreground nil
                          :background "#8b8b00"
                          :bold t))
    (set-face-attribute 'hl-line nil
                        :background "#191970")
    (set-face-attribute 'region nil
                        :background "#4169e1")
    (set-face-attribute 'show-paren-match nil
                        :background "#2e8b57")
    (set-face-attribute 'font-lock-comment-face nil
                        :foreground "#cd6600")
    (set-face-attribute 'font-lock-keyword-face nil
                        :foreground "#ff6eb4")
    (set-face-attribute 'markdown-header-face nil
                        :height 1.1
                        :foreground "#87ceff"
                        :bold t)
    (set-face-attribute 'markdown-header-rule-face nil
                        :inherit 'markdown-header-face)
    (set-face-attribute 'markdown-header-delimiter-face nil
                        :inherit 'markdown-header-face)
    (set-face-attribute 'whitespace-tab nil
                        :background "#1f1f1f"))
  )

;; -------------------------------------------------------------------------
;; window system がある時
(when window-system
  ;; カーソルの色
  (set-cursor-color "green")

  ;; ツールバーの表示
  (tool-bar-mode -1)

  ;; スクロールバーを消す(nil:消える,right:右側)
  (set-scroll-bar-mode "right")

  ;; フォントの指定
  (set-default-font "Inconsolata-12")

  ;; ウィンドウサイズを画面に揃える（精度は微妙）
  (set-frame-size
   (selected-frame)
   (max (min (/ (/ (display-pixel-width) 2) (frame-char-width))
             kui/max-colmun-number)
        kui/min-colmun-number)
   (/ (display-pixel-height) (frame-char-height)))
  )
