;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-

(set-language-environment "Japanese")

(set-cursor-color "green")

;; meadow 向けの設定
; (if (string-equal system-type "windows-nt")    (let ()  ))

;; 自動保存機能
(setq auto-save-default t
      auto-save-list-file-name "~/.emacs-auto-save-list" ;; 自動保存に関する情報
      auto-save-intrval 50  ;; 自動保存する打鍵回数
      auto-save-timeout 10)  ;; 自動保存する時間

;; フォントロックモード (強調表示等) を有効にする
;; (global-font-lock-mode t)

;; マークしている範囲をハイライトする
(setq-default transient-mark-mode t)

;; 対応する括弧をハイライト
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; バッファ末尾に余計な改行コードを防ぐための設定
(setq next-line-add-newlines nil)

;; 縦分割の時、一行で表示しきれない時の挙動
;; nil: 改行、t: オーバーフロー
(setq truncate-partial-width-windows nil)

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

;; ウィンドウシステムを使っているとき
(if window-system
    (let ()

      ;; ツールバーの表示
      (tool-bar-mode -1)

      ;; スクロールバーを消す(nil:消える,right:右側)
      (set-scroll-bar-mode "right")

      ;; ウィンドウサイズを画面に揃える（精度は微妙）
      (add-to-list 'default-frame-alist
                   (cons 'height (/ (- (x-display-pixel-height) 50)
                                    (frame-char-height))))
      ))

;; BS で選択範囲を消す
(delete-selection-mode 1)

;; 現在の行をハイライト
(global-hl-line-mode)

;; 保存前に末尾空白の削除
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; パッケージ管理
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; -------------------------------------------------------------------------
;; グローバルキーバインド変更

;; 不要なキーバインド削除
(global-unset-key "\C-\\")	;; 入力モード切り替え
(global-unset-key "\C-t")	;; 文字入れ替え

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
(global-set-key "\M-o" 'list-tags)

;; goto-line を実行
(define-key ctl-x-map "l" 'goto-line)

;; -------------------------------------------------------------------------
;; 自作関数

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
        (move-beginning-of-line nil)))
  )
(global-set-key "\C-a" 'kui/move-beginning-of-line)

;; require の代わりに使う
(defun autoload-if-exist (function file &optional docstring interactive type)
  "autoload if FILE exist"
  (if (locate-library file)
      (let () (autoload function file docstring interactive type) t)
    ))

;; emacs 用マジックコメント挿入
(defun insert-magic-comment ()
  "insert magic comment with current coding & major-mode into current line."
  (interactive)
  (if (or buffer-file-coding-system major-mode)
      (insert
       (concat "-*- "
               (if buffer-file-coding-system
                   (concat "coding:"
                           (symbol-name buffer-file-coding-system)
                           "; "))
               (if major-mode
                   (concat "mode:"
                           (replace-regexp-in-string "-mode\\'" ""
                                                     (symbol-name major-mode))
                           "; "))
               "-*-\n"))
    (message "Both current coding and major-mode are nil.")))

;; 確認なしでバッファの削除
(defun kui/kill-buffer-with-no-confirmation ()
  (interactive)
  (kill-buffer nil))
(global-set-key "\C-xk" 'kui/kill-buffer-with-no-confirmation)

;; -------------------------------------------------------------------------
;; 便利な感じのマイナーモード

;; (when (require 'col-highlight)
;;   (col-highlight-set-interval 3)
;;   (col-highlight-toggle-when-idle 1)
;;   )

;; auto-complete-mode
;; http://cx4a.org/software/auto-complete/index.ja.html
;; (define-key ac-complete-mode-map "\M-/" 'ac-stop)
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)

  ;; ac-modes に登録されてるメジャーモード時に ac 発動
  (global-auto-complete-mode t)

  ;; *候補間を移動
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  ;; (define-key ac-complete-mode-map "\C-o" 'ac-next)
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

;; rsense
(setq rsense-home (expand-file-name "~/.emacs.d/src/rsense-0.3"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(and (require 'cl nil t) (require 'rsense nil t))

;; tabbar-mode
(when (require 'tabbar nil t)

  ;; tabbar のタブのグループの仕方
  ;;   デフォルト: 一部を除き major-mode ごとにタブをグループ化
  ;;   下: 全部同じグループに
  ;;       (http://www.emacswiki.org/emacs/TabBarMode)
  (setq tabbar-buffer-groups-function (lambda () (list "Buffers")))

  (when (require 'cl nil t) ;; remove-if とかに必要

    ;; 表示するタブのフィルタリング
    ;;   * で始まるバッファはタブに表示しない
    (setq tabbar-buffer-list-function
          (lambda ()
            (remove-if
             (lambda (b)
               (and (string-match "^ ?\\*" (buffer-name b))
                    (not (string-equal (buffer-name (current-buffer))
                                       (buffer-name b)))))
             (buffer-list)))))

  ;; 左に表示されるボタンを消す
  (dolist (button '(tabbar-buffer-home-button
                    tabbar-scroll-left-button
                    tabbar-scroll-right-button))
    (set button (cons (cons "" nil) (cons "" nil))))

  ;; 色とか
  (set-face-attribute 'tabbar-selected nil
                      :background "dim gray"
                      :foreground "white"
                      )

  ;; ウィンドウシステムを使っていないとき
  (when (not window-system)

    ;; タブの間に挟む文字
    (setq tabbar-separator-value "/")

    ;; faces
    (set-face-attribute 'tabbar-default nil
                        :background "brightblack" ;;"#333333"
                        :foreground "black" ;; "#000000"
                        :underline t
                        :box nil)
    (set-face-attribute 'tabbar-selected nil
                        :background "black"
                        :foreground "white"
                        :underline nil
                        :box nil)
    (set-face-attribute 'tabbar-unselected nil
                        :background "white" ;;"#333333"
                        :foreground "black" ;; "#cccccc"
                        :underline t
                        :box nil)
    (set-face-attribute 'tabbar-separator nil
                        :background "black")
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
(defvar flymake-display-err-delay 1
  "delay to display flymake error message ")
(defvar flymake-display-err-timer nil
  "timer for flymake-display-err-menu-for-current-line")
(defvar flymake-display-err-before-line nil)
(defvar flymake-display-err-before-colmun nil)

(eval-after-load "flymake"
  '(when (require 'popup nil t)

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
         ;; (message (concat "line:" cur-line ", col:" cur-col))
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

     ))

;; anything.el
(when (require 'anything-config nil t)
  (global-set-key "\C-xa" 'anything-c-apropos)
  (global-set-key "\C-xf" 'anything-for-files)
  (global-set-key "\C-xb" 'anything-buffers-list)
  )

;; whitespace-mode
(when (require 'whitespace nil t)
  ;; n 列以上はハイライトで警告
  (setq whitespace-line-column 90)

  (setq whitespace-style
        '(face ;; faceを使って視覚化する。
          ;; 行末の空白
          trailing
          ;; 長すぎる行のうち whitespace-line-column 以降部分
          lines-tail
          ;; タブ
          tabs
          ;; タブの前にあるスペース
          ;; space-before-tab
          ;; タブの後にあるスペース
          ;; space-after-tab
          ))

  (set-face-attribute 'whitespace-tab nil
                      :foreground nil
                      :background "#111111")

  ;; デフォルトで視覚化を有効にする。
  (global-whitespace-mode 1))

;; gnu global (gtags)
(when nil ;; (require 'gtags nil t)
  (global-set-key "\M-t" nil)
  (global-set-key "\M-tt" 'gtags-find-tag)
  (global-set-key "\M-tr" 'gtags-find-rtag)
  (global-set-key "\M-ts" 'gtags-find-symbol)
  (global-set-key "\M-tv" 'gtags-find-symbol)
  (global-set-key "\M-tf" 'gtags-find-file)
  (global-set-key "\M-tb" 'gtags-pop-stack)
  (global-set-key "\M-tp" 'gtags-pop-stack)
  )

;; ctag-update.el 自動で TAGS アップデートしてくれる
(when (require 'ctags-update nil t)
  ;; あとで、対象の *-mode-hook に、ctags-update-minor-mode をくっ付ける
  )

;; -------------------------------------------------------------------------
;; メジャーモードの設定や読み込み

;; markdown-mode
(when (autoload-if-exist 'markdown-mode "markdown-mode"
                         "Major mode for editing Markdown files" t)

  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (eval-after-load "markdown-mode"
    (let ()
      ;; markdown-mode 読み込まれた時に評価される
      ;;(define-key markdown-mode-map "\C-i" 'indent-for-tab-command)
      ;;(define-key markdown-mode-map "TAB" 'indent-for-tab-command)
      (add-hook 'markdown-mode-hook
                '(lambda ()
                   (set (make-variable-buffer-local 'indent-tabs-mode) t)
                   (set (make-variable-buffer-local 'tab-width) 4)

                   (remove-hook (make-variable-buffer-local 'before-save-hook)
                                'delete-trailing-whitespace)

                   (set (make-variable-buffer-local 'whitespace-style)
                        '(face ;; faceを使って視覚化する。
                          ;; タブ
                          tabs
                          ;; タブの前にあるスペース
                          space-before-tab
                          ;; タブの後にあるスペース
                          space-after-tab
                          ))

                   ))
      ))
  )

;; yaml-mode
(when (autoload-if-exist 'yaml-mode "yaml-mode"
                         "Major mode for editing yaml files" t)

  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (eval-after-load "yaml-mode"
    (let ()
      ;; yaml-mode 読み込まれた時に評価される
      )))

;; ruby-mode
(when '(require 'ruby-mode nil t)
  (add-to-list 'auto-mode-alist '("/Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (setq ruby-deep-indent-paren nil)

  ;; flymakeでrubyの構文チェック
  (when (require 'flymake-ruby nil t)
    (add-hook 'ruby-mode-hook 'flymake-ruby-load))

  (add-hook 'ruby-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-rsense-method)
              (add-to-list 'ac-sources 'ac-source-rsense-constant)
              ))
  )

;; coffee-mode
(when (autoload-if-exist 'coffee-mode "coffee-mode"
                         "Major mode for editing coffescript files" t)

  (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
  (add-to-list 'auto-mode-alist '("/Cakefile\\'" . coffee-mode))

  (add-hook 'coffee-mode-hook
            (lambda ()
              (when (require 'col-highlight)
                (column-highlight-mode))
              ))

  (eval-after-load "coffee"
    (let* ((coffee-command "coffee"))
      ;; coffee-mode が読み込まれた時に評価される
      (message "Load coffee-settings")
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

;; confluence-el
(when (require 'confluence nil t)

  ;; コンフルのAPIのURL
  (setq confluence-url "http://amewiki.cadc.cyberagent.local/rpc/xmlrpc")

  ;; パスワード一回入れたら以降聞かれないようにする
  (setq confluence-save-credentials t)

  ;; キーバインド設定
  (add-hook 'confluence-mode-hook
            '(lambda ()
               (local-set-key "\C-cc" 'confluence-create-page)
               (local-set-key "\C-cf" 'confluence-get-page)
               (local-set-key "\C-cm" 'confluence-ediff-merge-current-page)
               ))
  )

;; css-mode
(setq css-indent-offset 2)

;; js-mode
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
