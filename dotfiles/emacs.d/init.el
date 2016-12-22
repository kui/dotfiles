;; init.el --- kui configs -*- coding: utf-8-unix -*-
;;; Commentary:
;; nyan nyan
;;; Code:
(message "Start init.el %s" (current-time-string))
(defvar kui/init-start-time (float-time))

(require 'cl-lib)

(put 'kui/append-to-list 'lisp-indent-function 1)
(defmacro kui/append-to-list (list &rest elements)
  "Add all to LIST.  ELEMENTS is a list of added element."
  `(dolist (item (list ,@elements))
     (add-to-list ,list item)))

(kui/append-to-list 'load-path
  (locate-user-emacs-file "site-lisp"))

(require 'package)
(kui/append-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/")
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; 言語設定は環境変数に依存
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;; dired などでマルチバイト文字が化ける
(setq default-file-name-coding-system 'utf-8-unix)

;; meadow 向けの設定
;; (if (string-equal system-type "windows-nt") (let () ))

;; git checkout によるファイル変更などに追従する
;; (global-auto-revert-mode 1)

;; 自動保存ファイルの保存先ディレクトリ
(defconst emacs-bk-dir "~/.emacs-bk/")
(mkdir emacs-bk-dir t)

;; 自動保存機能
(setq auto-save-default t
      ;; 自動保存に関する情報
      auto-save-list-file-name (expand-file-name "~/.emacs-auto-save-list")
      ;; 自動保存する打鍵回数
      auto-save-interval 50
      ;; 自動保存する時間
      auto-save-timeout 10
      ;; 自動保存ファイルの保存先
      backup-directory-alist `((".*" . ,emacs-bk-dir))
      auto-save-file-name-transforms `((".*" ,emacs-bk-dir t))
      auto-save-list-file-prefix emacs-bk-dir)

;; http://emacsformacosx.com/ 向け設定
(when (string= window-system "ns")
  (setq ns-command-modifier   'meta
        ns-alternate-modifier 'super))

;; disable electric-pair-mode
(setq electric-pair-mode nil)

;; ロックファイル(.#で始まるファイル) を無効化
(setq create-lockfiles nil)

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
(setq transient-mark-mode t)

;; 現在の行をハイライト
(global-hl-line-mode)

;; -------------------------------------------------------------------------
;; グローバルキーバインド変更

;; 不要なキーバインド削除
(global-unset-key "\C-\\") ;; 入力モード切り替え
(global-unset-key "\C-t")  ;; 文字入れ替え
(global-unset-key "\M-m") ;; インデント先頭にジャンプ

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

;; 直前のカーソル位置に戻る
(global-set-key "\M-[" 'pop-global-mark)

;; -------------------------------------------------------------------------
;; 自作関数

;; このファイル(init.el)を開く
(defun kui/find-init-file ()
  "Edit init file.
Switch to a buffer visiting init file."
  (interactive)
  (find-file-existing user-init-file))

;; sudo で開き直す
(defun kui/reopen-with-sudo ()
  "Reopen current buffer-file with sudo using tramp."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (find-alternate-file (concat "/sudo::" file-name))
      (error "Cannot get a file name"))))

;; C-w をもう少し賢く
(defun kui/backward-kill-word-or-kill-region ()
  "Better `backward-kill-word'."
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key "\C-w" 'kui/backward-kill-word-or-kill-region)

(defun kui/backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key "\C-u" 'kui/backward-kill-line)

;; インデント先頭に移動
;; インデント先頭時は行頭移動
;; 行頭時は何もしない （要するに eclipse 風）
(defun kui/move-beginning-of-line ()
  "`back-to-indentation' but execute `move-beginning-of-line' \
if point is in indentation or nothing if point is in BoL."
  (interactive)
  (unless (= (point) (point-at-bol))
    (let ((old-point (point)))
      (back-to-indentation)
      (if (= old-point (point))
          (move-beginning-of-line nil)))))
(global-set-key "\C-a" 'kui/move-beginning-of-line)

;; 現在の行をコメントアウト
(defun kui/comment-or-uncomment-current-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

;; region ある時は、そのリージョンをコメントアウト
;; region ない時は、現在行をコメントアウト
(defun kui/comment-or-uncomment ()
  "Comment or uncomment region, but if region is not active, comment or \
uncomment the current line."
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (comment-or-uncomment-region (region-beginning) (region-end))
    (kui/comment-or-uncomment-current-line)))
(global-set-key (kbd "C-;") 'kui/comment-or-uncomment)

;; マジックコメント挿入
(defun kui/insert-magic-comment ()
  "Insert magic comment with current coding & `major-mode'."
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
  "Kill the current buffer without interaction."
  (interactive)
  (kill-buffer nil))
(global-set-key "\C-xk" 'kui/kill-buffer-without-interaction)

;; フルスクリーン状態をトグル
(defun kui/toggle-fullscreen ()
  "Toggle full screen."
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
  (cl-find-if 'executable-find seq))

(defun kui/find-buffer-if (func)
  "Return buffer if FUNC return non-nil.
Return nil if FUNC did not return non-nil with any buffer."
  (cl-find-if func (buffer-list)))

;; bname って名前のバッファを返す。そんなバッファ無い時は nil。
(defun kui/find-buffer-by-name (bname)
  "Return buffer named BNAME.
Return nil if not found BNAME buffer."
  (kui/find-buffer-if
   (lambda (b) (string-equal bname (buffer-name b)))))
;; (kui/find-buffer-by-name "*scratch*")

(defun kui/find-buffer-by-name-regexp (bname-regexp)
  "Return buffer named a name matched BNAME-REGEXP.
Return nil if not found BNAME buffer."
  (kui/find-buffer-if
   (lambda (b) (string-match-p bname-regexp (buffer-name b)))))

;; *scratch* バッファに切り替え（消してしまっていたら作成）
(defun kui/switch-to-scratch-buffer ()
  "Switch to *scratch*.  create *scratch* if it did not exists."
  (interactive)
  (if (kui/find-buffer-by-name "*scratch*")
      (switch-to-buffer "*scratch*")
    (switch-to-buffer "*scratch*")
    (insert initial-scratch-message)))

(defun kui/active-minor-mode-p (mode)
  "Return MODE if MODE was activate on the current buffer, \
but if not, return nil."
  (not (and (boundp mode)
            (symbol-value mode)
            (fboundp (or (get mode :minor-mode-function) mode)))))

(defun kui/current-minor-modes ()
  "Return minor modes on the current buffer."
  (remove-if 'kui/active-minor-mode-p
             minor-mode-list))

(defun kui/find-font (&rest fonts)
  "Return an existing font which was find at first in FONTS."
  (cl-find-if (lambda (f)
                (find-font (font-spec :name f)))
              fonts))

(defun kui/revert-buffer ()
  "Execute `revert-buffer' without confimations if it was not edited."
  (interactive)
  (if (not (buffer-modified-p))
      (let ()
        (revert-buffer t t)
        (message "revert the current buffer"))
    (error "ERROR: The buffer has been modified")))
(global-set-key (kbd "<f5>")
                'kui/revert-buffer)

;; これ相当のもの、最初からありそうな気がする
(defun kui/try-symbol-value (symbol &optional fallback-value)
  "Return `symbol-value' if the SYMBOL dosenot exists, or Return FALLBACK-VALUE."
  (condition-case err (symbol-value symbol) (error fallback-value)))

(defun kui/add-to-list-if-exist (list-var element &optional append compare-fn)
  "Execute (add-to-list LIST-VAR ELEMENT APPEND COMPARE-FN), if LIST-VAR was defined as an appendable value."
  (let ((v (kui/try-symbol-value list-var)))
    (if (consp v)
        (add-to-list list-var element append compare-fn))))

(defun kui/chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

;; -------------------------------------------------------------------------
;; 便利な感じのマイナーモード

;; マイナーモードは必ずインストールする
(setq use-package-always-ensure t)

;; 対応する括弧のハイライト
(use-package paren
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'mixed))

;; 環境変数をシェルからインポート
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; git の変更行を表示
(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (use-package git-gutter-fringe) ;; fringe を使って表示
  )

;; git-blame
;; (use-package git-blame)

;; popwin
(use-package popwin
  :config
  (popwin-mode)
  (kui/append-to-list 'popwin:special-display-config
    '("*Buffer List*" :position :bottom :height 20 :dedicated t :tail t)
    )
  )

;; プロジェクトルートからディレクトリ表示
(use-package direx-project
  :ensure direx
  :config
  (kui/append-to-list 'popwin:special-display-config
    '(direx:direx-mode :position :left :width 40 :dedicated t))
  (defun kui/jump-to-project-directory-other-window-if-in-project ()
    "Open directory tree from project root"
    (interactive)
    (if (direx-project:find-project-root-noselect (or buffer-file-name default-directory))
        (direx-project:jump-to-project-root-other-window)
      (direx:jump-to-directory-other-window)))
  (global-set-key (kbd "M-j") 'kui/jump-to-project-directory-other-window-if-in-project)
  )

;; editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

;; 2ストロークキーのプレフィックスキーを入力したときの次のキー一覧表示
(use-package guide-key
  :config
  (guide-key-mode 1)
  (setq
   guide-key/guide-key-sequence '("M-t" "C-c" "C-x RET" "C-x C-h" "C-x r" "M-m")
   guide-key/popup-window-position 'bottom
   guide-key/polling-time 0.5
   guide-key/recursive-key-sequence-flag t
   )
  )

;; スタイルやシンタックスチェックする
(use-package flycheck
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  ;; Do not use flycheck-pos-tip, it seems not to work right on Mac
  ;;(kui/with-pkg 'flycheck-pos-tip)

  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  (kui/append-to-list 'popwin:special-display-config
    '(flycheck-error-list-mode :position :bottom :height 20 :dedicated t))
  (global-set-key (kbd "M-e") 'flycheck-list-errors)
  )

;; 補完
(use-package company
  :defer t
  :init
  ;; 現状使わなくなってしまったのでロードするタイミングなし
  :config
  (setq company-global-modes nil
        ;; 自動補完を有効
        company-auto-complete t
        ;; 補完リスト表示字数
        company-minimum-prefix-length 2
        )

  ;; 補完開始
  (define-key company-mode-map "\M-/" 'company-complete)

  ;; 補完リスト表示時
  (define-key company-active-map "\C-[" 'company-abort)
  (define-key company-active-map "\C-n" 'company-select-next)
  (define-key company-active-map "\C-p" 'company-select-previous)
  (define-key company-active-map "\C-h" (lambda ()
                                          (interactive)
                                          (company-abort)
                                          (delete-char 1)))
  )

;; 補完
(use-package auto-complete-config
  :ensure auto-complete
  :config
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
  (define-key ac-mode-map "\M-/" 'ac-start)

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

(use-package helm
  :bind (("C-x C-f" . helm-find-files)
         ("C-x a" . helm-apropos)
         ("C-x b" . helm-buffers-list)
         ("M-i" . helm-buffers-list)
         ("M-o" . helm-imenu)
         ("C-o" . helm-imenu)
         ("C-s" . helm-occur)
         ("M-x" . helm-M-x))
  :bind (:map helm-map
              ("C-h" . delete-backward-char)
              ("C-w" . kui/backward-kill-word-or-kill-region)
              ("C-u" . kui/backward-kill-line)
              ("TAB" . helm-execute-persistent-action))
  :config
  (setq
   ;; helm-buffer-max-length 20
   )

  (use-package helm-swoop
    :bind ("C-s" . helm-swoop)
    :bind (:map helm-swoop-map
                ("C-w" . kui/backward-kill-word-or-kill-region)))
  )

(use-package whitespace
  :config
  ;; n 列以上はハイライトで警告
  ;; (setq whitespace-line-column 90)

  (setq whitespace-style
        '(face ;; faceを使って視覚化する。
          trailing ;; 行末の空白
          ;; lines-tail ;; 長すぎる行のうち whitespace-line-column 以降部分をハイライト
          tabs
          tab-mark
          space-before-tab
          space-after-tab
          ))

  ;; デフォルトで有効にする。
  (global-whitespace-mode 1)
  (setq whitespace-action '(auto-cleanup))
  (defun kui/whitespace-auto-cleanup-enable ()
    (interactive)
    (setq-local whitespace-action '(auto-cleanup)))
  (defun kui/whitespace-auto-cleanup-disable ()
    (interactive)
    (setq-local whitespace-action nil))
  )

;; ctag-update.el 自動で TAGS アップデートしてくれる
(use-package ctags-update
  :config
  (set 'ctags-update-command
       (kui/find-if-executable '("ctags-exuberant"
                                 "exuberant-ctags"
                                 "ctags")))
  )

;; 一度に複数のカーソルを操作
(use-package multiple-cursors
  :bind (("M-m n" . mc/mark-next-like-this)
         ("M-m p" . mc/mark-previous-like-this)
         ("M-m a" . mc/mark-all-like-this)
         ("M-m i" . mc/mark-more-like-this-extended))
  :init
  (multiple-cursors-mode)
  )

;; -------------------------------------------------------------------------
;; メジャーモードの設定や読み込み

;; メジャーモードのインストールはオプショナル
(setq use-package-always-ensure nil)

(use-package emacs-lisp-mode
  :no-require t
  :bind (:map emacs-lisp-mode-map
              ("C-c f f" . find-function)
              ("C-c f v" . find-variable)
              ("C-c f l" . find-library)
              ("C-c d f" . describe-function)
              ("C-c d v" . describe-variable))
  )

;; http://emacs-jp.github.io/programming/golang.html
(use-package go-mode
  :no-require t
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("M-," . pop-tag-mark))
  :config
  (use-package go-autocomplete :ensure t)
  (use-package helm
    :config
    (defvar kui/helm-go-source
      '((name . "Helm Go")
        (candidates . (lambda ()
                        (cons "builtin" (go-packages))))
        (action . (("Import package" . kui/helm-go-import-add)
                   ("Show document" . godoc)
                   ))))
    (defun kui/helm-go-import-add (candidate)
      (dolist (package (helm-marked-candidates))
        (go-import-add current-prefix-arg package)))
    (defun kui/helm-go ()
      (interactive)
      (helm :sources '(kui/helm-go-source) :buffer "*helm go*")))
  (defun kui/go-init ()
    (setq-local tab-width 4)
    (setq-local whitespace-style '(face tab-mark tabs)))
  (add-hook 'go-mode-hook 'kui/go-init)
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package markdown-mode
  :no-require t
  :ensure t ;; *scratch* で使うため
  :config
  (defun kui/markdown-init ()
    ;; (setq-local indent-tabs-mode nil)
    (setq-local tab-width 4)
    (setq-local whitespace-style
                '(;; faceを使って視覚化する。
                  face
                  ;; タブ
                  tabs
                  tab-mark
                  ;; タブの前にあるスペース
                  space-before-tab
                  ;; タブの後にあるスペース
                  space-after-tab
                  ))
    (electric-indent-local-mode -1))
  (add-hook 'markdown-mode-hook 'kui/markdown-init)

  ;; *scratch* 関連を更新
  (setq
   ;; *scratch* の major-mode
   initial-major-mode 'markdown-mode
   ;; *scratch* の初期文字列
   initial-scratch-message "Scratch\n========\n\n"
   )
  )

(use-package ruby-mode
  :no-require t
  :mode "\\.gemspec\\'"
  :mode "/config\\.ru\\'"
  :mode "\\(Rake\\|Gem\\|Thor\\|Berks\\|Vagrant\\)file\\'"
  :mode "\\.builder\\'"
  :config
  (setq ruby-deep-indent-paren nil)
  (defun kui/ruby-init ()
    (flycheck-mode)
    ;; (electric-pair-mode t)
    (electric-indent-mode t))
  (add-hook 'ruby-mode-hook 'kui/ruby-init)

  (use-package robe
    :ensure t
    :config
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'robe-mode-hook 'ac-robe-setup))

  (when (and (executable-find "rbenv")
             (kui/package-require 'rbenv))
    (add-hook 'ruby-mode-hook 'global-rbenv-mode))

  (use-package ruby-block
    :ensure t
    :config
    (setq ruby-block-highlight-toggle 'overlay)
    (add-hook 'ruby-mode-hook '(lambda () (ruby-block-mode t))))
  )

(use-package coffee-mode
  :no-require t
  :config
  (setq coffee-tab-width 2)
  (setq coffee-debug-mode t)

  (kui/add-to-list-if-exist 'ac-modes 'coffee-mode)

  (use-package col-highlight
    :ensure t
    :config
    (add-hook 'coffee-mode-hook 'column-highlight-mode))

  ;; flymake
  ;; (when (and (kui/package-require 'flymake-coffeescript)
  ;;            (executable-find flymake-coffeescript-command))
  ;;   (add-hook 'coffee-mode-hook 'flymake-coffeescript-load))

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
            (lambda ()
               (set (make-local-variable 'indent-line-function)
                    'kui/coffee-indent-line)))
  )

(use-package coffee-mode
  :no-require t
  :config
  (setq css-indent-offset 2)
  (kui/add-to-list-if-exist 'ac-modes 'css-mode)
  )

(use-package scss-mode
  :no-require t
  :config
  (setq scss-compile-at-save nil))

(use-package js-mode
  :no-require t
  :config
  (setq js-indent-level 2))
(use-package js2-mode
  :no-require t
  :config
  (js2-mode-hide-warnings-and-errors)
  (add-to-list 'auto-mode-alist '("\\.js\\'"  . js2-mode)))
(use-package "flycheck"
  :config
  (defun kui/use-node-modules-bin ()
    (let* ((local-path (kui/chomp-end (shell-command-to-string "npm bin"))))
      (setq-local exec-path (cons local-path exec-path))))
  (defun kui/flycheck-init-js ()
    (kui/use-node-modules-bin)
    ;; Disable jshint if eslint enabled
    (when (and (flycheck-may-use-checker 'javascript-eslint)
               (flycheck-may-use-checker 'javascript-jslint))
      (message "disale jshint on flycheck")
      (flycheck-disable-checker 'javascript-jshint))
    (when (executable-find "flow")
      (use-package flycheck-flow :ensure t)))

  (add-hook 'js-mode-hook 'kui/flycheck-init-js)
  (when (featurep 'js2-mode)
    (add-hook 'js2-mode-hook 'kui/flycheck-init-js))
  )
(defun kui/eslint-fix (file)
  "Execute eslint --fix FILE."
  (iteractive)
  (shell-command (concat "eslint --fix " (buffer-file-name)))
  (revert-buffer t t t))

(use-package html-mode
  :no-require t
  :config
  (kui/add-to-list-if-exist 'ac-modes 'html-mode))

(use-package multi-web-mode
  :no-require t
  ;; :mode "\\.html?\\'"
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script>" "</script>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)>"
                             "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1)
  )

(use-package web-mode
  :no-require t
  :mode "\\.html?\\'"
  :config
  (kui/add-to-list-if-exist 'ac-modes 'web-mode)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0)
  )

(use-package groovy-mode
  :no-require t
  :mode "\\.gradle\\'"
  :config
  (kui/add-to-list-if-exist 'ac-modes 'groovy-mode)
  (defun kui/set-groovy-indent ()
    (set (make-local-variable 'indent-tabs-mode) nil)
    (set (make-local-variable 'c-basic-offset) 4))
  (add-hook 'groovy-mode-hook 'kui/set-groovy-indent)
  )

(use-package hcl-mode
  :no-require t
  :mode "\\.tf\\'"
  :mode "\\.tfvars\\'")

(use-package haml-mode
  :no-require t
  :config
  (kui/add-to-list-if-exist 'ac-modes 'haml-mode)
  )

(use-package rust-mode
  :no-require t
  :config
  ;; flycheck for Cargo
  (use-package flycheck-rust
    :ensure t
    :config
    (when (featurep 'flycheck)
      (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
      (add-hook 'rust-mode-hook 'flycheck-mode)))
  )

(use-package typescript
  :no-require t
  :config
  (kui/add-to-list-if-exist 'ac-modes 'typescript-mode)
  (use-package tts
    :ensure t
    :config
    (setq tss-popup-help-key "C-:"
          tss-jump-to-definition-key "M-."
          tss-implement-definition-key "M-,")
    (tss-config-default)
    (add-hook 'typescript-mode-hook 'tss-setup-current-buffer t)
    (add-hook 'kill-buffer-hook 'tss--delete-process t))
  )

;; -------------------------------------------------------------------------
;; 見た目
(custom-set-faces
 '(hl-line ((((background light)) :background "#eeeeff"))))
(when (featurep 'git-gutter)
  (custom-set-faces
   '(git-gutter-fr:added ((t (:inherit (fringe git-gutter:added)))))
   '(git-gutter-fr:deleted ((t (:inherit (fringe git-gutter:deleted)))))
   '(git-gutter-fr:modified ((t (:inherit (fringe git-gutter:modified)))))
   ))

(when window-system
  ;; カーソルの色
  ;; (set-cursor-color "geeen")

  ;; カーソルの形
  (setq cursor-type 'bar)

  ;; ツールバーの表示
  (tool-bar-mode -1)

  ;; スクロールバーを消す(nil:消える,right:右側)
  (set-scroll-bar-mode nil)

  ;; フォントの指定
  (let ((font (kui/find-font "Ricty-11"
                             "Inconsolata-11"
                             "Monospace-11")))
    (if font (set-default-font font)))

  ;; ウィンドウサイズを画面に揃える（精度は微妙）
  (set-frame-size
   (selected-frame)
   (max (min (/ (/ (display-pixel-width) 2) (frame-char-width))
             kui/max-colmun-number)
        kui/min-colmun-number)
   (/ (display-pixel-height) (frame-char-height)))
  )

(message "End init.el %s" (current-time-string))
(message "Elapsed time: %s sec" (- (float-time) kui/init-start-time))
;;; init.el ends here
