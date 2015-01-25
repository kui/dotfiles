;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8-unix -*-
(message "Start init.el %s" (current-time-string))
(defvar kui/init-start-time (float-time))

(require 'cl-lib)

;; パッケージ管理
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; 言語設定は環境変数に依存
(set-language-environment nil)

;; dired などでマルチバイト文字が化ける
(setq default-file-name-coding-system 'utf-8-unix)

;; meadow 向けの設定
;; (if (string-equal system-type "windows-nt") (let () ))

;; git checkout によるファイル変更などに追従する
;; (global-auto-revert-mode 1)

;; 自動保存機能
(setq auto-save-default t
      ;; 自動保存に関する情報
      auto-save-list-file-name (concat user-emacs-directory "/auto-save-list")
      ;; 自動保存する打鍵回数
      auto-save-interval 50
      ;; 自動保存する時間
      auto-save-timeout 10)

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

;; -------------------------------------------------------------------------
;; 自作関数

;; pkg-alist から pkg-name のバージョンを取り出す
(defun kui/package-get-vers (pkg-name pkg-alist)
  "Return version of PKG-NAME in PKG-ALIST"
  (let ((pkg-desc (car (cdr (assoc pkg-name pkg-alist)))))
    (if pkg-desc (package-desc-version pkg-desc))))

;; インストール済みの pkg-name のバージョン
(defun kui/package-get-activated-vers (pkg-name)
  "Return activated version of PKG-NAME"
  (kui/package-get-vers pkg-name package-alist))

;; アーカイブにある最新の pkg-name のバージョン
(defun kui/package-get-latest-vers (pkg-name)
  "Return latest version of PKG-NAME in archives"
  (kui/package-get-vers pkg-name package-archive-contents))

;; pkg-name のアップデートがある時は t を返す
(defun kui/package-update-available-p (pkg-name)
  "Return t if PKG-NAME's update is available."
  (let ((installed-ver (kui/package-get-activated-vers pkg-name))
        (latest-ver (kui/package-get-latest-vers pkg-name)))
    (if latest-ver
        (version-list-< installed-ver latest-ver)
      (message "Cannot get latest version of package %s" pkg-name)
      nil)))

(defun kui/pop-as-alist (keys seq)
  "Pop head elements of SEQ as alist which has KEYS.

Example:

	(kui/pop-as-alist '(:a :b) '(:a 1 :b 2 :c 3))
	;; => ((:a . 1) (:b . 2))
	(kui/pop-as-alist '(:a :b) '(:d 1 :e 2 :a 3 :b 4))
	;; => nil"
  (let ((first (nth 0 seq)))
    (if (cl-find first keys)
        (cons `(,(nth 0 seq) . ,(nth 1 seq))
              (kui/pop-as-alist keys (nthcdr 2 seq)))
      nil)))

;; インストール済みでアップデート可能なパッケージをリストアップ
(defun kui/package-update-available-package-list ()
  "Return package list which have updates."
  (unless package--initialized (package-initialize t))
  (unless package-archive-contents (package-refresh-contents))
  (remove-if (lambda (pname) (not (kui/package-update-available-p pname)))
             package-activated-list))

;; アップデート可能なインストール済みパッケージ全てをアップデート
(defun kui/package-update-all-package (&optional non-interactive)
  "Update all package which is update-available with y-or-n interaction.
if NON-INTERACTIVE is non-nil, update all package without interaction."
  (interactive)
  (unless package--initialized (package-initialize t))
  (unless package-archive-contents (package-refresh-contents))
  (let ((pkg-list (kui/package-update-available-package-list)))
    (if (not pkg-list)
        (message "No update available package")
      (dolist (pkg-name pkg-list)
        (if (y-or-n-p (format "update package?: %s" pkg-name))
            (package-install pkg-name)))
      (package-initialize))))

;; インストールされていないパッケージを require した時に、
;; 自動でインストールしたあとに require してくれる
(put 'kui/package-require 'lisp-indent-function 1)
(defun kui/package-require (feature &rest args)
  "Install and require FEATURE with `package'. return non-nil,
if (requie future) success.

See also `kui/with-pkg'.

Examples:

	(when (kui/package-require 'git-gutter)
	  (global-git-gutter-mode t))

	(when (kui/package-require 'auto-complete-config
	                           :packagename 'auto-complete)
	  (ac-config-default)
	  (global-auto-complete-mode t))

	(when (kui/package-require 'typescript-mode
	                           :filename \"TypeScript\")
	  (add-hook 'typescript-mode-hook (lambda () ... )))"
  (let* ((opts (kui/pop-as-alist '(:file :package :throwerror) args))
         (fl (cdr (assoc :file opts)))
         (pkg (or (cdr (assoc :package opts)) feature))
         (err (cdr (assoc :throwerror opts))))
    (message "%s: feature=%s, file=%s, pacakge=%s, throwerror=%s"
             "kui/package-require" feature fl pkg err)
    (condition-case e
        (let ()
          (unless (package-installed-p pkg) (package-install pkg))
          (require feature fl))
      (error (if err (error (cadr e))
               (message "Error on %s: %s"
                        "kui/package-require" (cadr e))
               nil)))))

(put 'kui/with-pkg 'lisp-indent-function 1)
(defmacro kui/with-pkg (feature &rest args)
  "Install `package' and require FRETURE. Execute BODY, If FEATURE was found.

Examples:

	(kui/with-pkg 'git-gutter
	  (global-git-gutter-mode t))

	(kui/with-pkg 'auto-complete-config
	  :packagename 'auto-complete
	  (ac-config-default)
	  (global-auto-complete-mode t))

	(kui/with-pkg 'typescript-mode
	  :filename \"TypeScript\"
	  (add-hook 'typescript-mode-hook (lambda () ... )))"
  (let* ((opts (kui/pop-as-alist '(:file :package) args))
         (fname (cdr (assoc :file opts)))
         (pname (cdr (assoc :package opts)))
         (body (nthcdr (* 2 (length opts)) args)))
    `(when (kui/package-require ,feature :file ,fname :package ,pname) ,@body)))

(put 'kui/with-lib 'lisp-indent-function 1)
(defmacro kui/with-lib (file &rest body)
  "Execute BODY if FILE can be loaded as library.
This macro does not load FILE. So, you cannot set variables in FILE.
Use `kui/after-loaded'."
  `(when (locate-library ,file) ,@body))

;; 24.4 以降なら with-eval-after-loaded で同じことができる
(put 'kui/after-loaded 'lisp-indent-function 1)
(defmacro kui/after-loaded (file &rest body)
  "Execute BODY after the FILE loading"
  `(eval-after-load ,file (lambda () ,@body)))

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
    (let ((old-point (point)))
      (back-to-indentation)
      (if (= old-point (point))
          (move-beginning-of-line nil)))))
(global-set-key "\C-a" 'kui/move-beginning-of-line)

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
  (find-if 'executable-find seq))

(defun kui/find-buffer-if (func)
  "Return buffer if FUNC return non-nil.
Return nil if FUNC did not return non-nil with any buffer."
  (find-if func (buffer-list)))

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
  "switch to *scratch*.
create *scratch* if it did not exists"
  (interactive)
  (if (kui/find-buffer-by-name "*scratch*")
      (switch-to-buffer "*scratch*")
    (switch-to-buffer "*scratch*")
    (insert initial-scratch-message)))

(defun kui/active-minor-mode-p (mode)
  "Return MODE if MODE was activate on the current buffer,
but if not, return nil."
  (not (and (boundp mode)
            (symbol-value mode)
            (fboundp (or (get mode :minor-mode-function) mode)))))

(defun kui/current-minor-modes ()
  "Return minor modes on the current buffer"
  (remove-if 'kui/active-minor-mode-p
             minor-mode-list))

(defun kui/find-font (&rest fonts)
  "Return an existing font which was find at first"
  (find-if (lambda (f)
             (find-font (font-spec :name f)))
           fonts))

(defun kui/revert-buffer ()
  "Execute `revert-buffer' without confimations if it was not edited"
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
  (condition-case err (symbol-value symbol) (error fallback-value)))

(defun kui/add-to-list-if-exist (list-var element &optional append compare-fn)
  "Add ELEMENT to LIST-VAR if LIST-VAR is defined as a appendable value"
  (let ((v (kui/try-symbol-value list-var)))
    (if (consp v)
        (add-to-list list-var element append compare-fn))))

(defun kui/parent-directory (path &optional num)
  (let ((num (if num num 1)))
    (if (<= num 0)
        path
      (file-name-directory
       (directory-file-name (kui/parent-directory path (- num 1)))))))

;; -------------------------------------------------------------------------
;; 便利な感じのマイナーモード

;; 対応する括弧のハイライト
(kui/with-pkg 'paren
  (show-paren-mode 1)
  (setq show-paren-style 'mixed))

;; 環境変数をシェルからインポート
(kui/with-pkg 'exec-path-from-shell
  (exec-path-from-shell-initialize))

;; git-gutter
(kui/with-pkg 'git-gutter
  (global-git-gutter-mode t)

  (setq git-gutter:unchanged-sign " ")

  (global-set-key "\C-cgn" 'git-gutter:next-diff)
  (global-set-key "\C-cgp" 'git-gutter:previous-diff)
  (global-set-key "\C-cgo" 'git-gutter:popup-diff)
  )

;; git-blame
(kui/with-pkg 'git-blame
  ;; ...
  )

;; popup
(kui/with-pkg 'popup
  ;; ...
  )

;; popwin
(kui/with-pkg 'popwin
  (setq
   ;; display-buffer の置き換え
   display-buffer-function 'popwin:display-buffer

   ;; popwin がでてくる場所のデフォルト値
   ;; popwin:popup-window-position 'right
   )
  (add-to-list 'popwin:special-display-config
               '("*Buffer List*" :position :bottom :height 20 :dedicated t :tail t))
  )

;; direx
(when (and (kui/package-require 'direx)
           (featurep 'popwin))
  (push '(direx:direx-mode :position :left :width 40 :dedicated t)
        popwin:special-display-config)

  (require 'direx-project)
  (defun kui/jump-to-project-directory-other-window-if-in-project ()
    (interactive)
    (if (direx-project:find-project-root-noselect (or buffer-file-name default-directory))
        (direx-project:jump-to-project-root-other-window)
      (direx:jump-to-directory-other-window)))

  (global-set-key (kbd "M-j") 'kui/jump-to-project-directory-other-window-if-in-project)
  )

;; editorconfig
(kui/with-pkg 'editorconfig
  ;;...
  )

;; gude-key
(kui/with-pkg 'guide-key
  (setq
   guide-key/guide-key-sequence '("M-t" "C-c" "C-x RET" "C-x C-h" "C-x r" "M-m" "Esc m")
   guide-key/popup-window-position 'bottom
   guide-key/polling-time 0.5
   guide-key/popup-if-super-key-sequence t
   guide-key/recursive-key-sequence-flag t
   )

  (guide-key-mode 1))

(kui/with-pkg 'flycheck
  ;; ...
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (kui/with-pkg 'flycheck-pos-tip
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  (global-set-key "\M-e" 'flycheck-next-error)
  (global-set-key "\M-E" 'flycheck-previous-error)
  )

;; company-mode
(kui/with-pkg 'company

  (setq company-global-modes nil
        ;; 自動補完を有効
        company-auto-complete t
        ;; 補完リスト表示字数
        company-minimum-prefix-length 2)

  ;; 補完開始
  (define-key company-mode-map "\M-/" 'company-complete)

  ;; 補完リスト表示時
  (define-key company-active-map "\C-[" 'company-abort)
  (define-key company-active-map "\C-n" 'company-select-next)
  (define-key company-active-map "\C-p" 'company-select-previous)
  (define-key company-active-map "\C-h" (lambda ()
                                          (interactive)
                                          (company-abort)
                                          (delete-backward-char 1)))
  )

;; auto-complete-mode
(kui/with-pkg 'auto-complete-config
  :package 'auto-complete

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

(kui/after-loaded "flymake"
  (kui/with-pkg 'popup

    ;; flymake 現在行のエラーをpopup.elのツールチップで表示する
    ;; https://gist.github.com/292827
    (defvar flymake-display-err-delay 1
      "delay to display flymake error message ")
    (defvar flymake-display-err-timer nil
      "timer for flymake-display-err-menu-for-current-line")
    (defvar flymake-display-err-before-line nil)
    (defvar flymake-display-err-before-colmun nil)
    (defun flymake-display-err-menu-for-current-line ()
      (interactive)
      (let* ((line-no (line-number-at-pos))
             (line-err-info-list
              (nth 0 (flymake-find-err-info flymake-err-info line-no))))
        (when (and (flymake-display-err-check-moved line-no (current-column))
                   line-err-info-list)
          (setq flymake-display-err-before-line-no line-no)
          (let* ((count (length line-err-info-list))
                 (menu-item-text nil))
            (while (> count 0)
              (setq menu-item-text
                    (flymake-ler-text (nth (1- count) line-err-info-list)))
              (let* ((file (flymake-ler-file (nth (1- count)
                                                  line-err-info-list)))
                     (line (flymake-ler-line (nth (1- count)
                                                  line-err-info-list))))
                (if file
                    (setq menu-item-text
                          (concat menu-item-text " - " file "("
                                  (format "%d" line) ")"))))
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

    (unless flymake-display-err-timer
      (setq flymake-display-err-timer
            (run-with-idle-timer flymake-display-err-delay t
                                 'flymake-display-err-menu-for-current-line)))

    ;; (global-set-key "\M-e"
    ;;                 '(lambda ()
    ;;                    (interactive)
    ;;                    (message "next error")
    ;;                    (flymake-goto-next-error)
    ;;                    (flymake-display-err-menu-for-current-line)))
    ;; (global-set-key "\M-E"
    ;;                 '(lambda ()
    ;;                    (interactive)
    ;;                    (message "prev error")
    ;;                    (flymake-goto-prev-error)
    ;;                    (flymake-display-err-menu-for-current-line)))
    ))

(kui/with-pkg 'helm
  (setq
   helm-ff-transformer-show-only-basename nil
   helm-buffer-max-length 20)

  (global-set-key "\C-xa" 'helm-apropos)
  (global-set-key "\C-x\C-f" 'helm-find-files)
  (global-set-key "\C-xb" 'helm-buffers-list)
  (global-set-key "\C-o" 'helm-buffers-list)
  (global-set-key "\M-i" 'helm-imenu)
  (global-set-key "\C-s" 'helm-occur)
  (global-set-key "\M-x" 'helm-M-x)

  (kui/after-loaded 'helm
    (define-key helm-map "\C-h" 'delete-backward-char)
    (define-key helm-map "TAB" 'helm-execute-persistent-action))
  (kui/after-loaded 'helm-files
    (define-key helm-find-files-map "\C-h" 'delete-backward-char)
    (define-key helm-find-files-map "TAB" 'helm-execute-persistent-action))

  (kui/with-pkg 'helm-swoop
    (global-set-key "\C-s" 'helm-swoop))
  )

;; whitespace-mode
(kui/with-pkg 'whitespace
  ;; n 列以上はハイライトで警告
  ;; (setq whitespace-line-column 90)

  (setq whitespace-style
        '(face ;; faceを使って視覚化する。
          trailing ;; 行末の空白
          ;; lines-tail ;; 長すぎる行のうち whitespace-line-column 以降部分をハイライト
          tabs
          tab-mark
          ;; space-before-tab
          ;; space-after-tab
          ))

  ;; デフォルトで有効にする。
  (global-whitespace-mode 1)
  (setq whitespace-action '(auto-cleanup))
  (defun kui/whitespace-auto-cleanup-enable ()
    (interactive)
    (set (make-local-variable 'whitespace-action) '(auto-cleanup)))
  (defun kui/whitespace-auto-cleanup-disable ()
    (interactive)
    (set (make-local-variable 'whitespace-action) nil))
  )

;; gnu global (gtags)
(kui/with-lib "gtag"
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
(kui/with-pkg 'ctags-update
  (set 'ctags-update-command
       (kui/find-if-executable '("ctags-exuberant"
                                 "exuberant-ctags"
                                 "ctags")))
  )

(kui/with-pkg 'multiple-cursors
  (multiple-cursors-mode)
  (global-set-key "\M-mn" 'mc/mark-next-like-this)
  (global-set-key "\M-mp" 'mc/mark-previous-like-this)
  (global-set-key "\M-ma" 'mc/mark-all-like-this)
  (global-set-key "\M-mi" 'mc/mark-more-like-this-extended)
  )

;; -------------------------------------------------------------------------
;; メジャーモードの設定や読み込み

;; lisp-interaction-mode の設定
(define-key lisp-interaction-mode-map "\C-cff" 'find-function)
(define-key lisp-interaction-mode-map "\C-cfv" 'find-variable)
(define-key lisp-interaction-mode-map "\C-cfl" 'find-library)
(define-key lisp-interaction-mode-map "\C-cdf" 'describe-function)
(define-key lisp-interaction-mode-map "\C-cdv" 'describe-variable)

;; markdown-mode
;; 読み込めたら *scratch* に使うから kui/autoload-if-exist じゃなくて require
(kui/with-pkg 'markdown-mode
  (defun kui/markdown-init-set-values ()
    ;; (set (make-variable-buffer-local 'indent-tabs-mode) nil)
    (set (make-variable-buffer-local 'tab-width) 4)
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
           ))
    (electric-indent-local-mode -1))
  (add-hook 'markdown-mode-hook 'kui/markdown-init-set-values)

  ;; *scratch* 関連を更新
  (setq
   ;; *scratch* の major-mode
   initial-major-mode 'markdown-mode

   ;; *scratch* の初期文字列
   initial-scratch-message "Scratch\n========\n\n")
  )

;; yaml-mode
(kui/with-lib "yaml-mode"
  ;; ...
  )

;; ruby-mode
(kui/with-lib "ruby-mode"
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("/config\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\(Rake\\|Gem\\|Thor\\|Berks\\|Vagrant\\)file\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.builder\\'" . ruby-mode)))
(kui/after-loaded "ruby-mode"
  (setq ruby-deep-indent-paren nil)

  (defun kui/ruby-init ()
    (flycheck-mode)
    (message "hoge")
    (electric-pair-mode t)
    (electric-indent-mode t))
  (add-hook 'ruby-mode-hook 'kui/ruby-init)

  (kui/with-pkg 'robe
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'robe-mode-hook 'ac-robe-setup))

  (when (and (executable-find "rbenv")
             (kui/package-require 'rbenv))
    (add-hook 'ruby-mode-hook 'global-rbenv-mode))

  (kui/with-pkg 'ruby-block
    (setq ruby-block-highlight-toggle 'overlay)
    (add-hook 'ruby-mode-hook '(lambda () (ruby-block-mode t))))
  )

;; coffee-mode
(kui/with-lib "coffee"
  (kui/add-to-list-if-exist 'ac-modes 'coffee-mode)
  (kui/with-pkg 'col-highlight
    (add-hook 'coffee-mode-hook 'column-highlight-mode)))
(kui/after-loaded "coffee"
  (setq coffee-tab-width 2)
  (setq coffee-debug-mode t)

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

;; css-mode
(setq css-indent-offset 2)
(kui/add-to-list-if-exist 'ac-modes 'css-mode)

;; scss-mode
(kui/with-lib "scss-mode"
  (setq scss-compile-at-save nil))

;; js-mode
(kui/with-lib "js"
  (add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode)))
(kui/after-loaded "js"
  (setq js-indent-level 2)
  )

;; html-mode
(kui/add-to-list-if-exist 'ac-modes 'html-mode)

;; multi-web-mode
(kui/with-lib "multi-web-mode"
  (add-to-list 'auto-mode-alist '("\\.html\\'" . multi-web-mode)))
(kui/after-loaded "multi-web-mode"
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script>" "</script>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)>"
                             "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1)
  )

;; web-mode
(kui/with-lib "web-mode"
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (kui/add-to-list-if-exist 'ac-modes 'web-mode)
  (add-hook 'web-mode-hook
            '(setq web-mode-markup-indent-offset 2
                   web-mode-css-indent-offset 2
                   web-mode-code-indent-offset 2
                   web-mode-style-padding 0
                   web-mode-script-padding 0)))

;; groovy-mode
(kui/with-lib "groovy-mode"
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
  (kui/add-to-list-if-exist 'ac-modes 'groovy-mode))

;; haml-mode
(kui/with-lib "haml-mode"
  (kui/add-to-list-if-exist 'ac-modes 'haml-mode))

;; rust-mode
(kui/after-loaded "rust-mode"

  ;; (defun kui/flycheck-rust-crate-bin ()
  ;;   (if (save-excursion (search-forward-regexp "fn\\s-+main\\s-*(\\s-*)" nil nil))
  ;;       (setq flycheck-rust-crate-type "bin")))
  ;; (add-hook 'flycheck-mode-hook 'kui/flycheck-rust-crate-bin)

  ;; flycheck for Cargo
  (kui/with-pkg 'flycheck-rust
    (kui/after-loaded "flycheck"
      (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
    (add-hook 'rust-mode-hook 'flycheck-mode))

  ;; racer(Code Completion)
  (let* ((racer-path (executable-find "racer"))
         (racer-path (if racer-path (file-chase-links racer-path 100)))
         (racer-dir (if racer-path (kui/parent-directory racer-path 3)))
         (racer-load-path (if racer-dir (concat racer-dir "editors")))
         (rust-path (getenv "RUST_SRC_PATH")))
    (when (and racer-dir rust-path)
      (add-to-list 'load-path racer-load-path)
      (when (require 'racer nil t)
        (setq racer-rust-src-path rust-path
              racer-cmd racer-path))))
  )

(kui/after-loaded "typescript"
  (kui/add-to-list-if-exist 'ac-modes 'typescript-mode)

  (kui/with-pkg 'flycheck
    (flycheck-define-checker typescript
      "A TypeScript syntax checker using tsc command."
      :command ("tsc" "--out" "/dev/null" source)
      :error-patterns
      ((error line-start (file-name) "(" line "," column "): error " (message) line-end))
      :mode typescript-mode)
    (add-to-list 'flycheck-checkers 'typescript))

  (kui/with-pkg 'tss
    (setq tss-popup-help-key "C-:"
          tss-jump-to-definition-key "M-."
          tss-implement-definition-key "M-,")
    ;; (tss-config-default)
    (add-hook 'typescript-mode-hook 'tss-setup-current-buffer t)
    (add-hook 'kill-buffer-hook 'tss--delete-process t)
    ))

;; -------------------------------------------------------------------------
;; 色とか
(kui/with-pkg 'color-theme
  (color-theme-initialize)

  (kui/with-pkg 'color-theme-sanityinc-tomorrow
    (load-theme 'sanityinc-tomorrow-night t)
    (custom-set-faces
     '(highlight
       ((((background dark))
         :upperline t
         :underline t
         :background "#441133")))
     '(hl-line
       ((((background dark))
         :background "#112244")))
     '(show-paren-match
       ((((background dark))
         :inherit nil
         :weight ultra-bold
         :background "#332244"
         :foreground nil))))

    (unless window-system
      (custom-set-faces
       '(mode-line
         ((((background dark))
           :background "#444444")))
       '(highlight
         ((((background dark))
           :background "#262626"
           :inherit nil))))))
  )

(when (featurep 'helm)
  (custom-set-faces
   '(helm-buffer-directory
     ((((background dark))
       :foreground "#33ff33")))
   '(helm-ff-directory
     ((((background dark))
       :foreground "#33ff33")))
   '(helm-selection
     ((t
       :inherit 'highlight
       :weight bold)))))

(when (featurep 'git-gutter)
  (dolist (f '(git-gutter:unchanged git-gutter:modified
               git-gutter:added git-gutter:deleted))
    (custom-set-faces `(,f ((((background dark))
                             :background "#333300"
                             :inverse-video nil)
                            (((background light))
                             :backgorund "#ffff99"
                             :inverse-video nil))))))

(when window-system
  ;; カーソルの色
  (set-cursor-color "green")

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
