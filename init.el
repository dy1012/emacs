;;初期ディレクトリ
(cd "~/")

;;スタート画面いらない
(setq inhibit-startup-screen t)

;;起動時にscratch表示
(add-hook 'emacs-startup-hook '(lambda () (switch-to-buffer "*scratch*")))

;;行番号表示
(global-linum-mode t)

;;折り返しいらない
(setq-default truncate-partial-width-windows t)
(setq-default truncate-lines t)

;;ツールバーいらない
(tool-bar-mode 0)

;;スクロールバーいらない
(set-scroll-bar-mode nil)

;;タイトル設定
(setq frame-title-format "%f - Emacs")

;;とりあえずバックアップいらない
(setq make-backup-files nil)

;;ファイルエンコード設定
(setenv "LANG" "ja_JP.UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;;自動再読込み
(global-auto-revert-mode 1)

;;フォント設定
(add-to-list 'default-frame-alist '(font . "-outline-Osaka－等幅-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1"))

;;ウィンドウ幅設定
(setq initial-frame-alist (append '((top . 20) (left . 20) (width . 160) (height . 40)) initial-frame-alist))

;;背景透過
(setq default-frame-alist (append (list '(alpha . (90 95))) default-frame-alist))

;;行ハイライト
(global-hl-line-mode t)
(set-face-background 'hl-line "#1C1C1C")

;;括弧ハイライト
(show-paren-mode 1)

;;キーバインド関連
;;isearch: F3: 前方検索
(define-key isearch-mode-map [f3] 'isearch-repeat-forward)

;;isearch: Shift-F3: 後方検索
(define-key isearch-mode-map [S-f3] 'isearch-repeat-backward)

;;分割ウィンドウ移動関連
;;M-<left>、M-<right>はデフォルトと衝突してると思うがC-<left>、C-<right>で同じ動作をする気がする
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;;Windows用の設定
(cond
 ((string-match "win" system-configuration)
  ;;IME設定
  ;;パッチの当たったemacsが必要
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
 )
)

;;package取得先追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;カラーテーマ設定
(when (require 'monokai-theme))

;;全角スペース、タブ、改行を可視化
(when (and (>= emacs-major-version 23) (require 'whitespace nil t))
  (setq whitespace-style
	'(face
	  tabs spaces newline trailing space-before-tab space-after-tab
	  space-mark tab-mark newline-mark))
  (let ((dark (eq 'dark (frame-parameter nil 'background-mode))))
    (set-face-attribute 'whitespace-space nil :foreground "#555" :background 'unspecified)
    (set-face-attribute 'whitespace-tab nil :foreground "#555" :background 'unspecified :strike-through t)
    (set-face-attribute 'whitespace-newline nil :foreground "#555" :background 'unspecified))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
	'((space-mark   ?\xA0  [?\xA4]  [?_]) (space-mark   ?\x8A0 [?\x8A4] [?_]) (space-mark   ?\x920 [?\x924] [?_]) (space-mark   ?\xE20 [?\xE24] [?_]) (space-mark   ?\xF20 [?\xF24] [?_]) (space-mark   ?　    [?□]    [?＿])
	  (newline-mark ?\n    [?\xAB ?\n])))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))

;;tabbar
(when (require 'tabbar)
  (tabbar-mode)
  (tabbar-mwheel-mode nil)
  (dolist (btn '(tabbar-buffer-home-button
		 tabbar-scroll-left-button
		 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil) (cons "" nil))))

  (setq tabbar-separator '(0.8))

  (set-face-attribute 'tabbar-default nil
		      :background "#191919"
		      :box '(:line-width 1 :color "#191919" :style nil)
		      :foreground "#999999"
		      :family "Osaka－等幅"
		      :height 1.0)

  (set-face-attribute 'tabbar-unselected nil
		      :background "#191919"
		      :box '(:line-width 1 :color "#191919" :style nil)
		      :foreground "#999999"
		      :family "Osaka－等幅"
		      :height 1.0)

  (set-face-attribute 'tabbar-selected nil
		      :background "#232323"
		      :box '(:line-width 1 :color "#232323" :style nil)
		      :foreground "#FFFFFF"
		      :family "Osaka－等幅"
		      :height 1.0)
)

;;anything
(when (require 'anything-startup)
  (global-set-key (kbd "\C-x b") 'anything)
)

;;scala-mode2
(when (require 'scala-mode2)
  (add-to-list 'load-path "~/.emacs.d/elpa/ensime_2.10.0-0.9.8.9/elisp/")
  (when (require 'ensime) (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

;;magit
(when (require 'magit))
