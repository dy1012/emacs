;;----------------------------------------------------------------
;; 基本設定
;;----------------------------------------------------------------

;;スタート画面いらない
(setq inhibit-startup-screen t)

;;起動時にscratch表示
(add-hook 'emacs-startup-hook '(lambda () (switch-to-buffer "*scratch*")))

;;折り返しいらない
(setq-default truncate-partial-width-windows t)
(setq-default truncate-lines t)

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

;;yes/no問い合わせを簡略化
(fset 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------
;; 外観設定
;;----------------------------------------------------------------

;;ツールバーいらない
(tool-bar-mode 0)

;;スクロールバーいらない
(set-scroll-bar-mode nil)

;;タイトル設定
(setq frame-title-format "%f - Emacs")

;;行番号表示
(global-linum-mode t)

;;行、列番号を表示
(line-number-mode t)
(column-number-mode t)

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

;;----------------------------------------------------------------
;; キーバインド
;;----------------------------------------------------------------

;;isearch: F3: 前方検索
;;isearch: Shift-F3: 後方検索
(define-key isearch-mode-map [f3] 'isearch-repeat-forward)
(define-key isearch-mode-map [S-f3] 'isearch-repeat-backward)

;;分割ウィンドウ移動関連
;;M-<left>、M-<right>はデフォルトと衝突してると思うがC-<left>、C-<right>で同じ動作をする気がする
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;;分割ウィンドウ移動
;;分割されていない場合、横に割って移動を行う
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;;Windows用の設定
(cond ((string-match "win" system-configuration)
  ;;IME設定
  ;;パッチの当たったemacsが必要
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)))

;;Alt+ホイールでフォントサイズ変更
(global-set-key (kbd "M-<wheel-up>") 'text-scale-increase)
(global-set-key (kbd "M-<wheel-down>") 'text-scale-decrease)

;;----------------------------------------------------------------
;; プラグイン設定
;;----------------------------------------------------------------
;;package取得先追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;カラーテーマ設定
(when (locate-library "monokai-theme") (require 'monokai-theme))

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
(when (locate-library "tabbar") (require 'tabbar)
  (tabbar-mode)
  (tabbar-mwheel-mode nil)

  (global-set-key (kbd "C-.") 'tabbar-forward-tab)
  (global-set-key (kbd "C-,") 'tabbar-backward-tab)

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
		      :height 1.0))

;;anything
(when (locate-library "anything-startup") (require 'anything-startup)
  (global-set-key (kbd "\C-x b") 'anything)
  ;;C-x C-bはいらないのでキーバインド解除
  (global-unset-key (kbd "C-x C-b")))

;;magit
(when (locate-library "magit") (require 'magit))

;;popwin
(when (locate-library "popwin") (require 'popwin)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'right))

;;direx
(when (locate-library "direx") (require 'direx)
  (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
  (push '(direx:direx-mode :position right :width 40 :dedicated t) popwin:special-display-config))

(when (locate-library "google-translate") (require 'google-translate)
  ;;日本語から英語に翻訳を行う
  (defun google-translate-ja-en ()
  (interactive)
  (custom-set-variables
   '(google-translate-default-source-language "ja")
   '(google-translate-default-target-language "en"))
  (google-translate-query-translate))

  ;;英語から日本語に翻訳を行う
  (defun google-translate-en-ja ()
    (interactive)
    (custom-set-variables
     '(google-translate-default-source-language "en")
     '(google-translate-default-target-language "ja"))
    (google-translate-query-translate))

  ;;キーバインド設定
  (global-set-key (kbd "C-c j") 'google-translate-ja-en)
  (global-set-key (kbd "C-c e") 'google-translate-en-ja)
  (global-set-key (kbd "C-c t") 'google-translate-at-point)
  (push '("*Google Translate*") popwin:special-display-config))

;;----------------------------------------------------------------
;; Scala
;;----------------------------------------------------------------
;;scala-mode2
;;(when (require 'scala-mode2)
;;  (add-to-list 'load-path "~/.emacs.d/elpa/ensime_2.10.0-0.9.8.9/elisp/")
;;  (when (require 'ensime) (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

;;----------------------------------------------------------------
;; Groovy
;;----------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/Emacs-Groovy-Mode"))
(when (locate-library "groovy-mode")
  (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
  (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
  (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode)))

;;----------------------------------------------------------------
;; yasnippet
;;----------------------------------------------------------------
(when (locate-library "yasnippet") (require 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t)
  (custom-set-variables '(yas-trigger-key "C-;"))
  (define-key yas-minor-mode-map (kbd "C-c i i") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c i n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c i v") 'yas-visit-snippet-file)

  (when (locate-library "anything")
    (eval-after-load "anything-config"
      '(progn
	 (defun my-yas/prompt (prompt choices &optional display-fn)
	   (let* ((names (loop for choice in choices
			       collect (or (and display-fn (funcall display-fn choice)) choice)))

		  (selected (anything-other-buffer
			     `(((name . ,(format "%s" prompt))
				(candidates . names)
				(action . (("Insert snippet" . (lambda (arg) arg))))))
			     "*anything yas/prompt*")))
	     (if selected
		 (let ((n (position selected names :test 'equal)))
		   (nth n choices))
	       (signal 'quit "user quit!"))))
	 (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
	 (define-key anything-command-map (kbd "y") 'yas/insert-snippet)))))
