;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        表示          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;対応する括弧をハイライト表示させる設定
(show-paren-mode t)

;同名のファイルを開いたとき Switch to buffer などでファイル名がわかりやすく見えるようになる設定
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;選択されたリージョンを色付きにしてわかりやすくする設定
(setq-default transient-mark-mode t)

;; マッチした場合の色
(set-face-background 'show-paren-match-face "RoyalBlue1")
(set-face-foreground 'show-paren-match-face "black")
;; マッチしていない場合の色
(set-face-background 'show-paren-mismatch-face "Red")
(set-face-foreground 'show-paren-mismatch-face "black")

;;メニューを日本語化
;(require 'menu-tree t)

;;画像ファイルを表示する
(auto-image-file-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; タブ、全角スペースを表示する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "red" :underline t))) nil)
(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode())
  (font-lock-add-keywords
   major-mode
   '(
     ("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-2 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     (" [\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;
;;  View-mode  ;;
;;;;;;;;;;;;;;;;;

;;view-modeをデフォルトにする
(add-hook 'find-file-hook
          '(lambda ()
             (interactive)
             (view-mode)))
(setq view-read-only t)

;;キーバインド定義
(defvar pager-keybind
  `( ;; vi-like
    ("h" . backward-char)
    ("j" . next-line)
    ("k" . previous-line)
    ("l" . forward-char)
    ("n" . ,(lambda () (interactive) (scroll-up 1)))
    ("p" . ,(lambda () (interactive) (scroll-down 1)))
    ))
(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)
(defun view-mode-hook0 ()
  (define-many-keys view-mode-map pager-keybind)
  (hl-line-mode 1))
(add-hook 'view-mode-hook 'view-mode-hook0)

;;jk同時押しでview-modeに移行
(setq load-path (cons "~/.emacs.d/" load-path))
(require 'key-chord)
(setq key-chord-two-keys-delay 0.03)
(key-chord-mode 1)
(key-chord-define-global "jk" 'view-mode)

;; 書き込み不能なファイルはview-modeで開くように
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))
;; 書き込み不能な場合はview-modeを抜けないように
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))
(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            編集行を目立たせる（現在行をハイライト表示する）               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")
;;(setq hl-line-face 'hlline-face)
(setq hl-line-face 'underline) ; 下線
(global-hl-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        基本          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-kで改行も含めてカットする
(setq kill-whole-line t)

;; Ctrl+Zで最小化しない
(define-key global-map "\C-z" 'scroll-down)

;;find-fileのファイル名補完で大文字小文字を区別しない設定
(setq read-file-name-completion-ignore-case t)

;;バックアップファイルを作成しない
(setq make-backup-files nil)

;;バックスペース
(global-set-key "\C-h" 'delete-backward-char)
;; 行数を表示させる
(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d ")

;;選択部分を一気にインデント
(global-set-key "\C-x\C-i" 'indent-region)

;;画面のスクロールの移動量を１にする
(setq scroll-step 1)

;;regionがactiveなときBackspaceでそれを削除
(defadvice backward-delete-char-untabify
  (around ys:backward-delete-region activate)
  (if (and transient-mark-mode mark-active)
      (delete-region (region-beginning) (region-end))
    ad-do-it))

;; 下まで行ったら折り返し
(setq windmove-wrap-around t)
;; ウィンドウ間をShift+矢印キーで移動
;; C-M-{h,j,k,l}でウィンドウ間を移動
(define-key global-map (kbd "C-M-k") 'windmove-up)
(define-key global-map (kbd "C-M-j") 'windmove-down)
(define-key global-map (kbd "C-M-l") 'windmove-right)
(define-key global-map (kbd "C-M-h") 'windmove-left)
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        補完          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;auto-complete
(setq load-path (cons "~/.emacs.d/auto-complete-1.3.1" load-path))
(require 'auto-complete)
(global-auto-complete-mode t)
;;find-fileのファイル名補完で大文字小文字を区別しない設定
(setq read-file-name-completion-ignore-case t)

(require 'auto-complete-latex)

;;補完が自動で起動するのを停止
(setq ac-auto-start nil)
;;補完の起動キーの設定
(ac-set-trigger-key "TAB")
;;括弧の補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        折り畳み          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook
	  '(lambda ()
	     (hs-minor-mode 1)))
(define-key
  global-map
  (kbd "C-c 3") 'hs-toggle-hiding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Load Path          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path(append(list(expand-file-name "~/.emacs.d"))load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Lookup           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(autoload 'lookup "lookup" nil t)
;(autoload 'lookup-region "lookup" nil t)
;(autoload 'lookup-pattern "lookup" nil t)
;(setq lookup-search-agents '((ndtp "dserver") (ndspell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Python          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "~/.emacs.d/pymacs_dir"))

;; python-mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; pycomplete
(add-hook 'python-mode-hook '(lambda () 
     (require 'pycomplete)))


;;;;;;;;;;;;;;;;;;
;; Twitter Mode ;;
;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/twittering-mode-2.0.0")
(require 'twittering-mode)
(setq twittering-use-master-password t)


;;;;;;;;;;;;;
;;;; term ;;;
;;;;;;;;;;;;;
;;文字コード
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)

;;エスケープを綺麗に表示
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;multi-term
(require 'multi-term)
(setq multi-term-program shell-file-name)

;;multi-term の起動のショートカットを作る
(global-set-key (kbd "C-c m") 'multi-term)

;;;;;;;;;;;;;;;;;;;;;;
;;;      TeX       ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; TexShop
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;プレビュー
(setq dvi2-command "open -a Skim"
      tex-command "~/Library/TeXShop/bin/platex2pdf-utf8")

(setq bibtex-command (cond ((string-match "uplatex" tex-command) "/usr/texbin/upbibtex")
                           ((string-match "platex" tex-command) "/usr/texbin/pbibtex")
                           ((string-match "lualatex\\|xelatex" tex-command) "/usr/texbin/bibtexu")
                           (t "/usr/texbin/bibtex")))
(setq YaTeX-no-begend-shortcut t
      YaTeX-kanji-code 4
      YaTeX-use-hilit19 nil
      YaTeX-use-font-lock nil)
;AMS-LaTeX を使用する
(setq YaTeX-use-AMS-LaTeX t)

(if window-system
    (progn
      (require 'font-latex)
      (add-hook 'yatex-mode-hook 'font-latex-setup 'append)
      (add-hook 'yatex-mode-hook 'turn-on-font-lock 'append)))

;YaTeXでコメントアウト、解除を割り当てる
(add-hook 'yatex-mode-hook
	    '(lambda ()
	       (local-set-key "\C-c\C-c" 'comment-region)
	       (local-set-key "\C-c\C-u" 'uncomment-region) ))


; RefTeXをYaTeXで使えるようにする
(add-hook 'yatex-mode-hook '(lambda () (reftex-mode t)))
; RefTeXで使うbibファイルの位置を指定する
;(setq reftex-default-bibliography '("~/Library/TeX/bib/papers.bib"))

;;RefTeXに関する設定
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
;;RefTeXにおいて数式の引用を\eqrefにする
(setq reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil)))
;; hilit19 は使わない。更にフェイスを追加して、色をカスタマイズ。
(setq YaTeX-use-hilit19 nil)
(setq YaTeX-use-font-lock nil)
(if window-system
    (progn
      (require 'font-latex)
      (add-hook 'yatex-mode-hook 'font-latex-setup)
      (add-hook 'yatex-mode-hook 'turn-on-font-lock 'append)))

(cond
 ((featurep 'font-lock)
  (defface font-latex-math-face
    '((((class grayscale) (background light)) 
       (:foreground "DimGray" :underline t))
      (((class grayscale) (background dark)) 
       (:foreground "LightGray" :underline t))
      (((class color) (background light)) (:foreground "SaddleBrown"))
      (((class color) (background dark))  (:foreground "burlywood"))
      (t (:underline t)))
    "Font Lock mode face used to highlight math in LaTeX."
    :group 'font-latex-highlighting-faces)
  
  (defface font-latex-label-face
    '((((class static-color)) (:foreground "yellow" :underline t))
      (((type tty)) (:foreground "yellow" :underline t))
      (((class color) (background dark)) (:foreground "pink"))
      (((class color) (background light)) (:foreground "ForestGreen"))
      (t (:bold t :underline t)))
    "Font Lock mode face used to highlight labels."
    :group 'font-lock-faces)))

(setq YaTeX-font-lock-formula-face 'font-latex-math-face
      YaTeX-font-lock-label-face 'font-latex-label-face)
