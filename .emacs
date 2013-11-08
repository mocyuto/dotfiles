;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Load Path          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "~/.emacs.d/elisp" load-path))
(setq load-path(append(list(expand-file-name "~/.emacs.d"))load-path))
;; ↓とにかく設定ファイルの最初のほうで読みこむ
;(load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
;(add-to-list 'load-path "~/.emacs.d/jdee/dist/jdee-2.4.1/lisp")

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

;; タブをスペース4つにする
(setq-default tab-width 4 indent-tabs-mode nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          yasnippet          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'cl)
;; ;;問い合わせを簡略化
;; (fset 'yes-or-no-p 'y-or-n-p)
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
;; (require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
;;         "~/.emacs.d/yasnippet/snippets" ;; 最初から入っていたスニペット
;;         ))
;; (yas-global-mode 1)


;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
;; (setqだとtermなどで干渉問題ありでした)
;; もちろんTAB以外でもOK 例えば "C-;"とか
;(custom-set-variables '(yas-trigger-key "TAB"))
;; 既存スニペットを挿入する
;(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
;(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
;(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
;; (defun yas/expand-link-choice (&rest keys)
;;   "Hyperlink to select yasnippet template."
;;   (yas/expand-link (completing-read "Select template: " keys nil t)))
;; (yas/expand-link-choice "defgp" "defcm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        ac-anything          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 古い
;(require 'ac-anything)
;(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)
;; 最新
;; (require 'anything)
;; (add-to-list 'load-path "~/.emacs.d/anything-config/")
;; (require 'anything-config)
;; (add-to-list 'anything-sources 'anything-c-source-emacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        補完          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; パスを通す
(defvar ac-dir (expand-file-name "~/.emacs.d/auto-complete"))
(add-to-list 'load-path ac-dir)
(add-to-list 'load-path (concat ac-dir "/lib/ert"))
(add-to-list 'load-path (concat ac-dir "/lib/fuzzy"))
(add-to-list 'load-path (concat ac-dir "/lib/popup"))

(require 'auto-complete)
(global-auto-complete-mode t)

;;; ベースとなるソースを指定
;; (defvar my-ac-sources
;;               '(ac-source-yasnippet
;;                 ac-source-abbrev
;;                 ac-source-dictionary
;;                 ac-source-words-in-same-mode-buffers))

;;find-fileのファイル名補完で大文字小文字を区別しない設定
(setq read-file-name-completion-ignore-case t)

(when (require 'auto-complete-config nil t)
  (setq ac-use-menu-map t);; C-n/C-pで候補選択可能
  (setq ac-dwim t) ;; 空気を読む
  (setq ac-auto-start 2) ;;補完が自動で起動するのを停止
  (setq ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict") ;; 辞書ファイルのディレクトリ
  (setq ac-comphist-file "~/.emacs.d/elisp/auto-complete/ac-comphist.dat") ;; 補完履歴のキャッシュ先
)
;; 補完辞書の参照をデフォルト設定に
(ac-config-default)

;;補完の起動キーの設定
;; (global-set-key "\M-/" 'ac-start)
;;括弧の補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)


;; 適用するメジャーモードを足す
(setq ac-modes
      (append ac-modes
              '(perl-mode cperl-mode html-mode sql-mode vbnet-mode
                          css-mode actionscript-mode web-mode
                          js2-mode )))
;; (add-to-list 'ac-modes '(web-mode) t)
;; (add-to-list 'ac-modes '(scss-mode) t)
;; (add-to-list 'ac-modes '(html-mode) )
;; (add-to-list 'ac-modes '(js-mode) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        折り畳み          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(define-key
  global-map
  (kbd "C-c 3") 'hs-toggle-hiding)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Python          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (eval-after-load "pymacs"
;;   '(add-to-list 'pymacs-load-path "~/.emacs.d/pymacs_dir"))

;; ;; python-mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;; interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; ;; pycomplete
;; ;(add-hook 'python-mode-hook '(lambda ()
;; ;     (require 'pycomplete)))

;; ;; auto-complete for python
(require 'ac-python)
(add-to-list 'ac-modes 'python-mode)


;; pysmell
;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))
;; (defvar ac-source-pysmell
;;   '((candidates
;;      . (lambda ()
;;          (require 'pysmell)
;;          (pysmell-get-all-completions))))
;;   "Source for PySmell")

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'ac-sources)
;;                   (append ac-sources '(ac-source-pysmell)))))

;;;;;;;;;;;;;;;;;;
;;     Java     ;;
;;;;;;;;;;;;;;;;;;

;; (load "jde-autoload")

;; (defun my-jde-mode-hook ()
;;   (require 'jde)

;;   (setq jde-build-function 'jde-ant-build) ; ビルドにantを利用する
;;   (setq jde-ant-read-target t)             ; targetを問い合わせる
;;   (setq jde-ant-enable-find t)             ; antに-findオプションを指定する(要らないかも)

;;   ;; complilationバッファを自動的にスクロールさせる
;;   (setq compilation-ask-about-save nil)
;;   (setq compilation-scroll-output 'first-error)

;;   (define-key jde-mode-map (kbd "C-c C-v .") 'jde-complete-minibuf)
;;   )

;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)

;; (require 'cedet)
;; ;(semantic-load-enable-minimum-features)
;; (when (require 'malabar-mode nil t)
;;   (setq malabar-groovy-lib-dir (concat user-emacs-directory "lisp/malabar-1.5-SNAPSHOT/lib"))
;;   (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;;   ;; 日本語だとコンパイルエラーメッセージが化けるのでlanguageをenに設定
;;   (setq malabar-groovy-java-options '("-Duser.language=en"))
;;   ;; 普段使わないパッケージを import 候補から除外
;;   (setq malabar-import-excluded-classes-regexp-list
;;         (append
;;          '(
;;            "^java\\.awt\\..*$"
;;            "^com\\.sun\\..*$"
;;            "^org\\.omg\\..*$"
;;            ) malabar-import-excluded-classes-regexp-list))
;;   (add-hook 'malabar-mode-hook
;;             (lambda ()
;;               (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                         nil t)))
;; )

;;;;;;;;;;;;;;;;;;;;;
;;;;   php-mode  ;;;;
;;;;;;;;;;;;;;;;;;;;;

;(load-library "php-mode-1.5.0")
(require 'php-mode)

;;;;;;;;;;;;;;;;;;;;;
;;;;   js2-mode  ;;;;
;;;;;;;;;;;;;;;;;;;;;

;(autoload 'js2-mode "js2" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      elisp-install       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; まず、install-elisp のコマンドを使える様にします。
(require 'install-elisp)
;; 次に、Elisp ファイルをインストールする場所を指定します。
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         flymake          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)

;;;;;;;;;;;;;;;;;;;;;;
;;;      TeX       ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;(require 'auto-complete-latex)

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
      YaTeX-kanji-code 4)
;      YaTeX-use-hilit19 nil
;      YaTeX-use-font-lock nil)
;AMS-LaTeX を使用する
(setq YaTeX-use-AMS-LaTeX t)

;(if window-system
;    (progn
;      (require 'font-latex)
;      (add-hook 'yatex-mode-hook 'font-latex-setup 'append)
;      (add-hook 'yatex-mode-hook 'turn-on-font-lock 'append)))

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
;(setq YaTeX-use-hilit19 nil)
;(setq YaTeX-use-font-lock nil)
;(if window-system
;    (progn
;      (require 'font-latex)
;      (add-hook 'yatex-mode-hook 'font-latex-setup)
;      (add-hook 'yatex-mode-hook 'turn-on-font-lock 'append)))

;(cond
; ((featurep 'font-lock)
;  (defface font-latex-math-face
;    '((((class grayscale) (background light)) 
;       (:foreground "DimGray" :underline t))
;      (((class grayscale) (background dark)) 
;       (:foreground "LightGray" :underline t))
;      (((class color) (background light)) (:foreground "SaddleBrown"))
;      (((class color) (background dark))  (:foreground "burlywood"))
;      (t (:underline t)))
;    "Font Lock mode face used to highlight math in LaTeX."
;    :group 'font-latex-highlighting-faces)
;  
;  (defface font-latex-label-face
;    '((((class static-color)) (:foreground "yellow" :underline t))
;      (((type tty)) (:foreground "yellow" :underline t))
;      (((class color) (background dark)) (:foreground "pink"))
;      (((class color) (background light)) (:foreground "ForestGreen"))
;      (t (:bold t :underline t)))
;    "Font Lock mode face used to highlight labels."
;    :group 'font-lock-faces)))
;
;(setq YaTeX-font-lock-formula-face 'font-latex-math-face
 ;     YaTeX-font-lock-label-face 'font-latex-label-face)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   for io    ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.io\\'" . io-mode))
(autoload 'io-mode "io-mode" "Major mode to edit Io language files in Emacs." t)
;; tabwidth
(setq io-tab-width 2)
