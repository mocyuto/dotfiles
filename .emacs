;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Load Path          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path(append(list(expand-file-name "~/.emacs.d"))load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))

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

;;;;;;;;;;;;;;;;;;
;; Auto Install ;;
;;;;;;;;;;;;;;;;;;

(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)

;;;;;;;;;;;;;;;;;
;;  View-mode  ;;
;;;;;;;;;;;;;;;;;

;;view-modeをデフォルトにする
(add-hook 'find-file-hook
          '(lambda ()
             (interactive)
             (view-mode)))
(setq view-read-only t)

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
  (hl-line-mode 1)
  (define-key view-mode-map " " 'scroll-up))
(add-hook 'view-mode-hook 'view-mode-hook0)

;;ワンタッチでview-modeに移行
(setq load-path (cons "~/.emacs.d/" load-path))
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
(key-chord-define-global "jk" 'view-mode)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        補完          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;auto-complete
(setq load-path (cons "/home/lr/yuto/.emacs.d/auto-complete-1.3.1" load-path))
(require 'auto-complete)
(global-auto-complete-mode t)

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

;; flymake for python
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pychecker"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)


;;;;;;;;;;;;;;;;;;
;; Twitter Mode ;;
;;;;;;;;;;;;;;;;;;

;(add-to-list 'load-path "~/.emacs.d/twittering-mode-2.0.0")
;(require 'twittering-mode)
;(setq twittering-use-master-password t)

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
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
