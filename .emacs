;; パス追加
(add-to-list 'load-path "~/.emacs.d")

;; 行ごと切り取り
(setq kill-whole-line t)
;; 補完で大文字小文字を区別しない
(setq read-file-name-completion-ignore-case t)
;; バックアップなし
(setq make-backup-files nil)
;; 行数を表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d ")
;;画面のスクロールの移動量を１にする
(setq scroll-step 1)
;; 対応する括弧をハイライト
(show-paren-mode t)
;; beep音を点滅に
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq hl-line-face 'underline) ; 下線

;; auto-complete
;; emacs上でload-file必須
(setq ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict") ;; 辞書ファイルのディレクトリ
(require 'auto-complete-config)
(ac-config-default)

;;括弧の補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)

;; コメントアウトショートカット
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(setq comment-style 'multi-line)
;;;;;;;;;;;;;;;;
;; view mode  ;;
;;;;;;;;;;;;;;;;

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

;;;;;;;;;;
;; helm ;;
;;;;;;;;;;
(add-to-list 'load-path "~/Github/helm")
(require 'helm-config)
(helm-mode 1)
