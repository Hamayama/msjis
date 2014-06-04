;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2014-6-4 v1.02
;;
;; ＜内容＞
;;   Windows のコマンドプロンプトで Gauche(gosh.exe) を使うときに、
;;   日本語(CP932)の表示と入力を可能とするモジュールです。
;;
;; ＜注意事項＞
;;   (1)Gauche の開発最新版が必要です(v0.9.3.3ではエラーになります)。
;;   (2)コンソールの入出力についてのみ文字コードが変換されます。
;;      (ファイルの読み書きやリダイレクトの文字コードは変換されません)
;;   (3)まれにキー入力を受け付けなくなることがあります(原因不明)。
;;
;; ＜インストール＞
;;   msjis.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば C:\Program Files\Gauche\share\gauche-0.9\site\lib 等)
;;
;; ＜使い方＞
;;   対話環境にする場合
;;     (use msjis)
;;     (msjis-repl)
;;
;;   対話環境にしない場合
;;     (use msjis)
;;     (msjis-mode)
;;
(define-module msjis
  (use gauche.charconv)
  (use gauche.vport)
  (use gauche.uvector)
  (use os.windows)
  (export msjis-repl msjis-mode))
(select-module msjis)

;; リダイレクトありかチェックする関数
;; (リダイレクトの有無はGetConsoleMode()が成功するかどうかで判定できる)
(define (redirected-handle? hdl)
  (guard (exc
          ((<system-error> exc) #t))
         (sys-get-console-mode hdl) #f))

;; 1文字入力処理(Windows XPで行頭の文字が「g」に化けてエラーになる場合がある。原因不明)
(define (getc-console hdl)
  (let1 buf (make-u8vector 2 0)
    (sys-read-console hdl buf)
    (let1 chr (string-ref (ces-convert (u8vector->string buf) 'UTF-16LE) 0)
      ;(format #t "~8,'0x;" (char->integer chr))
      chr)))

;; 1文字入力処理2(文字化け対策版)(ただしCP932専用)
(define (getc-console2 hdl)
  (let1 buf (make-u8vector 2 0)
    (read-block! buf (standard-input-port) 0 1)
    ;; 2バイト文字のチェック(CP932専用)
    (if (>= (u8vector-ref buf 0) #x80) (read-block! buf (standard-input-port) 1 2))
    (let1 chr (string-ref (ces-convert (u8vector->string buf) 'CP932) 0)
      ;(format #t "~8,'0x;" (char->integer chr))
      chr)))

;; 1文字出力処理
(define (putc-console hdl chr)
  (sys-write-console hdl (string chr)))

;; 文字列出力処理
(define (puts-console hdl str)
  (sys-write-console hdl str))

;; 標準入力の変換ポートの作成
(define (make-console-stdin-port)
  (let1 hdl (sys-get-std-handle STD_INPUT_HANDLE)
    (if (redirected-handle? hdl) #f
      ;(make <virtual-input-port> :getc (cut getc-console hdl))
      (make <virtual-input-port> :getc (cut getc-console2 hdl))
      )))

;; 標準出力の変換ポートの作成
(define (make-console-stdout-port)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (if (redirected-handle? hdl) #f
      (make <virtual-output-port> :putc (cut putc-console hdl <>) :puts (cut puts-console hdl <>)))))

;; 標準エラー出力の変換ポートの作成
(define (make-console-stderr-port)
  (let1 hdl (sys-get-std-handle STD_ERROR_HANDLE)
    (if (redirected-handle? hdl) #f
      (make <virtual-output-port> :putc (cut putc-console hdl <>) :puts (cut puts-console hdl <>)))))

;; 変換ポートを設定してREPLを起動する
(define (msjis-repl)
  (with-ports (make-console-stdin-port) (make-console-stdout-port) (make-console-stderr-port)
              (lambda ()
                ;(print "MSJISモード")
                (read-eval-print-loop))))

;; 変換ポートの設定のみ実施する
(define (msjis-mode)
  (if-let1 port (make-console-stdin-port)  (current-input-port  port))
  (if-let1 port (make-console-stdout-port) (current-output-port port))
  (if-let1 port (make-console-stderr-port) (current-error-port  port))
  (undefined))

