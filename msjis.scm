;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2014-6-11 v1.08
;;
;; ＜内容＞
;;   Windows のコマンドプロンプトで Gauche(gosh.exe) を使うときに、
;;   日本語(CP932)の表示と入力を可能とするモジュールです。
;;
;; ＜インストール方法＞
;;   msjis.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば (gauche-site-library-directory) で表示されるフォルダ等)
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
;; ＜注意事項＞
;;   (1)コンソールの入出力についてのみ文字コードが変換されます。
;;      (ファイルの読み書きやリダイレクトの文字コードは変換されません)
;;
;;   (2)Windows 8でまれにキー入力を受け付けなくなることがあります。
;;      (このプログラム以外でも発生するのでコマンドプロンプトの問題かも)
;;
(define-module msjis
  (use gauche.charconv)
  (use gauche.vport)
  (use gauche.uvector)
  (use os.windows)
  ;(use gauche.version)
  (export msjis-repl msjis-mode))
(select-module msjis)

;; デバッグ表示
(define (debug-print-str str)
  (display str (standard-output-port))
  (flush (standard-output-port)))
(define (debug-print-char-code chr)
  (format (standard-output-port) "~8,'0Xh" (char->integer chr))
  (flush (standard-output-port)))
(define (debug-print-buffer buf)
  (display (map (cut format #f "~2,'0Xh" <>) (u8vector->list buf)) (standard-output-port))
  (flush (standard-output-port)))

;; リダイレクトありかチェックする関数
;; (リダイレクトの有無はWin32APIのGetConsoleMode()が成功するかどうかで判定できる)
(define (redirected-handle? hdl)
  (guard (exc
          ((<system-error> exc) #t))
         (sys-get-console-mode hdl) #f))

;; ここで標準入出力のハンドルを保持しておくと
;;   *** SYSTEM-ERROR: read failed on #<iport (stdin) 00a8df50>: Bad file descriptor
;; や
;;   *** SYSTEM-ERROR: write failed on #<oport (stdout) 00a8dee0>: Bad file descriptor
;; というエラーが出なくなる。ガベージコレクションの関係か?
(define stdin-handle  (sys-get-std-handle STD_INPUT_HANDLE))
(define stdout-handle (sys-get-std-handle STD_OUTPUT_HANDLE))
(define stderr-handle (sys-get-std-handle STD_ERROR_HANDLE))



;; 1文字入出力の処理方法1 (現在未使用)
;;   Win32APIのReadConsole()とWriteConsole()を使う。
;;   ＜既知の問題点＞
;;     (1)ReadConsole()
;;          Windows XPで行頭の文字が「g」に化けて入力されて、エラーになる場合がある。原因不明。
;;
;;     (2)WriteConsole()
;;          Gauche v0.9.3.3のsys-write-consoleが、内部でCP932→Unicodeの変換をしていて化ける。
;;          回避しようと事前にCP932に変換してから渡すと、今度は不完全文字列ということでエラー。
;;          開発最新版では直っている(win-compat.c)。
;;
(define (make-getc-console hdl)
  (lambda ()
    (let1 buf (make-u8vector 2 0)
      (sys-read-console hdl buf)
      ;(debug-print-buffer buf)
      (let1 chr (string-ref (ces-convert (u8vector->string buf) 'UTF-16LE) 0)
        ;(debug-print-char-code chr)
        chr))))

(define (make-putc-console hdl)
  (lambda (chr)
    (sys-write-console hdl (string chr))))

(define (make-puts-console hdl)
  (lambda (str)
    (sys-write-console hdl str)))



;; 1文字入出力の処理方法2 (現在使用)
;;   read-block!とwrite-blockを使う。
;;   コードページがCP932であることが前提。
;;
(define (make-getc-console2 port)
  (lambda ()
    (let1 buf (make-u8vector 2 0)
      (read-block! buf port 0 1)
      ;; CP932の2バイト文字のチェック
      (let1 b (u8vector-ref buf 0)
        (if (or (and (>= b #x81) (<= b #x9F)) (and (>= b #xE0) (<= b #xFC)))
          (read-block! buf port 1 2)))
      ;(debug-print-buffer buf)
      (let1 chr (string-ref (ces-convert (u8vector->string buf) 'CP932) 0)
        ;(debug-print-char-code chr)
        chr))))

(define (make-putc-console2 port)
  (lambda (chr)
    (let1 buf (string->u8vector (ces-convert (string chr) (gauche-character-encoding) 'CP932))
      ;(debug-print-buffer buf)
      (write-block buf port)
      (flush port))))

(define (make-puts-console2 port)
  (lambda (str)
    (let1 buf (string->u8vector (ces-convert str (gauche-character-encoding) 'CP932))
      ;(debug-print-buffer buf)
      (write-block buf port)
      (flush port))))



;; 標準入力の変換ポートの作成
(define (make-console-stdin-port)
  (let1 hdl (sys-get-std-handle STD_INPUT_HANDLE)
    (if (redirected-handle? hdl) #f
      ;(make <virtual-input-port> :getc (make-getc-console hdl))
      (make <virtual-input-port> :getc (make-getc-console2 (standard-input-port)))
      )))

;; 標準出力の変換ポートの作成
(define (make-console-stdout-port)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (if (redirected-handle? hdl) #f
      ;(make <virtual-output-port> :putc (make-putc-console hdl) :puts (make-puts-console hdl))
      (make <virtual-output-port>
            :putc (make-putc-console2 (standard-output-port))
            :puts (make-puts-console2 (standard-output-port)))
      )))

;; 標準エラー出力の変換ポートの作成
(define (make-console-stderr-port)
  (let1 hdl (sys-get-std-handle STD_ERROR_HANDLE)
    (if (redirected-handle? hdl) #f
      ;(make <virtual-output-port> :putc (make-putc-console hdl) :puts (make-puts-console hdl))
      (make <virtual-output-port>
            :putc (make-putc-console2 (standard-error-port))
            :puts (make-puts-console2 (standard-error-port)))
      )))



;; 変換ポートを設定してREPLを起動する
(define (msjis-repl)
  (with-ports (make-console-stdin-port)
              (make-console-stdout-port)
              (make-console-stderr-port)
              (lambda ()
                ;(print "MSJISモード")
                (read-eval-print-loop))))

;; 変換ポートの設定のみ実施する
(define (msjis-mode)
  (if-let1 port (make-console-stdin-port)  (current-input-port  port))
  (if-let1 port (make-console-stdout-port) (current-output-port port))
  (if-let1 port (make-console-stderr-port) (current-error-port  port))
  (undefined))

