;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2014-8-6 v1.14
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
;;   リダイレクト時の動作の設定
;;     (msjis-repl) または (msjis-mode) に数値の引数をつけて呼び出すと、
;;     リダイレクト時の動作を設定できます。以下の引数が使用可能です。
;;       0 : リダイレクト時には変換なし(デフォルト)
;;       1 : リダイレクト時には改行コード変換(LF→CRLF)あり
;;       2 : リダイレクト時には文字コード変換(CP932変換)あり
;;       3 : リダイレクト時には文字コード変換(CP932変換)と改行コード変換(LF→CRLF)あり
;;     例えば (msjis-mode 1) とするとリダイレクト時には、
;;     改行コードLFをCRLFに変換して出力します。
;;
;; ＜注意事項＞
;;   (1)コンソールの標準入力、標準出力、標準エラー出力についてのみ
;;      文字コードが変換されます。
;;
;;   (2)Windows 8でまれにキー入力を受け付けなくなることがあります。
;;      (このプログラム以外でも発生するのでコマンドプロンプトの問題かも)
;;
(define-module msjis
  (use gauche.charconv)
  (use gauche.vport)
  (use gauche.uvector)
  (use os.windows)
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

;; 標準入出力のハンドルの保持
;; (保持しておかないとエラーになる。Gauche v0.9.4-rc2では修正ずみ)
(define stdin-handle  (sys-get-std-handle STD_INPUT_HANDLE))
(define stdout-handle (sys-get-std-handle STD_OUTPUT_HANDLE))
(define stderr-handle (sys-get-std-handle STD_ERROR_HANDLE))



;; 1文字入出力の変換処理
;;   read-block!とwrite-blockを使用。
;;   コードページがCP932であることが前提。
;;
(define (make-getc-console port)
  (lambda ()
    (let ((chr 0)
          (buf (make-u8vector 2 0))
          (ret 0))
      (set! ret (read-block! buf port 0 1))
      ;; CP932の2バイト文字のチェック
      (if (not (eof-object? ret))
        (let1 b (u8vector-ref buf 0)
          (if (or (and (>= b #x81) (<= b #x9F)) (and (>= b #xE0) (<= b #xFC)))
            (set! ret (read-block! buf port 1 2)))))
      ;(debug-print-buffer buf)
      ;; 文字コードの変換(CP932→内部コード)
      (set! chr (string-ref (ces-convert (u8vector->string buf) 'CP932) 0))
      ;(debug-print-char-code chr)
      ;; ファイル終端(EOF)のチェック
      (if (eof-object? ret)
        (begin
          ;(debug-print-str "[EOF]")
          (set! chr (eof-object))))
      chr)))

(define (make-putc-console port c932 crlf)
  (lambda (chr)
    (puts-console-sub (string chr) port c932 crlf)))

(define (make-puts-console port c932 crlf)
  (lambda (str)
    (puts-console-sub str port c932 crlf)))

(define (puts-console-sub str port c932 crlf)
  (let1 buf 0
    ;; 改行コードの変換(LF→CRLF)
    (if crlf
      (set! str (regexp-replace-all #/\n/ str "\r\n")))
    ;; 文字コードの変換(内部コード→CP932)
    (if c932
      (set! buf (string->u8vector (ces-convert str (gauche-character-encoding) 'CP932)))
      (set! buf (string->u8vector str)))
    ;(debug-print-buffer buf)
    (write-block buf port)
    (flush port)))



;; 標準入力の変換ポートの作成
(define (make-console-stdin-port rmode)
  (receive (c932 crlf) (get-console-param rmode (sys-get-std-handle STD_INPUT_HANDLE))
    (if c932
      (make <virtual-input-port>
            :getc (make-getc-console (standard-input-port)))
      #f)))

;; 標準出力の変換ポートの作成
(define (make-console-stdout-port rmode)
  (receive (c932 crlf) (get-console-param rmode (sys-get-std-handle STD_OUTPUT_HANDLE))
    (if (or c932 crlf)
      (make <virtual-output-port>
            :putc (make-putc-console (standard-output-port) c932 crlf)
            :puts (make-puts-console (standard-output-port) c932 crlf))
      #f)))

;; 標準エラー出力の変換ポートの作成
(define (make-console-stderr-port rmode)
  (receive (c932 crlf) (get-console-param rmode (sys-get-std-handle STD_ERROR_HANDLE))
    (if (or c932 crlf)
      (make <virtual-output-port>
            :putc (make-putc-console (standard-error-port) c932 crlf)
            :puts (make-puts-console (standard-error-port) c932 crlf))
      #f)))

;; 変換用パラメータの取得
(define (get-console-param rmode hdl)
  (let ((c932 #f)
        (crlf #f))
    (if (redirected-handle? hdl)
      (begin
        (if (or (= rmode 2) (= rmode 3)) (set! c932 #t))
        (if (or (= rmode 1) (= rmode 3)) (set! crlf #t)))
      (begin
        (set! c932 #t)
        (set! crlf #f)))
    (values c932 crlf)))



;; 変換ポートを設定してREPLを起動する
(define (msjis-repl :optional (rmode 0))
  (with-ports (make-console-stdin-port  rmode)
              (make-console-stdout-port rmode)
              (make-console-stderr-port rmode)
              (lambda () (read-eval-print-loop))))

;; 変換ポートの設定のみ実施する
(define (msjis-mode :optional (rmode 0))
  (if-let1 port (make-console-stdin-port  rmode) (current-input-port  port))
  (if-let1 port (make-console-stdout-port rmode) (current-output-port port))
  (if-let1 port (make-console-stderr-port rmode) (current-error-port  port))
  (undefined))

