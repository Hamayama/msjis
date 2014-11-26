;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2014-11-26 v1.22
;;
;; ＜内容＞
;;   Windows のコマンドプロンプトで Gauche(gosh.exe) を使うときに、
;;   日本語(CP932)の表示と入力を可能とするモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/msjis
;;
(define-module msjis
  (use gauche.charconv)
  (use gauche.vport)
  (use gauche.uvector)
  (use os.windows)
  (export 
    msjis-mode
    make-msjis-stdin-port
    make-msjis-stdout-port
    make-msjis-stderr-port))
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
  (guard (exc ((<system-error> exc) #t))
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
(define (make-msjis-getc port ces)
  (lambda ()
    (let* ((chr 0)
           (buf (make-u8vector 2 0))
           (ret (read-block! buf port 0 1)))
      ;; CP932の2バイト文字のチェック
      (if (not (eof-object? ret))
        (let1 b (u8vector-ref buf 0)
          (if (or (and (>= b #x81) (<= b #x9F)) (and (>= b #xE0) (<= b #xFC)))
            (set! ret (read-block! buf port 1 2)))))
      ;(debug-print-buffer buf)
      (cond
       ;; ファイル終端(EOF)のチェック
       ((eof-object? ret)
        (set! chr (eof-object))
        ;(debug-print-str "[EOF]")
        )
       ;; 文字コードの変換(CP932→内部コード)
       (else
        (set! chr (string-ref (ces-convert (u8vector->string buf) ces) 0))
        ;(debug-print-char-code chr)
        ))
      chr)))

(define (make-msjis-putc port c932 crlf ces)
  (lambda (chr)
    (msjis-puts-sub (string chr) port c932 crlf ces)))

(define (make-msjis-puts port c932 crlf ces)
  (lambda (str)
    (msjis-puts-sub str port c932 crlf ces)))

(define (msjis-puts-sub str port c932 crlf ces)
  (let1 buf 0
    ;; 改行コードの変換(LF→CRLF)
    (if crlf
      (set! str (regexp-replace-all #/\n/ str "\r\n")))
    ;; 文字コードの変換(内部コード→CP932)
    (if c932
      (set! buf (string->u8vector (ces-convert str (gauche-character-encoding) ces)))
      (set! buf (string->u8vector str)))
    ;(debug-print-buffer buf)
    (write-block buf port)
    (flush port)))



;; 標準入力の変換ポートの作成
(define (make-msjis-stdin-port :optional (rmode 0) (ces 'CP932))
  (receive (c932 crlf) (get-msjis-param rmode (sys-get-std-handle STD_INPUT_HANDLE))
    (if c932
      (make <virtual-input-port>
            :getc (make-msjis-getc (standard-input-port) ces))
      #f)))

;; 標準出力の変換ポートの作成
(define (make-msjis-stdout-port :optional (rmode 0) (ces 'CP932))
  (receive (c932 crlf) (get-msjis-param rmode (sys-get-std-handle STD_OUTPUT_HANDLE))
    (if (or c932 crlf)
      (make <virtual-output-port>
            :putc (make-msjis-putc (standard-output-port) c932 crlf ces)
            :puts (make-msjis-puts (standard-output-port) c932 crlf ces))
      #f)))

;; 標準エラー出力の変換ポートの作成
(define (make-msjis-stderr-port :optional (rmode 0) (ces 'CP932))
  (receive (c932 crlf) (get-msjis-param rmode (sys-get-std-handle STD_ERROR_HANDLE))
    (if (or c932 crlf)
      (make <virtual-output-port>
            :putc (make-msjis-putc (standard-error-port) c932 crlf ces)
            :puts (make-msjis-puts (standard-error-port) c932 crlf ces))
      #f)))

;; 変換用パラメータの取得
(define (get-msjis-param rmode hdl)
  (let ((c932 #f)
        (crlf #f))
    (cond ((redirected-handle? hdl)
           (if (or (= rmode 2) (= rmode 3)) (set! c932 #t))
           (if (or (= rmode 1) (= rmode 3)) (set! crlf #t)))
          (else
           (set! c932 #t)
           (set! crlf #f)))
    (values c932 crlf)))



;; 変換ポートの設定
(define (msjis-mode :optional (rmode 0) (ces 'CP932))
  (if-let1 port (make-msjis-stdin-port  rmode ces) (current-input-port  port))
  (if-let1 port (make-msjis-stdout-port rmode ces) (current-output-port port))
  (if-let1 port (make-msjis-stderr-port rmode ces) (current-error-port  port))
  (undefined))

