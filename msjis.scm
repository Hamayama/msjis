;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2015-2-11 v1.32
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
(define (make-msjis-getc port rdir hdl ces userwc)
  (if (and (not rdir) userwc)
    ;; リダイレクトなしでWin32APIのReadConsole()使用のとき
    (lambda () (msjis-getc-sub port hdl 'UTF-16LE #t zero? 4 2 2))
    ;; その他のとき
    (lambda () (msjis-getc-sub port hdl ces #f eof-object? 6 0 1))))

(define (msjis-getc-sub port hdl ces userwc eofcheckfunc maxbytes extrabytes readbytes)
  (let ((chr #\null)
        (str "")
        ;; ReadConsole()がバッファサイズより1バイト多く書き込む件に対応
        (buf (make-u8vector (+ maxbytes extrabytes) 0))
        (ret 0))
    ;; 文字が完成するまで指定バイトずつ読み込む
    (let loop ((i 0))
      (if userwc
        (set! ret (sys-read-console hdl (uvector-alias <u8vector> buf i (+ i readbytes))))
        (set! ret (read-block! buf port i (+ i readbytes))))
      (cond
       ;; ファイル終端(EOF)のとき
       ((eofcheckfunc ret)
        ;(debug-print-str "[EOF]")
        (set! chr (eof-object)))
       ;; ファイル終端(EOF)以外のとき
       (else
        ;; 文字コードの変換(外部コード→内部コード)
        (set! str (ces-convert (u8vector->string buf 0 (+ i readbytes)) ces))
        (cond
         ;; 文字が完成したとき
         ((> (string-length str) 0)
          ;(debug-print-char-code (string-ref str 0))
          (set! chr (string-ref str 0)))
         ;; 文字が未完成のとき
         (else
          (if (< (+ i readbytes) maxbytes)
            (loop (+ i readbytes)))
          ;; ここに何か書くと末尾再帰でなくなるので注意
          )))))
    ;(debug-print-buffer (u8vector-copy buf 0 (+ i readbytes)))
    chr))

(define (make-msjis-putc port conv crlf rdir hdl ces userwc)
  (if (and (not rdir) userwc)
    ;; リダイレクトなしでWin32APIのWriteConsole()使用のとき
    (lambda (chr) (sys-write-console hdl (string chr)))
    ;; その他のとき
    (lambda (chr) (msjis-puts-sub (string chr) port conv crlf ces))))

(define (make-msjis-puts port conv crlf rdir hdl ces userwc)
  (if (and (not rdir) userwc)
    ;; リダイレクトなしでWin32APIのWriteConsole()使用のとき
    (lambda (str) (sys-write-console hdl str))
    ;; その他のとき
    (lambda (str) (msjis-puts-sub str port conv crlf ces))))

(define (msjis-puts-sub str port conv crlf ces)
  (let1 buf 0
    ;; 改行コードの変換(LF→CRLF)
    (if crlf
      (set! str (regexp-replace-all #/\n/ str "\r\n")))
    ;; 文字コードの変換(内部コード→外部コード)
    (if conv
      (set! buf (string->u8vector (ces-convert str (gauche-character-encoding) ces)))
      (set! buf (string->u8vector str)))
    ;(debug-print-buffer buf)
    ;; バイト列を出力
    (write-block buf port)
    (flush port)))



;; 標準入力の変換ポートの作成
(define (make-msjis-stdin-port :optional (rmode 0) (ces 'CP932) (userwc #f))
  (check-ces ces (gauche-character-encoding) ces)
  (receive (conv crlf rdir hdl) (get-msjis-param rmode (sys-get-std-handle STD_INPUT_HANDLE))
    (if conv
      (make <virtual-input-port>
        :getc (make-msjis-getc (standard-input-port) rdir hdl ces userwc))
      #f)))

;; 標準出力の変換ポートの作成
(define (make-msjis-stdout-port :optional (rmode 0) (ces 'CP932) (userwc #f))
  (check-ces (gauche-character-encoding) ces ces)
  (receive (conv crlf rdir hdl) (get-msjis-param rmode (sys-get-std-handle STD_OUTPUT_HANDLE))
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-msjis-putc (standard-output-port) conv crlf rdir hdl ces userwc)
        :puts (make-msjis-puts (standard-output-port) conv crlf rdir hdl ces userwc))
      #f)))

;; 標準エラー出力の変換ポートの作成
(define (make-msjis-stderr-port :optional (rmode 0) (ces 'CP932) (userwc #f))
  (check-ces (gauche-character-encoding) ces ces)
  (receive (conv crlf rdir hdl) (get-msjis-param rmode (sys-get-std-handle STD_ERROR_HANDLE))
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-msjis-putc (standard-error-port) conv crlf rdir hdl ces userwc)
        :puts (make-msjis-puts (standard-error-port) conv crlf rdir hdl ces userwc))
      #f)))

;; 文字エンコーディングのチェック
(define (check-ces ces1 ces2 err_ces)
  (if (not (ces-conversion-supported? ces1 ces2))
    (errorf "ces \"~s\" is not supported" err_ces)))

;; 変換用パラメータの取得
(define (get-msjis-param rmode hdl)
  (let* ((rdir (redirected-handle? hdl))
         (conv (if rdir (if (or (= rmode 2) (= rmode 3)) #t #f) #t))
         (crlf (if rdir (if (or (= rmode 1) (= rmode 3)) #t #f) #f)))
    (values conv crlf rdir hdl)))



;; 変換ポートの設定
(define (msjis-mode :optional (rmode 0) (ces 'CP932) (userwc #f))
  (if-let1 port (make-msjis-stdin-port  rmode ces userwc) (current-input-port  port))
  (if-let1 port (make-msjis-stdout-port rmode ces userwc) (current-output-port port))
  (if-let1 port (make-msjis-stderr-port rmode ces userwc) (current-error-port  port))
  (undefined))

