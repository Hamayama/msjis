;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2016-3-16 v1.40
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
  (display (map (cut format "~2,'0Xh" <>) (u8vector->list buf)) (standard-output-port))
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



;; 1文字入力の変換処理
(define (make-msjis-getc port hdl ces use-api)
  (if use-api
    ;; Windows API 使用のとき
    (make-msjis-getc-sub port hdl 'UTF-16LE #t zero? 4 2 2)
    ;; Windows API 未使用のとき
    (make-msjis-getc-sub port hdl ces #f eof-object? 6 0 1)))

;; 1文字入力の変換処理サブ
(define (make-msjis-getc-sub port hdl ces use-api eofcheckfunc maxbytes extrabytes readbytes)
  ;; 手続きを作って返す
  (lambda ()
    (let ((chr #\null)
          (str "")
          (i   0)
          ;; ReadConsole()がバッファサイズより1バイト多く書き込む件に対応
          (buf (make-u8vector (+ maxbytes extrabytes) 0))
          (ret 0))
      ;; 文字が完成するまで指定バイトずつ読み込む
      (let loop ()
        (if use-api
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
          (guard (exc ((<error> exc)
                       ;; 文字が未完成のとき
                       (when (< (+ i readbytes) maxbytes)
                         (set! i (+ i readbytes))
                         (loop))))
            ;; 文字が完成したとき
            (set! chr (string-ref str 0))))))
      ;(debug-print-buffer (u8vector-copy buf 0 (+ i readbytes)))
      chr)))

;; 1文字出力の変換処理
(define (make-msjis-putc port conv crlf hdl ces use-api)
  (if use-api
    ;; Windows API 使用のとき
    (lambda (chr) ((make-msjis-puts-sub1 hdl 4096) (string chr)))
    ;; Windows API 未使用のとき
    (lambda (chr) ((make-msjis-puts-sub2 port conv crlf ces) (string chr)))))

;; 文字列出力の変換処理
(define (make-msjis-puts port conv crlf hdl ces use-api)
  (if use-api
    ;; Windows API 使用のとき
    (make-msjis-puts-sub1 hdl 4096)
    ;; Windows API 未使用のとき
    (make-msjis-puts-sub2 port conv crlf ces)))

;; 文字列出力の変換処理サブ1 (Windows API 使用)
(define (make-msjis-puts-sub1 hdl maxchars)
  ;; 手続きを作って返す
  (lambda (str)
    ;; 指定文字数ずつ書き出す
    (let loop ((i 0))
      (cond
       ((<= (string-length str) (+ i maxchars))
        (sys-write-console hdl (string-copy str i)))
       (else
        (sys-write-console hdl (string-copy str i (+ i maxchars)))
        (loop (+ i maxchars)))))))

;; 文字列出力の変換処理サブ2 (Windows API 未使用)
(define (make-msjis-puts-sub2 port conv crlf ces)
  ;; 手続きを作って返す
  (lambda (str)
    ;; 改行コードの変換(LF→CRLF)
    ;; 文字コードの変換(内部コード→外部コード)
    (let* ((str2 (if crlf (regexp-replace-all #/\n/ str "\r\n") str))
           (str3 (if conv (ces-convert str2 (gauche-character-encoding) ces) str2))
           (buf  (string->u8vector str3)))
      ;(debug-print-buffer buf)
      ;; バイト列を書き出す
      (write-block buf port)
      (flush port))))



;; 標準入力の変換ポートの作成
(define (make-msjis-stdin-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl ces use-api)
      (get-msjis-param rmode (sys-get-std-handle STD_INPUT_HANDLE) ces use-api #t)
    (if conv
      (make <virtual-input-port>
        :getc (make-msjis-getc (standard-input-port) hdl ces use-api))
      #f)))

;; 標準出力の変換ポートの作成
(define (make-msjis-stdout-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl ces use-api)
      (get-msjis-param rmode (sys-get-std-handle STD_OUTPUT_HANDLE) ces use-api #f)
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-msjis-putc (standard-output-port) conv crlf hdl ces use-api)
        :puts (make-msjis-puts (standard-output-port) conv crlf hdl ces use-api))
      #f)))

;; 標準エラー出力の変換ポートの作成
(define (make-msjis-stderr-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl ces use-api)
      (get-msjis-param rmode (sys-get-std-handle STD_ERROR_HANDLE) ces use-api #f)
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-msjis-putc (standard-error-port) conv crlf hdl ces use-api)
        :puts (make-msjis-puts (standard-error-port) conv crlf hdl ces use-api))
      #f)))

;; 変換用パラメータの取得
(define (get-msjis-param rmode hdl ces use-api stdin-flag)
  (let ((rdir     (redirected-handle? hdl))
        (conv     #f)
        (crlf     #f)
        (ces2     ces)
        (use-api2 use-api))
    ;; 文字エンコーディングが未指定のときは、コードページを取得して自動設定する
    (if (not ces2)
      (let1 cp (if stdin-flag (sys-get-console-cp) (sys-get-console-output-cp))
        (case cp
          ((65001) (set! ces2 'UTF-8)
                   (set! use-api2 #t))
          (else    (set! ces2 (string->symbol (format "CP~d" cp)))))))
    ;; 文字エンコーディングのチェック
    (if stdin-flag
      (check-ces ces2 (gauche-character-encoding) ces2)
      (check-ces (gauche-character-encoding) ces2 ces2))
    ;; 他の変換用パラメータの取得
    (set! conv (if rdir (if (or (= rmode 2) (= rmode 3)) #t #f) #t))
    (set! crlf (if rdir (if (or (= rmode 1) (= rmode 3)) #t #f) #f))
    (if rdir (set! use-api2 #f))
    ;; 結果を多値で返す
    (values conv crlf hdl ces2 use-api2)))

;; 文字エンコーディングのチェック
(define (check-ces ces-from ces-to ces-error)
  (if (not (ces-conversion-supported? ces-from ces-to))
    (errorf "ces \"~a\" is not supported" ces-error)))



;; 変換ポートの設定
(define (msjis-mode :optional (rmode 0) (ces '#f) (use-api #f))
  (if-let1 port (make-msjis-stdin-port  rmode ces use-api) (current-input-port  port))
  (if-let1 port (make-msjis-stdout-port rmode ces use-api) (current-output-port port))
  (if-let1 port (make-msjis-stderr-port rmode ces use-api) (current-error-port  port))
  (undefined))

