;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2017-9-6 v1.60
;;
;; ＜内容＞
;;   Windows のコマンドプロンプトで Gauche を使うときに、
;;   日本語 (CP932) の表示と入力を可能とするモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/msjis
;;
(define-module msjis
  (use gauche.charconv)
  (use gauche.vport)
  (use gauche.uvector)
  (use gauche.sequence) ; for-each-with-index用
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

;; リダイレクト有無のチェック
(define (redirected-handle? hdl)
  (guard (ex ((<system-error> ex) #t))
    (sys-get-console-mode hdl) #f))

;; 標準入出力のハンドルの保持
;; (保持しておかないとエラーになる。Gauche v0.9.4 では修正済み)
(define stdin-handle  (sys-get-std-handle STD_INPUT_HANDLE))
(define stdout-handle (sys-get-std-handle STD_OUTPUT_HANDLE))
(define stderr-handle (sys-get-std-handle STD_ERROR_HANDLE))

;; 入力については、Windows API は Unicode 版を使用する(将来用)
;; (現状は、Gauche の内部エンコーディングが utf-8 のときのみ
;;  Unicode 版になっている)
(define sys-read-console
  (if (global-variable-bound? 'os.windows 'sys-read-console-w)
    (with-module os.windows sys-read-console-w)
    (with-module os.windows sys-read-console)))



;; 1文字入力の変換処理(内部処理用)
(define (make-msjis-getc port hdl-type ces ces2 use-api)
  (if use-api
    ;; Windows API 使用のとき
    (make-msjis-getc-sub port hdl-type 'UTF-16LE ces2 #t 4 2 2)
    ;; Windows API 未使用のとき
    (make-msjis-getc-sub port hdl-type ces       ces2 #f 6 0 1)))

;; 1文字入力の変換処理サブ(内部処理用)
(define (make-msjis-getc-sub port hdl-type ces ces2 use-api maxbytes extrabytes readbytes)
  ;; 手続きを作って返す
  (lambda ()
    (rlet1 chr #\null
      ;; ReadConsole がバッファサイズより1バイト多く書き込む件の対策
      (let ((buf (make-u8vector (+ maxbytes extrabytes) 0))
            (hdl (if use-api (sys-get-std-handle hdl-type) #f))
            (i   0))
        ;; 文字が完成するまで指定バイトずつ読み込む
        (let loop ()
          (if (if use-api
                (zero? (sys-read-console hdl (uvector-alias <u8vector> buf i (+ i readbytes))))
                (eof-object? (read-block! buf port i (+ i readbytes))))
            ;; ファイル終端(EOF)のとき
            (begin
              ;(debug-print-str "[EOF]")
              (set! chr (eof-object)))
            ;; ファイル終端(EOF)以外のとき
            ;; 文字コードの変換(外部コード→内部コード)
            (let1 str (ces-convert (u8vector->string buf 0 (+ i readbytes)) ces ces2)
              (guard (ex ((<error> ex)
                          ;; 文字が未完成のとき
                          (when (< (+ i readbytes) maxbytes)
                            (set! i (+ i readbytes))
                            (loop))))
                ;; 文字が完成したとき
                (set! chr (string-ref str 0))))))
        ;(debug-print-buffer (u8vector-copy buf 0 (+ i readbytes)))
        ))))



;; 文字列出力の変換処理(内部処理用)
(define (make-msjis-puts port conv crlf hdl-type ces ces2 use-api)
  (if use-api
    ;; Windows API 使用のとき
    (make-msjis-puts-sub1 hdl-type ces ces2 4096)
    ;; Windows API 未使用のとき
    (make-msjis-puts-sub2 port conv crlf ces ces2)))

;; 文字列出力の変換処理サブ1 (Windows API 使用)(内部処理用)
(define (make-msjis-puts-sub1 hdl-type ces ces2 maxchars)
  ;; Windows API の調整用サブ
  (define (sys-write-console-sub hdl str)
    (cond-expand
     (gauche.ces.utf8
      ;; Windows API が Unicode 版のとき
      ;; (サロゲートペアの文字の折り返しの不具合対策)
      (let* ((cinfo (sys-get-console-screen-buffer-info hdl))
             (w     (+ 1 (- (slot-ref cinfo 'window.right)
                            (slot-ref cinfo 'window.left))))
             (i1    0))
        (for-each-with-index
         (lambda (i2 c)
           (when (>= (char->integer c) #x10000)
             (sys-write-console hdl (string-copy str i1 i2))
             (set! i1 i2)
             (let* ((cinfo (sys-get-console-screen-buffer-info hdl))
                    (x     (slot-ref cinfo 'cursor-position.x))
                    (y     (slot-ref cinfo 'cursor-position.y)))
               (when (> x (- w 4))
                 (sys-set-console-cursor-position hdl (- w 1) y)
                 (sys-write-console hdl " ")))))
         str)
        (sys-write-console hdl (string-copy str i1))))
     (else
      ;; Windows API が ANSI 版のとき
      ;; (文字コードの変換が必要)
      (set! str (ces-convert str ces2 ces))
      (sys-write-console hdl str))))
  ;; 手続きを作って返す
  (lambda (str/char)
    (let ((str (x->string str/char))
          (hdl (sys-get-std-handle hdl-type)))
      ;; 指定文字数ずつ書き出す
      (let loop ((i 0))
        (cond
         ((<= (string-length str) (+ i maxchars))
          (sys-write-console-sub hdl (string-copy str i)))
         (else
          (sys-write-console-sub hdl (string-copy str i (+ i maxchars)))
          (loop (+ i maxchars))))))))

;; 文字列出力の変換処理サブ2 (Windows API 未使用)(内部処理用)
(define (make-msjis-puts-sub2 port conv crlf ces ces2)
  ;; 手続きを作って返す
  (lambda (str/char)
    (let1 str (x->string str/char)
      (if crlf (set! str (regexp-replace-all #/\n/ str "\r\n")))
      (if conv (set! str (ces-convert str ces2 ces)))
      (let1 buf (string->u8vector str)
        ;(debug-print-buffer buf)
        ;; バイト列を書き出す
        (write-block buf port)
        (flush port)))))



;; 変換用パラメータの取得(内部処理用)
(define (get-msjis-param rmode hdl-type ces use-api stdin-flag)
  ;; 文字エンコーディングのチェック
  (define (check-ces ces1 ces2 ces-err)
    (unless (ces-conversion-supported? ces1 ces2)
      (errorf "ces \"~a\" is not supported" ces-err)))
  ;; 変換用パラメータの取得
  (let* ((rdir (redirected-handle? (sys-get-std-handle hdl-type)))
         (conv (if rdir (if (or (= rmode 2) (= rmode 3)) #t #f) #t))
         (crlf (if rdir (if (or (= rmode 1) (= rmode 3)) #t #f) #f))
         (ces2 (gauche-character-encoding)))
    ;; 文字エンコーディングが未指定のときは、コードページを取得して自動設定する
    (unless ces
      (let1 cp (if stdin-flag (sys-get-console-cp) (sys-get-console-output-cp))
        (case cp
          ((65001)
           (set! ces 'UTF-8)
           (set! use-api #t))
          (else
           (set! ces (string->symbol (format "CP~d" cp)))))))
    ;; Gauche の内部エンコーディングが sjis のときのエラー対策
    ;; (円記号を iconv が変換できずエラーになるケースがある。
    ;;  対策として、文字エンコーディングを 'SJIS から 'CP932 に変更する)
    (cond-expand
     (gauche.ces.sjis
      (set! ces2 'CP932)
      (if (#/^(SJIS|SHIFT[\-_]?JIS)$/i (x->string ces))
        (set! ces 'CP932)))
     (else))
    ;; 文字エンコーディングのチェック
    (if stdin-flag
      (check-ces ces ces2 ces)
      (check-ces ces2 ces ces))
    ;; リダイレクトありのときは Windows API は使用不可
    (if rdir (set! use-api #f))
    ;; 結果を多値で返す
    (values conv crlf hdl-type ces ces2 use-api)))



;; 標準入力の変換ポートの作成
(define (make-msjis-stdin-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl-type ces ces2 use-api)
      (get-msjis-param rmode STD_INPUT_HANDLE ces use-api #t)
    (if conv
      (make <virtual-input-port>
        :getc (make-msjis-getc (standard-input-port) hdl-type ces ces2 use-api))
      #f)))

;; 標準出力の変換ポートの作成
(define (make-msjis-stdout-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl-type ces ces2 use-api)
      (get-msjis-param rmode STD_OUTPUT_HANDLE ces use-api #f)
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-msjis-puts (standard-output-port) conv crlf hdl-type ces ces2 use-api)
        :puts (make-msjis-puts (standard-output-port) conv crlf hdl-type ces ces2 use-api))
      #f)))

;; 標準エラー出力の変換ポートの作成
(define (make-msjis-stderr-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl-type ces ces2 use-api)
      (get-msjis-param rmode STD_ERROR_HANDLE ces use-api #f)
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-msjis-puts (standard-error-port) conv crlf hdl-type ces ces2 use-api)
        :puts (make-msjis-puts (standard-error-port) conv crlf hdl-type ces ces2 use-api))
      #f)))



;; 変換ポートの設定
(define (msjis-mode :optional (rmode 0) (ces '#f) (use-api #f))
  (if-let1 port (make-msjis-stdin-port  rmode ces use-api) (current-input-port  port))
  (if-let1 port (make-msjis-stdout-port rmode ces use-api) (current-output-port port))
  (if-let1 port (make-msjis-stderr-port rmode ces use-api) (current-error-port  port))
  (values))

