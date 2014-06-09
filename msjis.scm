;; -*- coding: utf-8 -*-
;;
;; msjis.scm
;; 2014-6-9 v1.05
;;
;; ＜内容＞
;;   Windows のコマンドプロンプトで Gauche(gosh.exe) を使うときに、
;;   日本語(CP932)の表示と入力を可能とするモジュールです。
;;
;; ＜注意事項＞
;;   (1)Gauche の開発最新版が必要です(v0.9.3.3ではエラーになります)。
;;
;;   (2)コンソールの入出力についてのみ文字コードが変換されます。
;;      (ファイルの読み書きやリダイレクトの文字コードは変換されません)
;;
;;   (3)Windows XPで入力文字が文字化けする場合があります(原因不明)。
;;
;;   (4)Windows 8でまれにキー入力を受け付けなくなることがあります。
;;      (このプログラム以外でも発生するのでコマンドプロンプトの問題かも)
;;
;;   (5)Windows XPで入力文字が文字化けする場合、もしくは、
;;      Gaucheのバージョンがv0.9.3.3以下の場合は、
;;      (msjis-repl2)もしくは(msjis-mode2)が使えるかもしれません。
;;      ただしエラーが発生する場合があります(原因不明)。
;;
;; ＜インストール＞
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
(define-module msjis
  (use gauche.charconv)
  (use gauche.vport)
  (use gauche.uvector)
  (use os.windows)
  ;(use gauche.version)
  (export msjis-repl msjis-mode msjis-repl2 msjis-mode2))
(select-module msjis)

;; リダイレクトありかチェックする関数
;; (リダイレクトの有無はWin32APIのGetConsoleMode()が成功するかどうかで判定できる)
(define (redirected-handle? hdl)
  (guard (exc
          ((<system-error> exc) #t))
         (sys-get-console-mode hdl) #f))



;; 1文字入出力の処理方式1
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
      (let1 chr (string-ref (ces-convert (u8vector->string buf) 'UTF-16LE) 0)
        ;(format #t "~8,'0x;" (char->integer chr))
        chr))))

(define (make-putc-console hdl)
  (lambda (chr)
    (sys-write-console hdl (string chr))))

(define (make-puts-console hdl)
  (lambda (str)
    (sys-write-console hdl str)))



;; 1文字入出力の処理方式2 (古い環境用)
;;   read-block!とwrite-blockを使う。
;;   コードページがCP932であることが前提。
;;   ＜既知の問題点＞
;;     (1)<virtual-input-port>の作成時に、cutで包んで渡さないと以下のエラーが出る。原因不明。
;;          *** SYSTEM-ERROR: read failed on #<iport (stdin) 00a8df50>: Bad file descriptor
;;        また、<virtual-output-port>の作成時にも、cutで包んで渡さないと以下のエラーが出る。
;;          *** SYSTEM-ERROR: write failed on #<oport (stdout) 00a8dee0>: Bad file descriptor
;;        どちらのエラーも最初しばらく動いてその後発生する。原因不明。
;;
;;     (2)Windows 8でリダイレクト入力時に、
;;          *** SYSTEM-ERROR: read failed on #<iport (stdin) 00a8df50>: Bad file descriptor
;;        が発生することがある。原因不明。
;;
;;     (3)このモジュールで(use gauche.version)を追加すると、
;;        Windows XPでリダイレクト入力時に、
;;          *** SYSTEM-ERROR: read failed on #<iport (stdin) 00a8df50>: Bad file descriptor
;;        が常に発生する。原因不明。
;;
(define (make-getc-console2 port)
  (lambda ()
    (let1 buf (make-u8vector 2 0)
      (read-block! buf port 0 1)
      ;; CP932の2バイト文字のチェック
      (let1 b (u8vector-ref buf 0)
        (if (or (and (>= b #x81) (<= b #x9F)) (and (>= b #xE0) (<= b #xFC)))
          (read-block! buf port 1 2)))
      (let1 chr (string-ref (ces-convert (u8vector->string buf) 'CP932) 0)
        ;(format #t "~8,'0x;" (char->integer chr))
        chr))))

(define (make-putc-console2 port)
  (lambda (chr)
    (let1 buf (string->u8vector (ces-convert (string chr) (gauche-character-encoding) 'CP932))
      ;(display (map (cut format #f "0x~2,'0X" <>) (u8vector->list buf)) port)
      (write-block buf port)
      (flush port))))

(define (make-puts-console2 port)
  (lambda (str)
    (let1 buf (string->u8vector (ces-convert str (gauche-character-encoding) 'CP932))
      ;(display (map (cut format #f "0x~2,'0X" <>) (u8vector->list buf)) port)
      (write-block buf port)
      (flush port))))



;; 標準入力の変換ポートの作成
(define (make-console-stdin-port mode)
  (let1 hdl (sys-get-std-handle STD_INPUT_HANDLE)
    (if (redirected-handle? hdl) #f
      (if (not (= mode 2))
        (make <virtual-input-port> :getc (make-getc-console hdl))
        (let1 f1 (make-getc-console2 (standard-input-port))
          ;(display "!1 " (standard-output-port))
          (make <virtual-input-port> :getc (cut f1)))
        ))))

;; 標準出力の変換ポートの作成
(define (make-console-stdout-port mode)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (if (redirected-handle? hdl) #f
      (if (not (= mode 2))
        (make <virtual-output-port> :putc (make-putc-console hdl) :puts (make-puts-console hdl))
        (let ((f1 (make-putc-console2 (standard-output-port)))
              (f2 (make-puts-console2 (standard-output-port))))
          ;(display "!2 " (standard-output-port))
          (make <virtual-output-port> :putc (cut f1 <>) :puts (cut f2 <>)))
        ))))

;; 標準エラー出力の変換ポートの作成
(define (make-console-stderr-port mode)
  (let1 hdl (sys-get-std-handle STD_ERROR_HANDLE)
    (if (redirected-handle? hdl) #f
      (if (not (= mode 2))
        (make <virtual-output-port> :putc (make-putc-console hdl) :puts (make-puts-console hdl))
        (let ((f1 (make-putc-console2 (standard-error-port)))
              (f2 (make-puts-console2 (standard-error-port))))
          ;(display "!3 " (standard-output-port))
          (make <virtual-output-port> :putc (cut f1 <>) :puts (cut f2 <>)))
        ))))



;; 変換ポートを設定してREPLを起動する
(define (msjis-repl :optional (mode 1))
  (with-ports (make-console-stdin-port  mode)
              (make-console-stdout-port mode)
              (make-console-stderr-port mode)
              (lambda ()
                ;(print "MSJISモード")
                (read-eval-print-loop))))

;; 変換ポートの設定のみ実施する
(define (msjis-mode :optional (mode 1))
  (if-let1 port (make-console-stdin-port  mode) (current-input-port  port))
  (if-let1 port (make-console-stdout-port mode) (current-output-port port))
  (if-let1 port (make-console-stderr-port mode) (current-error-port  port))
  (undefined))

;; 古い環境用(エラーが発生する場合あり)
(define (msjis-repl2)
  (msjis-repl 2))

(define (msjis-mode2)
  (msjis-mode 2))

