;; -*- coding: utf-8 -*-

(use msjis)
(msjis-mode 1)

(define *inputstr* "")

(display "入力をどうぞ > ")
(flush)
(set! *inputstr* (read-line))
(if (equal? *inputstr* "") (set! *inputstr* (read-line)))
(print "入力データは「" *inputstr* "」です。")
