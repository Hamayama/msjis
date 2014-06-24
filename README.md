# msjis

![image](image.png)

## 概要
- Windows のコマンドプロンプトで Gauche(gosh.exe) を使うときに、  
  日本語(CP932)の表示と入力を可能とするモジュールです。


## インストール方法
- msjis.scm を Gauche でロード可能なフォルダにコピーします。  
  (例えば (gauche-site-library-directory) で表示されるフォルダ等)


## 使い方
- 対話環境にする場合
```
    (use msjis)
    (msjis-repl)
```

- 対話環境にしない場合
```
    (use msjis)
    (msjis-mode)
```


## 注意事項
1. コンソールの入出力についてのみ文字コードが変換されます。  
   (ファイルの読み書きやリダイレクトの文字コードは変換されません)

2. Windows 8でまれにキー入力を受け付けなくなることがあります。  
   (このプログラム以外でも発生するのでコマンドプロンプトの問題かも)

3. Ctrl-Cで対話環境を抜ける場合、2回押す必要があります(原因不明)。


## 参考情報
1. コマンドプロンプトで Gauche プログラミング - 主題のない日記  
   http://saito.hatenablog.jp/entry/2014/04/14/104006  
   (ここのコードを元に改造、デバッグしました。)

2. How to redirect STDOUT generated using WriteConsole in kernel32.dll?  
   http://social.msdn.microsoft.com/Forums/vstudio/en-US/716f2f70-9eed-4b96-9f43-f967605f307f/how-to-redirect-stdout-generated-using-writeconsole-in-kernel32dll?forum=netfxbcl  
   (Win32APIのReadConsole(),WriteConsole()は、リダイレクトありのときは使えない。  
   リダイレクトの有無はGetConsoleMode()が成功するかどうかで判定できる。)


## 環境等
- OS
 - Windows XP Home SP3
 - Windows 8 (64bit)
- 言語
 - Gauche v0.9.4-rc0
 - Gauche v0.9.3.3

## 履歴
- 2014-6-2  v1.00 (初版)
- 2014-6-4  v1.01 Windows XPで行頭の文字が「g」に化けてエラーになる場合がある件の対策
- 2014-6-4  v1.02 デバッグ表示埋め込み(今はコメントアウトしてある)
- 2014-6-4  v1.03 CP932の2バイト文字のチェックを修正
- 2014-6-7  v1.04 コメント修正のみ
- 2014-6-9  v1.05 1文字入出力の処理を整理  
  古い環境用に(msjis-repl2)と(msjis-mode2)を追加(エラーが発生する場合あり)
- 2014-6-10 v1.06 デバッグ表示処理整理(今はコメントアウトしてある)
- 2014-6-11 v1.07 Bad file descriptorエラーの対策  
  1文字入出力の処理方法2を採用  
  これにともない(msjis-repl2)と(msjis-mode2)は削除
- 2014-6-11 v1.08 exportの削除もれ修正
- 2014-6-13 v1.09 コメント修正のみ
- 2014-6-24 v1.10 コメント修正のみ
- 2014-6-24 v1.11 コメント修正のみ


(2014-6-24)
