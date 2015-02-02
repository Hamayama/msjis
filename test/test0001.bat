set GOSH_EXE=gosh
rem set GOSH_EXE="C:\Program Files (x86)\Gauche_0933_orig\bin\gosh.exe"

%GOSH_EXE%
rem %GOSH_EXE%
%GOSH_EXE% -umsjis
rem %GOSH_EXE% -umsjis
%GOSH_EXE% c:\test0001.scm
rem %GOSH_EXE% c:\test0002.scm
%GOSH_EXE% -l"c:\test0001.scm"
rem %GOSH_EXE% -l"c:\test0002.scm"
%GOSH_EXE% c:\test0001.scm < c:\test0001_input.txt
rem %GOSH_EXE% c:\test0002.scm < c:\test0002_input.txt
%GOSH_EXE% c:\test0001.scm > c:\test0001_output.txt
rem %GOSH_EXE% c:\test0002.scm > c:\test0002_output.txt
pause
