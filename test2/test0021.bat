set GOSH_EXE=gosh
rem set GOSH_EXE="C:\Program Files (x86)\Gauche_0933_orig\bin\gosh.exe"

%GOSH_EXE% c:\test0021A.scm < c:\test0021_input.txt  > c:\test0021_outputA.txt
%GOSH_EXE% c:\test0021B.scm < c:\test0021_input.txt  > c:\test0021_outputB.txt
%GOSH_EXE% c:\test0021C.scm < c:\test0021_inputC.txt > c:\test0021_outputC.txt
%GOSH_EXE% c:\test0021D.scm < c:\test0021_inputC.txt > c:\test0021_outputD.txt
rem %GOSH_EXE% c:\test0022A.scm < c:\test0022_input.txt  > c:\test0022_outputA.txt
rem %GOSH_EXE% c:\test0022B.scm < c:\test0022_input.txt  > c:\test0022_outputB.txt
rem %GOSH_EXE% c:\test0022C.scm < c:\test0022_inputC.txt > c:\test0022_outputC.txt
rem %GOSH_EXE% c:\test0022D.scm < c:\test0022_inputC.txt > c:\test0022_outputD.txt
pause
