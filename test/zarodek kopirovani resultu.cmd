@echo off

IF NOT "%~2"=="" IF "%~3"=="" GOTO START
ECHO Run this script frm the directory, where it is
ECHO This script requires the next parameters:
ECHO - 1 . destination directory (NOT ends witch slash)
ECHO - 2. subfolder which to restore, should start witch treenmae and ends with slash
ECHO Allways use forward slashes
ECHO Examples:
ECHO "%~nx0" "C:/User/myname/restoredphotos photos/

GOTO :EOF

:START

set SRCDIR=%~dp0

set DEST=%~1
set ROOT=%~2

setlocal EnableExtensions EnableDelayedExpansion

call :copy %ROOT% ../1/dva/tri/ctyri jedna/dva/tri/ctyri
call :copy %ROOT% ../1/dva/xxx/yyy   jedna/dva/xxx/yyy     
call :copy %ROOT% "../1/dvaxx/tttt"    "jedna/dvaxx/tttt"
call :copy %ROOT% "../1/dvaxx/tttt"    "jedna/sedum/rrrr"
call :copy %ROOT% "../1/dva/za/bi/ju"  "jedna/dva/za/bi/ju"
call :copy %ROOT% "../1/dva/za/bi/ju"  "jedna/osma/za/bi/juna/jedna/dva/tak/jo"


endlocal

exit /b 0

:copy [%1 - root path;%2 - source path;%3 - destination path ]
                                                
set ZARAZKA=68e5g6s44d5f7re8778921s323x5ds4f74ss07e
set stringa=%ZARAZKA%%~3
set "modified=!stringa:%ZARAZKA%%~1=!"

IF NOT "%modified%"=="%ZARAZKA%%~3" (
       Echo COPY "%SRCDIR%%~2" "%DEST%/%modified%""
    )  ELSE (
       Echo NEcopy DO %~3
    )

 
