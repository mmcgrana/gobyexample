rem this is a demo file.
@rem
@echo off

call c:\temp.bat somearg
call :lab somearg
rem This next one is wrong in the vim lexer!
call c:temp.bat

echo "Hi!"
echo hi
echo on
echo off
echo.
@echo off
if exist *.log echo The log file has arrived.
rem These are all escapes, also done incorrectly by the vim lexer
echo ^^ ^> ^< ^|

x=beginning
setlocal
x = new text
endlocal

echo testrem x
echo test rem x

for %%var in (*.jpg) do echo %%var
for /D %%var in (a b c) do echo %%var
for /R C:\temp %%var in (*.jpg) do iexplore.exe %%var
rem Vim has this one wrong too.
for /L %%var in (10,-1,1) do echo %%var
for /F %%var in ("hi!") do echo %%var
for /F "eol=c,skip=1,usebackq" %%var in (`command`) do echo %%var %~l %~fl %~dl %~pl %~nl %~xl %~sl %~al %~tl %~zl %~$PATH:l %~dpl %~dp$PATH:l %~ftzal

echo some file ?! > somefile.txt

set PATH=%PATH%;c:\windows

goto answer%errorlevel%
    :answer0
    echo Hi it's zero
    :answer1
    echo New

if exist a del a
else echo A is missing!


