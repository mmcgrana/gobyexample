@ @@ echo off
::This is an example of the Windows batch language.

setlocal EnableDelayedExpansion
(cls)
set/a^
_te^
sts^
=0,^
_"pa"^
ssed=0^
0
set,/a title= Batch test
title=%,/a title%
echo^ %~nx0,^ the>,con comprehensive testing suite
ver
echo(

if cmdextversion 2 goto =)
goto :fail

     :)
echo Starting tests at:
date/t & time/t
echo(

if '%*'=='--help' (
  echo Usage: %~nx0 [--help]
  echo   --help: Display this help message and quit.
  shift
  goto :exit comment) else rem

(call :comments)
call ::io+x
call:control:x
call::internal x

:exit
if /i !_tests!==!_passed! (
  color 02
) else if !*==* (
  color c
  if not defined _exit^
Code set _exit^
Code=1
)
set _percentage=NaN
if defined _tests (
  if !_tests! neq 0 (set/a_percentage=100*_passed/_tests)
)
echo(
if !_percentage!==NaN ( echo(There were no tests^^! & color e
) else ( echo Tests passed: %_passed%/%_tests% (%_percentage%%%^) )
pause
color
title
endlocal
exit /b %_exitCode%

x:fail
rem This should never happen.
echo Internal error 1>& 269105>>&2
set /a _exitCode=0x69+(0105*1000)
break
goto :exit

:comments
(rem )/?
)
rem "comment^
(rem.) & set /a _tests+=1
(rem) & goto :fail
(rem. ) & (rem. comment ) & echo Test %_tests%: Comments
rem )
)
)|comment
)(
:: comment
goto :comments^^1:comment
:comments^^1 comment
if(1==1) goto :comments^
^1
rem^ /?
rem ^
^
goto :comments^
2+comment
goto :fail
:comments2
rem >^
if 1==1 (goto :comments3)
:comments3)
goto :fail
:comments3
rem comment^
goto:fail
rem.comment comment^
goto fail
rem "comment comment"^
goto fail
rem comment comment^
if "1==1" equ "1==1" goto comments4
goto fail
:comments4
rem comment"comment^
set /a _passed+=1
GOTO :EOF
goto :fail

:IO
SET /A _tests+=1 & Echo Test !_tests:*!==^!: I/O
verify on
pushd .
if exist temp echo  temp already exists. & goto :eof
md temp
cd temp
mkdir 2>nul temp
chdir temp
>cd  echo Checking drive...
>>cd echo must be C or else this won't work
for /f "tokens=* usebackq" %%G in ("cd
) do (<nul set /p="%%G ")
echo(
DEL cd
if not "%cd:~0,3%"=="C:\" (
  call call echo  Wrong drive (should be C^):
  vol
  goto :test)
>test0^
.bat echo rem Machine-generated; do not edit
call echo set /a _passed+=1 >>test0.bat
type test0.bat >"test 1.bat
ren "test 1.bat" test2.bat
rename test2.bat test.bat
caLL ^
C:test
del test.bat 2>nul
2>NUL erase test0.bat
popd
rd temp\temp
rmdir temp
VERIFY OFF
goto:eof

:control
set /a _tests+=1
echo Test %_tests%: Control statements
set "_iterations=0">nul
for %%G in (,+,,-,
) do @(
  for /l %%H in (,-1;;-1	-3,) do (
    for /f tokens^=1-2^,5 %%I in ("2 %%H _ _ 10") do (
      for /f "tokens=1 usebackq" %%L in ( `echo %%G%%J ``` `
`  `    ) do ( for /f "tokens=2" %%M in ('echo ' %%L0 '
'  '      ) do ( set /a _iterations+=(%%M%%M^)
        )
      )
    )
  )
)
if exist %~nx0 if not exist %~nx0 goto :fail
if exist %~nx0 (
  if not exist %~nx0 goto :fail
) else (
  if exist %~nx0 goto :fail
)
if /i %_iterations% gtr -2 (
  if /i %_iterations% geq -1 (
    if /i %_iterations% lss 1 (
      if /i %_iterations% leq 0 (
        if /i %_iterations% equ 0 (
          if 1 equ 01 (
            if 1 neq "01" (
              if "1" neq 01 (
                set /a _passed+=1))))))))
) comment
goto :eof

:internal
set /a _tests+=1
echo Test %_tests%: Internal commands
keys on
mklink 2>nul
>nul path %path%
>nul dpath %dpath%
if not defined prompt prompt $P$G
prompt !prompt:~!rem/ $H?
echo on
rem/?
@echo off
rem(/?>nul
rem )/? >nul
(rem (/?) >nul
rem /?>nul
rem^/?>nul
if/?>nul || if^/^?>nul || if /?>nul || if x/? >nul
for/?>nul && for^/^?>nul && for /?>nul && for x/? >nul && for /?x >nul
goto/?>nul && goto^/? && goto^ /? && goto /^
? && goto /?>nul && goto:/? >nul && goto ) /? ) >nul && (goto /? )>nul
=set+;/p extension'),=.bat
for /f "tokens=2 delims==" %%G in ( 'assoc %+;/p extension'),%'
 ) do (
  assoc 2>nul %+;/p extension'),:*.=.%=%%G
  ftype 1>nul %%G
) &>nul ver
if errorlevel 0 if not errorlevel 1 set /a _passed+=1
goto :eof
:/?
goto :fail
