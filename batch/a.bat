@echo off
rem https://www.tohoho-web.com/ex/bat.html#about
rem で勉強したこと

call :funcA "ABC" 123
call :funcB "XYZ" 456

exit /B 0

:funcA
    echo funcA(%1, %2)
    exit /B 0
:funcB
    echo funcB(%1, %2)
    exit /B 0

rem goto ラベルA
rem 
rem :ラベルA
rem setlocal
rem set a1=123
rem rem % と数字の間に~をつけると、引数がダブルクオーテーションで囲まれていればそれを除去します。
rem rem echo "%~1"
rem echo %a1%
rem endlocal

pause
