@echo off
cd %~dp0

rem 不可逆的な面があるので実行は慎重に

rem カレントフォルダ以下のサブフォルダーも含めてすべてのファイルのファイル名を変更します
rem 例 for /R %target_dir% %%i in (*.*) do ren %%i %%~nxi.txt

for /R %target_dir% %%i in (*.progress) do ren %%i %%~nxi.clear
rem for /R %target_dir% %%i in (*.txt.clear) do ren %%i %%~nxi.txt
rem for /R %target_dir% %%i in (*.png.clear) do ren %%i %%~nxi.progress.txt
rem for /R %target_dir% %%i in (*.jpg.clear) do ren %%i %%~nxi.jpg

rem ファイル名の後ろにつけた「.txt」をすべて外します
rem 例 for /R %target_dir% %%i in (*.txt) do ren %%i %%~ni
rem for /R %target_dir% %%i in (*.jpg) do ren %%i %%~ni

pause
