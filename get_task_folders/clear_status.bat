@echo off
cd %~dp0

rem �s�t�I�Ȗʂ�����̂Ŏ��s�͐T�d��

rem �J�����g�t�H���_�ȉ��̃T�u�t�H���_�[���܂߂Ă��ׂẴt�@�C���̃t�@�C������ύX���܂�
rem �� for /R %target_dir% %%i in (*.*) do ren %%i %%~nxi.txt

for /R %target_dir% %%i in (*.progress) do ren %%i %%~nxi.clear
rem for /R %target_dir% %%i in (*.txt.clear) do ren %%i %%~nxi.txt
rem for /R %target_dir% %%i in (*.png.clear) do ren %%i %%~nxi.progress.txt
rem for /R %target_dir% %%i in (*.jpg.clear) do ren %%i %%~nxi.jpg

rem �t�@�C�����̌��ɂ����u.txt�v�����ׂĊO���܂�
rem �� for /R %target_dir% %%i in (*.txt) do ren %%i %%~ni
rem for /R %target_dir% %%i in (*.jpg) do ren %%i %%~ni

pause
