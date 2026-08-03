@echo off

set RUNDIR=%~dp0
set TARGET_CODE=.\code\main.py
set NETWORK_DRIVE_LETTER=R

rem pushd %RUNDIR% not func from sharefolder
net use %NETWORK_DRIVE_LETTER%: %RUNDIR%
if errorlevel 1 (
    echo "failed to map the network drive."
    pause
    exit /b 1
)
%NETWORK_DRIVE_LETTER%:

echo "preparing to run, please wait about few minites ..."

call venv_activate.bat
echo "python.exe %TARGET_CODE%"
python.exe %TARGET_CODE%
call venv_deactivate.bat

rem popd
cd /d C:\
net use %NETWORK_DRIVE_LETTER%: /delete /y

pause
