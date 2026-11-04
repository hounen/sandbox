@echo off

set RUNDIR=%~dp0
set TARGET_CODE=.\code\main.py

set RUNDIR_FILE=.\local_run_path.txt

if not exist %RUNDIR_FILE% (
    echo %RUNDIR% > %RUNDIR_FILE%
)

set /P RUNDIR_LAST=<.\local_run_path.txt
if not "%RUNDIR% "=="%RUNDIR_LAST%" (
    echo "project directory path changed."
    echo "recreating venv ..."
    call local_online_venv_recreate.bat
    echo %RUNDIR% > %RUNDIR_FILE%
)

call venv_activate.bat
python.exe %TARGET_CODE%
call venv_deactivate.bat

pause
