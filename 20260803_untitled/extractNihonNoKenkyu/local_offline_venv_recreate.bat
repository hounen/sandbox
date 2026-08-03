@echo off

set RUNDIR=%~dp0
set PYHOME=..\___bin\python\python-3.13.7-embed-amd64
set VENV_NAME=.venv

if exist %VENV_NAME% (
	rmdir /s /q %VENV_NAME%
)

%PYHOME%\python.exe -m virtualenv %VENV_NAME%

call venv_activate.bat
pip.exe install --no-index --find-links .\venv_packages -r venv_requirements.txt
call venv_deactivate.bat

cd %RUNDIR%

echo "venv recreated."

pause