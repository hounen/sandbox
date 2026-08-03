@echo off

set RUNDIR=%~dp0
set PYHOME=..\___bin\python\python-3.13.7-embed-amd64
set VENV_NAME=.venv

if exist %VENV_NAME% (
	rmdir /s /q %VENV_NAME%
)

%PYHOME%\python.exe -m virtualenv %VENV_NAME%

call venv_activate.bat
pip.exe install -r venv_requirements.txt
pip.exe download -r venv_requirements.txt -d venv_packages_tmp
call venv_deactivate.bat

if %ERRORLEVEL% equ 0 (
    if exist .\venv_packages_tmp (
        rmdir /s /q venv_packages
    )
    rename .\venv_packages_tmp venv_packages
)

cd %RUNDIR%

echo "venv recreated."