@echo off

set RUNDIR=%~dp0
set NETWORK_DRIVE_LETTER=R

set PYHOME=___bin\python\python-3.13.7-embed-amd64
set VENV_NAME=.venv
set PROJECT_HOME=%NETWORK_DRIVE_LETTER%:\\extractNihonNoKenkyu

rem pushd %RUNDIR% not func from sharefolder
net use %NETWORK_DRIVE_LETTER%: %RUNDIR%..
if errorlevel 1 (
    echo "failed to map the network drive."
    pause
    exit /b 1
)
%NETWORK_DRIVE_LETTER%:

echo "preparing to run, please wait about few minites ..."

cd %PROJECT_HOME%

if exist %VENV_NAME% (
	rmdir /s /q %VENV_NAME%
)

..\%PYHOME%\python.exe -m virtualenv %VENV_NAME%

call venv_activate.bat
echo "pip.exe install --no-index --find-links .\venv_packages -r venv_requirements.txt"
pip.exe install --no-index --find-links .\venv_packages -r venv_requirements.txt
call venv_deactivate.bat

cd %PROJECT_HOME%..

rem popd
cd /d C:\
net use %NETWORK_DRIVE_LETTER%: /delete /y

echo "venv recreated."

pause