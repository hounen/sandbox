@echo off
set RUNDIR=%~dp0

call venv_activate.bat
pip.exe install %1
pip.exe freeze > venv_requirements.txt
pip.exe download -r venv_requirements.txt -d venv_packages_tmp

if %ERRORLEVEL% equ 0 (
    if exist .\venv_packages_tmp (
        rmdir /s /q venv_packages
    )
    rename .\venv_packages_tmp venv_packages
)

call venv_deactivate.bat

cd %RUNDIR%

echo "venv pip installed" 

pause
