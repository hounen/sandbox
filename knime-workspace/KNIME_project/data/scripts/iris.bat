cd %~dp0

@REM use anaconda
@REM @SET PATH=C:\Users\tateishi\anaconda3\Scripts;%PATH%  
@REM @CALL activate venv01 || ECHO Activating python environment failed  
@REM python %*

@REM use venv
@REM Adapt the folder in the PATH to your system    
@SET PATH=.\venv02\Scripts;%PATH%  
@CALL activate || ECHO Activating python environment failed  
python iris.py
@CALL deactivate
