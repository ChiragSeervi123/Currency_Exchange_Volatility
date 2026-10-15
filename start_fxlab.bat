@echo off
REM ===========================================================================
REM  start_fxlab.bat - start the FXLab R backend (API + web interface).
REM  Double-click this file, or run it from a terminal in the project folder.
REM  Then open http://127.0.0.1:8000/ in your browser. Close the window or
REM  press Ctrl+C to stop the server.
REM ===========================================================================
setlocal
cd /d "%~dp0"

set "RSCRIPT="
where Rscript >nul 2>nul && set "RSCRIPT=Rscript"
if not defined RSCRIPT (
  for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" set "RSCRIPT=%%D\bin\Rscript.exe"
  )
)
if not defined RSCRIPT (
  echo Could not find Rscript.exe. Install R from https://cran.r-project.org/ or add R's bin folder to PATH.
  pause
  exit /b 1
)

echo Using "%RSCRIPT%"
echo Opening http://127.0.0.1:8000/ once the server is ready...
start "" /b cmd /c "timeout /t 4 /nobreak >nul & start http://127.0.0.1:8000/"
"%RSCRIPT%" backend\main.R
if errorlevel 1 pause
