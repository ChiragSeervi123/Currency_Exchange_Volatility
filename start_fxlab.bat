@echo off
REM ===========================================================================
REM  start_fxlab.bat - start the Chart Reader R backend (API + web interface).
REM  Double-click this file, or run it from a terminal in the project folder.
REM  Then open http://127.0.0.1:8000/ in your browser. Close the window or
REM  press Ctrl+C to stop the server.
REM
REM  Optional argument: the page to open, relative to the server, for example
REM    start_fxlab.bat app/splash.html     (this is what splash.bat does)
REM  If the backend is already running, the page is just opened in the browser.
REM ===========================================================================
setlocal
cd /d "%~dp0"

set "PAGE=%~1"
set "URL=http://127.0.0.1:8000/%PAGE%"

curl.exe -s -f -o nul --max-time 2 http://127.0.0.1:8000/health >nul 2>nul
if not errorlevel 1 (
  echo The Chart Reader backend is already running. Opening %URL%
  start "" "%URL%"
  exit /b 0
)

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
echo Opening %URL% once the server is ready...
start "" /b cmd /c "timeout /t 4 /nobreak >nul & start %URL%"
"%RSCRIPT%" backend\main.R
if errorlevel 1 pause
