@echo off
REM ===========================================================================
REM  splash.bat - start Chart Reader and open the 3D splash page.
REM  It uses start_fxlab.bat, so it starts the same R backend (API + web
REM  interface); the splash page's "Enter Chart Reader" button opens the app.
REM  Splash page: http://127.0.0.1:8000/app/splash.html
REM ===========================================================================
call "%~dp0start_fxlab.bat" app/splash.html
