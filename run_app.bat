@echo off
REM Start the Universal Blinder app (it opens in your web browser). Close this window to stop the app.
REM No parenthesised blocks below: a ")" inside an echo would end the block early.
set "RDIR="
for /f "delims=" %%i in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do if not defined RDIR set "RDIR=%ProgramFiles%\R\%%i"
if not defined RDIR goto no_r

echo Starting the Universal Blinder with "%RDIR%\bin\Rscript.exe" ...
echo The app opens in your web browser. Keep this window open while you use it; close it to stop the app.
"%RDIR%\bin\Rscript.exe" -e "if (!requireNamespace('universalblinder', quietly = TRUE)) stop('The universalblinder package is not installed for this R. In R run install.packages() on the package folder with repos = NULL, type = source (see USERGUIDE.md, Install and start).', call. = FALSE); universalblinder::run_app()"
echo.
echo The app has stopped.
pause
exit /b 0

:no_r
echo R was not found in "%ProgramFiles%\R".
echo Install R, or start the app from R with: universalblinder::run_app^(^)
pause
exit /b 1
