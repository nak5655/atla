@echo off
setlocal

REM Resolve repository root based on this script's location.
set "SCRIPT_DIR=%~dp0"
set "SOLUTION_PATH=%SCRIPT_DIR%src\Atla.slnx"
set "BIN_DIR=%USERPROFILE%\.atla\bin"

REM Validate that the solution file exists before publishing.
if not exist "%SOLUTION_PATH%" (
  echo [ERROR] Solution file not found: "%SOLUTION_PATH%"
  exit /b 1
)

REM Create the bin directory if it does not exist.
if not exist "%BIN_DIR%" (
  mkdir "%BIN_DIR%"
)

REM Publish Atla.Console to ~/.atla/bin.
dotnet publish "%SCRIPT_DIR%src\Atla.Console\Atla.Console.fsproj" -c Release -o "%BIN_DIR%"
if errorlevel 1 (
  echo [ERROR] dotnet publish failed for Atla.Console.
  exit /b 1
)
echo [OK] Atla.Console published to "%BIN_DIR%".

REM Publish Atla.LanguageServer to ~/.atla/bin.
dotnet publish "%SCRIPT_DIR%src\Atla.LanguageServer\Atla.LanguageServer.fsproj" -c Release -o "%BIN_DIR%"
if errorlevel 1 (
  echo [ERROR] dotnet publish failed for Atla.LanguageServer.
  exit /b 1
)
echo [OK] Atla.LanguageServer published to "%BIN_DIR%".

REM Add ~/.atla/bin to the user PATH if not already present.
for /f "usebackq tokens=2,*" %%A in (`reg query "HKCU\Environment" /v PATH 2^>nul`) do set "USER_PATH=%%B"
echo %USER_PATH% | findstr /i /c:"%BIN_DIR%" >nul 2>&1
if errorlevel 1 (
  setx PATH "%USER_PATH%;%BIN_DIR%"
  echo [OK] Added "%BIN_DIR%" to user PATH.
) else (
  echo [OK] "%BIN_DIR%" is already in user PATH.
)

REM Run `atla install` in the Std directory.
echo [INFO] Running atla install in "%SCRIPT_DIR%Std"...
pushd "%SCRIPT_DIR%Std"
"%BIN_DIR%\atla.exe" install
if errorlevel 1 (
  popd
  echo [ERROR] atla install failed.
  exit /b 1
)
popd
echo [OK] atla install completed.

exit /b 0
