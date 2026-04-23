@echo off
setlocal

REM Resolve repository root based on this script's location.
set "SCRIPT_DIR=%~dp0"
set "SOLUTION_PATH=%SCRIPT_DIR%src\Atla.slnx"

REM Validate that the solution file exists before publishing.
if not exist "%SOLUTION_PATH%" (
  echo [ERROR] Solution file not found: "%SOLUTION_PATH%"
  exit /b 1
)

REM Publish all projects in the solution using each project's FolderProfile.pubxml.
dotnet publish "%SOLUTION_PATH%" -c Release -p:PublishProfile=FolderProfile
if errorlevel 1 (
  echo [ERROR] dotnet publish failed.
  exit /b 1
)

echo [OK] Publish completed for "%SOLUTION_PATH%" with PublishProfile=FolderProfile.
exit /b 0
