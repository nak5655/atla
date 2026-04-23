@echo off
setlocal

REM Resolve repository root based on this script's location.
set "SCRIPT_DIR=%~dp0"
set "SOLUTION_PATH=%SCRIPT_DIR%src\Atla.slnx"
set "PROFILE_PATH=%SCRIPT_DIR%folderprofile.pubxml"

REM Validate that the solution file exists before publishing.
if not exist "%SOLUTION_PATH%" (
  echo [ERROR] Solution file not found: "%SOLUTION_PATH%"
  exit /b 1
)

REM Validate that the publish profile file exists.
if not exist "%PROFILE_PATH%" (
  echo [ERROR] Publish profile file not found: "%PROFILE_PATH%"
  exit /b 1
)

REM Read PublishDir from folderprofile.pubxml and resolve to an absolute path.
set "PUBLISH_DIR="
for /f "usebackq delims=" %%I in (`powershell -NoProfile -ExecutionPolicy Bypass -Command "$profilePath = $env:PROFILE_PATH; [xml]$xml = Get-Content -LiteralPath $profilePath; $publishDir = ($xml.Project.PropertyGroup.PublishDir | Select-Object -First 1); if ([string]::IsNullOrWhiteSpace($publishDir)) { exit 2 }; if (-not [System.IO.Path]::IsPathRooted($publishDir)) { $publishDir = Join-Path (Split-Path -Parent $profilePath) $publishDir }; $publishDir = [System.IO.Path]::GetFullPath($publishDir); if (-not $publishDir.EndsWith([System.IO.Path]::DirectorySeparatorChar) -and -not $publishDir.EndsWith([System.IO.Path]::AltDirectorySeparatorChar)) { $publishDir = $publishDir + [System.IO.Path]::DirectorySeparatorChar }; Write-Output $publishDir"`) do set "PUBLISH_DIR=%%I"

if not defined PUBLISH_DIR (
  echo [ERROR] Failed to read PublishDir from "%PROFILE_PATH%".
  exit /b 1
)

REM Publish all publishable projects in Release configuration to PublishDir.
dotnet publish "%SOLUTION_PATH%" -c Release -p:PublishDir="%PUBLISH_DIR%"
if errorlevel 1 (
  echo [ERROR] dotnet publish failed.
  exit /b 1
)

echo [OK] Publish completed for "%SOLUTION_PATH%".
echo [OK] Published outputs are under "%PUBLISH_DIR%".
exit /b 0
