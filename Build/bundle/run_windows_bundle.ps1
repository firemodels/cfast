$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($ScriptDir)) {
    $ScriptDir = "."
}

Set-Location $ScriptDir

python .\build_windows_bundle.py --manuals-from-release --upload
exit $LASTEXITCODE

