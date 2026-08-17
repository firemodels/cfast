$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($ScriptDir)) {
    $ScriptDir = "."
}

Set-Location $ScriptDir

$env:CFAST_SIGNING_CERT_SHA1 = "6E0E680BF8D66D576E5330D41C0EF4EC2828BE81"
$env:SIGNTOOL_EXE = "C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"

python .\build_windows_bundle.py --manuals-from-release --upload
exit $LASTEXITCODE

