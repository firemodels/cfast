$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($ScriptDir)) {
    $ScriptDir = "."
}

Set-Location $ScriptDir

$env:CFAST_SIGNING_CERT_SHA1 = "6E0E680BF8D66D576E5330D41C0EF4EC2828BE81"
$env:SIGNTOOL_EXE = "C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"

# Prefer the newest supported interpreter known to the py launcher, and fall
# back to python on PATH.
$Python = "python"
if (Get-Command py -ErrorAction SilentlyContinue) {
    foreach ($Version in "3.14", "3.13", "3.12", "3.11", "3.10", "3.9") {
        $Candidate = cmd /c "py -$Version -c ""import sys; print(sys.executable)"" 2>nul"
        if ($LASTEXITCODE -eq 0 -and $Candidate) {
            $Python = $Candidate
            break
        }
    }
}

$LogDir = Join-Path $ScriptDir "logs"
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null
$Log = Join-Path $LogDir ("windows_bundle_{0}.log" -f (Get-Date -Format "yyyyMMdd_HHmmss"))

Write-Output "*** Python: $Python"
Write-Output "*** Log:    $Log"

# Merge stderr into stdout inside cmd so PowerShell does not report build
# output written to stderr as errors.
$PSDefaultParameterValues["Out-File:Encoding"] = "utf8"
cmd /c """$Python"" .\build_windows_bundle.py --manuals-from-release --upload 2>&1" | Tee-Object -FilePath $Log
exit $LASTEXITCODE
