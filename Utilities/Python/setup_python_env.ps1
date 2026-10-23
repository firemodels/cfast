# Dot-source to configure and activate CFAST's Python environment on Windows:
# . .\setup_python_env.ps1 [--batchmode]
# Existing environments are reused; changed requirements are installed in place.
function Initialize-CfastPythonEnv {
    param([string[]]$Options)
    $ErrorActionPreference = 'Stop'
    foreach ($Option in $Options) {
        if ($Option -notin @('--batchmode', '-BatchMode')) { throw "Unknown argument: $Option" }
    }
    $Repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
    $GitHubDir = Join-Path $Repo '.github'
    $Requirements = Join-Path $GitHubDir 'requirements.txt'
    $Venv = Join-Path $GitHubDir 'cfast_python_env'
    $Interpreter = Join-Path $Venv 'Scripts/python.exe'
    if (-not (Test-Path -LiteralPath $Requirements -PathType Leaf)) { throw "Missing Python requirements: $Requirements" }
    if (-not (Test-Path -LiteralPath $Interpreter -PathType Leaf)) {
        & python -c 'import sys; sys.exit(sys.version_info < (3, 7))'
        if ($LASTEXITCODE -ne 0) { throw 'Python 3.7 or higher is required (packages may require a newer version).' }
        & python -m venv $Venv
        if ($LASTEXITCODE -ne 0) { throw 'Failed to create CFAST Python environment.' }
    }
    $InstalledRequirements = Join-Path $Venv 'cfast_requirements.txt'
    if (-not (Test-Path -LiteralPath $InstalledRequirements) -or
        (Get-FileHash -LiteralPath $Requirements).Hash -ne (Get-FileHash -LiteralPath $InstalledRequirements).Hash) {
        & $Interpreter -m pip install --upgrade pip
        if ($LASTEXITCODE -ne 0) { throw 'Failed to upgrade pip.' }
        Push-Location -LiteralPath $GitHubDir
        try {
            & $Interpreter -m pip install -r requirements.txt
            if ($LASTEXITCODE -ne 0) { throw 'Failed to install CFAST Python requirements.' }
        } finally { Pop-Location }
        Copy-Item -LiteralPath $Requirements -Destination $InstalledRequirements -ErrorAction Stop
    }
    . (Join-Path $Venv 'Scripts/Activate.ps1')
    $env:PYTHONPATH = (Join-Path $Repo '../fds/Utilities/Python') + [IO.Path]::PathSeparator + $env:PYTHONPATH
    & $Interpreter -c 'import fdsplotlib, numpy, pandas, scipy, matplotlib, PySide6'
    if ($LASTEXITCODE -ne 0) { throw 'CFAST Python dependency check failed.' }
    Write-Host "CFAST Python environment ready: $Venv"
}

Initialize-CfastPythonEnv -Options $args
