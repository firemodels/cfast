# Run the standalone CFAST pipeline on existing Windows checkouts.
[CmdletBinding()]
param(
    [ValidateSet('intel', 'gnu')][string]$Compiler = 'intel',
    [string]$Python = '',
    [string]$StateDirectory = $(if ($env:CFAST_CI_STATE_DIR) { $env:CFAST_CI_STATE_DIR } else { Join-Path $HOME '.cfastbot' }),
    [switch]$Help
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
if ($Help) {
    @'
Usage: .\run_cfastbot.ps1 [-Compiler intel|gnu] [-Python path] [-StateDirectory path]
Run clean debug/release builds, local cases, CEditQt checks, figures, and manuals.
Use a compiler-enabled Windows terminal with make, Python, pdflatex, and biber.
Smokeview uses Intel C/C++; -Compiler selects the CFAST Fortran compiler.
Existing sibling cfast, fds, exp, and smv checkouts are required.
Results are saved locally. This launcher does not publish or send email.
'@
    exit 0
}

function Require-File([string]$Path) {
    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { throw "Missing file: $Path" }
}

function Invoke-Stage {
    param([string]$Name, [string]$Directory, [string]$Program, [string[]]$Arguments = @())
    Write-Host "Running $Name"
    $stdout = Join-Path $script:Output "$Name.log"
    $stderr = Join-Path $script:Output "$Name.err"
    # Start-Process takes a single Windows command line. These arguments are
    # filenames/options, never shell expressions; quote spaces explicitly.
    $quoted = @($Arguments | ForEach-Object {
        if ($_ -match '"') { throw 'Embedded quotes are not supported in command arguments.' }
        '"' + $_ + '"'
    })
    $parameters = @{
        FilePath = $Program; WorkingDirectory = $Directory
        RedirectStandardOutput = $stdout; RedirectStandardError = $stderr
        NoNewWindow = $true; Wait = $true; PassThru = $true
    }
    if ($quoted.Count -gt 0) { $parameters.ArgumentList = $quoted -join ' ' }
    $process = Start-Process @parameters
    if ($process.ExitCode -ne 0) {
        Get-Content -LiteralPath $stdout, $stderr -Tail 30 | Write-Host
        throw "$Name failed (exit $($process.ExitCode)); see $stdout and $stderr"
    }
}

function Invoke-Build([string]$Name, [string]$Directory, [string]$ScriptName, [string]$Options) {
    Require-File (Join-Path $Directory $ScriptName)
    # The script name and options here are fixed by this launcher.
    Invoke-Stage $Name $Directory $env:ComSpec @('/d', '/c', "$ScriptName $Options")
    $diagnostics = Select-String -Path (Join-Path $script:Output "$Name.log"), (Join-Path $script:Output "$Name.err") -Pattern '(?i)\b(error|warning)\s*(#\d+|[A-Z]+\d+)?\s*:'
    if ($diagnostics) { throw "$Name reported compiler diagnostics; see the stage logs." }
}

function Read-Cases([string]$Suite) {
    $suiteDir = Join-Path $script:Repo $Suite
    $list = Join-Path $suiteDir 'scripts/CFAST_Cases.sh'
    Require-File $list
    foreach ($line in Get-Content -LiteralPath $list) {
        if ($line -match '^\s*(#.*)?$') { continue }
        if ($line -notmatch '^\s*\$RUNCFAST\s+-d\s+(?<Directory>\S+)\s+(?<Input>[^\s/\\]+\.in)\s*(?:#.*)?$') {
            throw "Unsupported case entry in ${list}: $line"
        }
        $directory = [IO.Path]::GetFullPath((Join-Path $suiteDir $Matches.Directory))
        $inputName = $Matches.Input
        Require-File (Join-Path $directory $inputName)
        [PSCustomObject]@{ Directory = $directory; InputName = $inputName; Stem = [IO.Path]::GetFileNameWithoutExtension($inputName) }
    }
}

function Invoke-Cases([string]$Suite, [string]$Mode, [string]$Executable) {
    $entries = @(Read-Cases $Suite)
    if ($entries.Count -eq 0) { throw "Empty suite: $Suite" }
    $index = 0
    foreach ($entry in $entries) {
        $index++
        Write-Host "$Suite ${Mode}: $index/$($entries.Count) $($entry.InputName)"
        $suffix = ''
        if ($Mode -eq 'ui') { $suffix = '.ui' }
        $status = Join-Path $entry.Directory ($entry.Stem + $suffix + '.exit')
        if (Test-Path -LiteralPath $status) { Remove-Item -LiteralPath $status }
        if ($Mode -ne 'ui') {
            $stop = Join-Path $entry.Directory ($entry.Stem + '.stop')
            if ($Mode -eq 'debug') { [IO.File]::WriteAllText($stop, "2`n") }
            elseif (Test-Path -LiteralPath $stop) { Remove-Item -LiteralPath $stop }
        }
        $name = "${Suite}_${Mode}_${index}"
        $result = 1
        try {
            if ($Mode -eq 'ui') {
                $options = @((Join-Path $script:Repo 'Source/CeditQt/run_verification_ui_tests.py'), '--mode', 'rewrite', '--repo-root', $script:Repo, '--input', (Join-Path $entry.Directory $entry.InputName), '--skip-helpers')
                if ($index -gt 1) { $options += '--skip-editor-checks' }
                Invoke-Stage $name $entry.Directory $script:PythonExe $options
            } else {
                Invoke-Stage $name $entry.Directory $Executable @($entry.InputName)
            }
            $result = 0
        } finally {
            [IO.File]::WriteAllText($status, "$result`n")
            # Preserve native CFAST .log files and keep stdout separate.
            foreach ($pair in @(@('log', 'stdout'), @('err', 'err'))) {
                $source = Join-Path $script:Output "$name.$($pair[0])"
                if (Test-Path -LiteralPath $source) {
                    Copy-Item -LiteralPath $source -Destination (Join-Path $entry.Directory ($entry.Stem + $suffix + '.' + $pair[1]))
                }
            }
        }
    }
}

$exitCode = 1
$script:Output = $null
try {
    if ($env:OS -ne 'Windows_NT') { throw 'This launcher requires Windows.' }
    $script:Repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
    $root = Split-Path $script:Repo -Parent
    $state = [IO.Path]::GetFullPath($StateDirectory)
    foreach ($name in @('cfast', 'fds', 'exp', 'smv')) {
        $checkout = Join-Path $root $name
        if (-not (Test-Path -LiteralPath $checkout -PathType Container)) { throw "Missing checkout: $checkout" }
        if ($state.TrimEnd('\', '/') -eq $checkout -or $state.StartsWith($checkout + '\', [StringComparison]::OrdinalIgnoreCase)) {
            throw 'StateDirectory must be outside repository checkouts.'
        }
    }
    $script:Output = Join-Path $state ('runs/' + (Get-Date -Format 'yyyyMMdd-HHmmss') + '-' + $PID)
    New-Item -ItemType Directory -Path $script:Output -Force | Out-Null
    [IO.File]::WriteAllText((Join-Path $state 'latest_run'), "$script:Output`n")
    Write-Host "CFASTbot logs: $script:Output"
    foreach ($program in @('make', 'pdflatex', 'biber', 'icx')) {
        if (-not (Get-Command $program -CommandType Application -ErrorAction SilentlyContinue)) { throw "Missing prerequisite: $program" }
    }
    $fortran = if ($Compiler -eq 'intel') { 'ifx' } else { 'gfortran' }
    if (-not (Get-Command $fortran -CommandType Application -ErrorAction SilentlyContinue)) { throw "Missing prerequisite: $fortran" }
    if ($env:CFAST_CI_PYTHON) { $Python = $env:CFAST_CI_PYTHON }
    if (-not $Python) {
        $Python = Join-Path $root 'fds/.github/fds_python_env/Scripts/python.exe'
        if (-not (Test-Path -LiteralPath $Python -PathType Leaf)) {
            . (Join-Path $script:Repo 'Utilities/Python/setup_python_env.ps1') --batchmode
            $Python = Join-Path $script:Repo '.github/cfast_python_env/Scripts/python.exe'
        }
    }
    $script:PythonExe = (Get-Command $Python -CommandType Application -ErrorAction Stop | Select-Object -First 1).Source
    Write-Host "CFAST Python: $script:PythonExe"
    $env:PYTHONPATH = (Join-Path $root 'fds/Utilities/Python') + [IO.Path]::PathSeparator + $env:PYTHONPATH
    $env:QT_QPA_PLATFORM = 'offscreen'
    $env:MPLBACKEND = 'Agg'
    $env:MPLCONFIGDIR = Join-Path $script:Repo '.github/cfast_matplotlib'
    New-Item -ItemType Directory -Path $env:MPLCONFIGDIR -Force | Out-Null
    Invoke-Stage 'python_check' $script:Repo $script:PythonExe @('-c', 'import fdsplotlib, numpy, pandas, scipy, matplotlib, PySide6')

    foreach ($suffix in @('_db', '')) {
        $buildDir = Join-Path $script:Repo "Build/CFAST/${Compiler}_win${suffix}"
        Invoke-Build "build_cfast$suffix" $buildDir 'make_cfast.bat' '--clean-cfast'
        Require-File (Join-Path $buildDir "cfast8_win${suffix}.exe")
    }
    $smv = Join-Path $root 'smv'
    Invoke-Build 'build_smv_libs' (Join-Path $smv 'Build/LIBS/intel_win') 'make_LIBS.bat' ''
    $smvBuild = Join-Path $smv 'Build/smokeview/intel_win'
    Invoke-Build 'build_smv_debug' $smvBuild 'make_smokeview_db.bat' '-bot'
    Require-File (Join-Path $smvBuild 'smokeview_win_db.exe')
    Invoke-Build 'build_smv_release' $smvBuild 'make_smokeview.bat' '-bot'
    $smvExe = Join-Path $smvBuild 'smokeview_win.exe'
    Require-File $smvExe
    foreach ($mode in @('debug', 'release', 'ui')) {
        $suffix = if ($mode -eq 'debug') { '_db' } else { '' }
        $exe = Join-Path $script:Repo "Build/CFAST/${Compiler}_win${suffix}/cfast8_win${suffix}.exe"
        foreach ($suite in @('Verification', 'Validation')) { Invoke-Cases $suite $mode $exe }
    }

    $figureDir = Join-Path $script:Repo 'Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES'
    New-Item -ItemType Directory -Path $figureDir -Force | Out-Null
    foreach ($line in Get-Content -LiteralPath (Join-Path $script:Repo 'Validation/scripts/CFAST_Pictures.sh')) {
        if ($line -match '^\s*(#.*)?$') { continue }
        if ($line -notmatch '^\s*\$RUNSMV\s+-d\s+(?<Directory>\S+)\s+(?<Input>\S+\.in)\s*$') { throw "Unsupported picture entry: $line" }
        $directory = Join-Path $script:Repo ('Validation/' + $Matches.Directory)
        $stem = [IO.Path]::GetFileNameWithoutExtension($Matches.Input)
        Require-File (Join-Path $directory "$stem.smv")
        Require-File (Join-Path $directory "$stem.ssf")
        Invoke-Stage "picture_$stem" $directory $smvExe @('-bindir', (Join-Path $smv 'for_bundle'), '-scriptrenderdir', $figureDir, '-runscript', $stem)
    }
    $pythonDir = Join-Path $script:Repo 'Utilities/Python'
    foreach ($suite in @('verification', 'validation')) {
        Invoke-Stage "plot_$suite" $pythonDir $script:PythonExe @("CFAST_${suite}_script.py")
        if (Select-String -Path (Join-Path $script:Output "plot_$suite.log"), (Join-Path $script:Output "plot_$suite.err") -Pattern 'Error|Traceback') {
            throw "$suite plotting reported errors; see the stage logs."
        }
    }
    $stats = Join-Path $figureDir 'Scatterplots/validation_scatterplot_output.csv'
    Require-File $stats
    Copy-Item -LiteralPath $stats -Destination $script:Output
    $baseline = Join-Path $figureDir 'Scatterplots/validation_scatterplot_output_baseline.csv'
    Require-File $baseline
    $differences = Compare-Object (Get-Content -LiteralPath $baseline) (Get-Content -LiteralPath $stats)
    if ($differences) { $differences | Out-File -LiteralPath (Join-Path $script:Output 'statistics_diff.txt') -Encoding utf8 }
    $guideOutput = Join-Path $script:Output 'guides'
    New-Item -ItemType Directory -Path $guideOutput -Force | Out-Null
    foreach ($guide in @('CFAST_Tech_Ref', 'CFAST_Users_Guide', 'CFAST_Validation_Guide', 'CFAST_Configuration_Guide')) {
        $directory = Join-Path $script:Repo "Manuals/$guide"
        $env:TEXINPUTS = '.;../LaTeX_Style_Files;'
        $started = [DateTime]::UtcNow.AddSeconds(-2)
        Invoke-Stage "guide_$guide" $directory $env:ComSpec @('/d', '/c', 'make_guide.bat')
        Require-File (Join-Path $directory "$guide.pdf")
        Require-File (Join-Path $directory "$guide.log")
        Require-File (Join-Path $directory "$guide.blg")
        foreach ($extension in @('pdf', 'log', 'blg')) {
            if ((Get-Item -LiteralPath (Join-Path $directory "$guide.$extension")).LastWriteTimeUtc -lt $started) { throw "$guide did not regenerate its .$extension file." }
        }
        if (Select-String -Path (Join-Path $directory "$guide.log") -Pattern '^!|Fatal error|undefined|multiply.defined') { throw "$guide reported LaTeX errors or unresolved references." }
        if (Select-String -Path (Join-Path $directory "$guide.blg") -Pattern '\bERROR\b') { throw "$guide reported bibliography errors." }
        Copy-Item -LiteralPath (Join-Path $directory "$guide.pdf") -Destination $guideOutput
    }
    $exitCode = 0
    Write-Host "CFASTbot complete; logs and guides: $script:Output"
} catch {
    [Console]::Error.WriteLine($_.Exception.Message)
    if ($script:Output) { $_.Exception.Message | Out-File -LiteralPath (Join-Path $script:Output 'errors') -Encoding utf8 }
} finally {
    if ($script:Output) { [IO.File]::WriteAllText((Join-Path $script:Output 'exit_code'), "$exitCode`n") }
}
exit $exitCode
