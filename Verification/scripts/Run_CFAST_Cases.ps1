# Run the cases listed in CFAST_Cases.sh directly, without Bash or the CI runner.
param(
    [Alias('e')][string]$Executable,
    [Alias('I')][ValidateSet('intel', 'gnu')][string]$Compiler = 'intel',
    [Alias('d')][switch]$DebugBuild,
    [string]$Case,
    [Alias('v')][switch]$Preview,
    [Alias('h')][switch]$Help
)

$ErrorActionPreference = 'Stop'
if ($Help) {
    @'
Usage: .\Run_CFAST_Cases.ps1 [options]
  -e path       CFAST executable path or name on PATH
  -I intel|gnu  Compiler for the standard Windows build (default: intel)
  -d            Select the standard debug build
  -Case name    Run one input filename or stem
  -v            Preview cases without running or changing outputs
  -h            Show this help
Cases run locally, in list order, and stop on the first failure.
'@
    exit 0
}

$exitCode = 0
try {
    if ($args.Count -gt 0) { throw "Unknown arguments: $args. Use -h for supported options." }
    $verificationDir = Split-Path $PSScriptRoot -Parent
    $repo = Split-Path $verificationDir -Parent
    $caseList = Join-Path $PSScriptRoot 'CFAST_Cases.sh'
    if (-not $Executable) {
        $debugSuffix = ''
        if ($DebugBuild) { $debugSuffix = '_db' }
        $Executable = Join-Path $repo "Build/CFAST/${Compiler}_win${debugSuffix}/cfast8_win${debugSuffix}.exe"
    }
    if (Test-Path -LiteralPath $Executable -PathType Leaf) {
        $resolvedExe = (Resolve-Path -LiteralPath $Executable).ProviderPath
    } else {
        $command = Get-Command -Name $Executable -CommandType Application -ErrorAction SilentlyContinue |
            Select-Object -First 1
        if (-not $command) {
            throw "Executable not found: $Executable`nUse -e 'C:\path\to\cfast.exe' to select your executable, or -I gnu for a standard GNU build."
        }
        $resolvedExe = $command.Source
    }

    # Read the declarative case entries; never execute the shell file.
    $lineNumber = 0
    $cases = @(foreach ($line in Get-Content -LiteralPath $caseList) {
        $lineNumber++
        if ($line -match '^\s*(#.*)?$') { continue }
        if ($line -notmatch '^\s*\$RUNCFAST\s+-d\s+(?<Directory>\S+)\s+(?<Input>[^\s/\\]+\.in)\s*(?:#.*)?$') {
            throw "Unsupported case-list entry at ${caseList}:${lineNumber}: $line"
        }
        $inputName = $Matches.Input
        $directory = Join-Path $verificationDir $Matches.Directory
        $stem = [IO.Path]::GetFileNameWithoutExtension($inputName)
        if ($Case -and $inputName -ne $Case -and $stem -ne $Case) { continue }
        $inputPath = Join-Path $directory $inputName
        if (-not (Test-Path -LiteralPath $inputPath -PathType Leaf)) {
            throw "Input not found: $inputPath"
        }
        [PSCustomObject]@{ Directory = $directory; InputName = $inputName; InputPath = $inputPath; Stem = $stem }
    })
    if ($cases.Count -eq 0) { throw "No matching verification cases: $Case" }

    Write-Host "CFAST executable: $resolvedExe"
    foreach ($entry in $cases) {
        if ($Preview) {
            Write-Host "Would run: $($entry.InputPath)"
            continue
        }
        Write-Host "Running locally: $($entry.InputPath)"
        # CFAST opens its own .log file; keep redirected stdout separate on Windows.
        $log = Join-Path $entry.Directory ($entry.Stem + '.stdout')
        $err = Join-Path $entry.Directory ($entry.Stem + '.err')
        $status = Join-Path $entry.Directory ($entry.Stem + '.exit')
        $stop = Join-Path $entry.Directory ($entry.Stem + '.stop')
        foreach ($staleFile in @($status, $stop)) {
            if (Test-Path -LiteralPath $staleFile) { Remove-Item -LiteralPath $staleFile }
        }
        try {
            # Start-Process keeps native stderr out of PowerShell's error stream.
            $process = Start-Process -FilePath $resolvedExe -ArgumentList ('"' + $entry.InputName + '"') `
                -WorkingDirectory $entry.Directory -RedirectStandardOutput $log -RedirectStandardError $err `
                -NoNewWindow -Wait -PassThru
            $exitCode = $process.ExitCode
        } catch {
            [IO.File]::WriteAllText($status, "1`n")
            throw
        }
        [IO.File]::WriteAllText($status, "$exitCode`n")
        if ($exitCode -ne 0) {
            [Console]::Error.WriteLine("Run failed (exit ${exitCode}): $($entry.InputPath); see $log and $err")
            break
        }
    }
} catch {
    [Console]::Error.WriteLine("Run aborted: $($_.Exception.Message)")
    $exitCode = 1
}
exit $exitCode
