#!/usr/bin/env pwsh
param (
    [Parameter(Mandatory=$true, Position=0)]
    [string]$Year,

    [Parameter(Position=1)]
    [string]$Day,

    [Alias("e")]
    [switch]$Example
)

$Root = $PSScriptRoot

function Invoke-Script {
    param($Y, $D)
    
    $DayPad = $D.PadLeft(2, '0')
    $Dir = Join-Path $Root "aoc" "Y$Y" "D$DayPad"
    
    # Check Python
    if (Test-Path (Join-Path $Dir "solution.py")) {
        Write-Host "--- Day $D ($Y) [Python] ---" -ForegroundColor Green
        # Let run.py handle the output, we just measure time
        $cmdArgs = @("aoc\runner.py", $Y, $D)
        if ($Example) { $cmdArgs += "-e" }
        
        $time = Measure-Command {
            $env:PYTHONPATH = Join-Path $Root "common"
            & python $cmdArgs | Out-Host
        }
        
        Write-Host "Execution Time: $($time.TotalMilliseconds.ToString("F2")) ms`n" -ForegroundColor DarkGray
        return $true
    }
    
    # Check Haskell
    $HaskellDir = Join-Path $Dir "D$DayPad"
    $HaskellFile = Join-Path $HaskellDir "Solution.hs"
    
    if (Test-Path $HaskellFile) {
        Write-Host "--- Day $D ($Y) [Haskell] ---" -ForegroundColor Green
        
        $time = Measure-Command {
            $cmdArgs = @("run", "aoc", "--", $Y, $D)
            if ($Example) { $cmdArgs += "-e" }
            
            # Run via cabal
            & cabal $cmdArgs | Out-Host
        }
        Write-Host "Total Execution Time: $($time.TotalMilliseconds.ToString("F2")) ms`n" -ForegroundColor DarkGray
        return $true
    }

    return $False
}

if (-not [string]::IsNullOrWhiteSpace($Day)) {
    if (-not (Invoke-Script $Year $Day)) {
        Write-Error "No solution found for $Year Day $Day"
    }
} else {
    # Run all days 1-25
    for ($i = 1; $i -le 25; $i++) {
        Invoke-Script $Year $i.ToString() | Out-Null
    }
}
