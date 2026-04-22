@echo off
:: ------ cfast version and revision numbers ---------

set cfast_version=6.3.1
set cfast_revision=91df0ac

:: ---------- cfast repository settings ------------

set git_root=%userprofile%\cfast
set git_drive=c:
set linux_git_root=cfast
set osx_git_root=cfast

:: ---------- User/Host names -----------------

:: Linux user and host name

set linux_hostname=blaze.nist.gov
set linux_username=%username%
set linux_logon=%linux_username%@%linux_hostname%

:: OSX user and host name

set osx_hostname=192.168.1.5
set osx_username=%username%
set osx_logon=%osx_username%@%osx_hostname%

