@echo off
:: ------ cfast version and revision numbers ---------

set cfast_version=6.3.1
set cfast_revision=91df0ac

:: ---------- cfast repository settings ------------

set git_root=%userprofile%\cfast
set git_drive=c:
set linux_git_root=cfast

:: ---------- User/Host names -----------------

set linux_hostname=blaze.nist.gov
set osx_hostname=floga.el.nist.gov

set linux_username=%username%
set git_logon=%linux_username%@%linux_hostname%

