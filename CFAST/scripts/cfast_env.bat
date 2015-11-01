@echo off
:: ------ cfast version and revision numbers ---------

set cfast_version=6.3.1
set cfast_revision=91df0ac

:: ---------- cfast repository settings ------------

set svn_root=%userprofile%\cfast
set svn_drive=c:
set linux_svn_root=cfast

:: ---------- User/Host names -----------------

set linux_hostname=blaze.nist.gov
set osx_hostname=floga.el.nist.gov

set linux_username=%username%
set svn_logon=%linux_username%@%linux_hostname%

