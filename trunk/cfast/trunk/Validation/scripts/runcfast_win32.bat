@echo off
set dir=%1
set infile=%2

set fulldir=%BASEDIR%/%dir%

set in=%infile%
set out=%infile%.err
set stopfile=%infile%.stop

Rem test existence of %CFAST%

Rem test existence of %fulldir%

Rem test existence of CFAST input file %fulldir%/%in%

Rem erase %fulldir%\%stopfile%

cd %fulldir%
echo **********************
echo %in% started
%CFAST% %in%  /V
echo %in% completed
echo **********************
