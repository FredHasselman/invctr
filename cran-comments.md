This version fixes an issue with Copyright noted by CRAN:
* During package development, I used a function written by Martin Maechler, with Copyright (C) 2010-2012 The R Core Team. It was still present in the source file, without mentioning the Copyright holder as such in the package DESCRIPTION. Thank you for noticing this error!
* I Removed the function from the source file, there is no need to include it in the package.


## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04.5 LTS (on travis-ci), R 3.5.2
* win-builder (devel and release)
* appveyor: Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows Server 2012 R2 x64 (build 9600)

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## revdep_check results
No reverse dependencies (yet)


