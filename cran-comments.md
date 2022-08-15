This version fixes the issue with the change to HTML5 in documentation pages in R version 4.2.0 as noted by CRAN.

## Test environments
* local OS X 12.4 install, R 4.2.1
* R Hub builder:
   - Fedora Linux, R-devel, clang, gfortran 
   - Windows Server 2022, R-devel, 64 bit
   - Ubuntu Linux 20.04.1 LTS, R-release, GCC
* win-builder (devel and release):
   - x86_64-w64-mingw32 (64-bit)

## R CMD check results
0 Errors
0 Warnings

and 1 expected NOTE on all platforms.

This note concerns a change of the e-mail address of the maintainer (me):

New maintainer: Fred Hasselman <fred.hasselman@ru.nl>
Old maintainer(s): Fred Hasselman <f.hasselman@bsi.ru.nl>


In addition, there were platform/R version specific NOTES

R Hub builder:
- Fedora Linux, R-devel, clang, gfortran: "Skipping checking HTML validation: no command 'tidy' found"
- Windows Server 2022, R-devel, 64 bit: "Found the following files/directories: 'lastMiKTeXException'"

These were not found on Ubuntu Linux 20.04.1 LTS, R-release, GCC

Win builder x86_64-w64-mingw32 (64-bit):
- R version 4.2.1 (2022-06-23 ucrt): "Non-standard file/directory found at top level:  'pkgdown'"

This was not found on R Under development (unstable) (2022-08-14 r82716 ucrt)


## revdep_check results
OK: 0
BROKEN: 0
Total time: <1 min
