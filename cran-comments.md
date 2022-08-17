This version concerns a new build under R Version 4.2.1 in order to address the the change to HTML5 in documentation pages as of R version 4.2.0. 

## Test environments
* local:
   - OS X 12.4, R version 4.2.1 (2022-06-23)
* R-hub builder:
   - Fedora Linux, R Under development (unstable) (2022-08-14 r82716), clang, gfortran 
   - Windows Server 2022, R Under development (unstable) (2022-07-15 r82598 ucrt) 
   - Ubuntu Linux 20.04.1 LTS, R version 4.2.1 (2022-06-23), GCC
* win-builder :
   - x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2022-08-14 r82716 ucrt)
   - x86_64-w64-mingw32 (64-bit), R version 4.2.1 (2022-06-23 ucrt)
* Appveyor:
   - x86_64-w64-mingw32/x64 (64-bit), R version 4.2.1 Patched (2022-08-14 r82716 ucrt)

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 1-2 notes ✖

1 note on all platforms is expected and concerns a change of the e-mail address of the maintainer (me):

New maintainer: Fred Hasselman <fred.hasselman@ru.nl>
Old maintainer(s): Fred Hasselman <f.hasselman@bsi.ru.nl>

In addition, there were some platform/R version specific notes:

R-hub builder, R development version:
- Fedora Linux, R-devel, clang, gfortran: "Skipping checking HTML validation: no command 'tidy' found"
- Windows Server 2022, R-devel, 64 bit: "Found the following files/directories: 'lastMiKTeXException'"

Win-builder x86_64-w64-mingw32 (64-bit), R release version:
- R version 4.2.1 (2022-06-23 ucrt): "Non-standard file/directory found at top level:  'pkgdown'"

The other test environments yielded no additional notes.

## Reverse dependencies
revdep_check results:

OK: 0
BROKEN: 0
Total time: <1 min
