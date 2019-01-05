## Test environments

* local: darwin15.6.0-3.5.1
* travis: 3.1, 3.2, 3.3, oldrel, release, devel
* r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* win-builder: windows-x86_64-devel

## R CMD check results

0 errors | 0 warnings | 1 note

*   Days since last update: 6
 
    This a patch release because I discovered I had inadvertently depended on 
    RStudio >1.1 (via the rstudioapi package). My automatic tests did not 
    discover this because they're always run from the console. To prevent
    this happening in the future, I've added a step to the release process
    to always test in older versions of RStudio.

## Revdep check results

Since this only affects RStudio, I did not retest revdeps.
