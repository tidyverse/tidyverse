## Test environments
* local OS X install, R-release
* ubuntu 14.04 (on travis-ci), R-oldrel, R-release, R-devel
* win-builder (devel and release)

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
