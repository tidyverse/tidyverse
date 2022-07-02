## revdepcheck results

We checked 217 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 5 new problems
 * We failed to check 2 packages

I believe the failures have to be false positives, because there were no significant changes in the tidyverse package.

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* covid19br
  checking re-building of vignette outputs ... WARNING

* dumbbell
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* LexFindR
  checking re-building of vignette outputs ... WARNING

* RaMS
  checking re-building of vignette outputs ... WARNING

* SPARSEMODr
  checking re-building of vignette outputs ... WARNING

### Failed to check

* loon.shiny (NA)
* loon.tourr (NA)
