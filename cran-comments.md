## R CMD check results

0 errors | 0 warnings | 1 notes

> checking package dependencies ... NOTE
  Imports includes 29 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

This is a false positive - the majority of packages are maintained by me or my team, so there's little risk of them becoming unavailable.

## revdepcheck results

We checked 91 reverse dependencies (84 from CRAN + 7 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 4 packages

Issues with CRAN packages are summarised below.

### Failed to check

* heatwaveR    (NA)
* specklestar  (NA)
* updog        (NA)
* weibulltools (NA)
