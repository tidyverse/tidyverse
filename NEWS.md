# tidyverse 1.3.0

* The tidyverse now has an associated paper at the 
  [Journal of Open Source Software](http://joss.theoj.org/) that you can
  use to cite the tidyverse if you use it in a paper - see 
  `citation("tidyverse")` for details.

* Eliminate repeats in the package list when loading an odd number of 
  packages (#94, #100, @dchiu911)

* Packages attached from same library they were initially loaded from 
  (#171, @gabrocsardi)

* If conflicted package is loaded, omit display of conflicts. 
  This includes fix to `tidyverse_conflicts()` to avoid accidentally
  triggering conflited shims (#136).

* `tidyverse_deps()` now succeeds even if a dependency of a depedency 
  is missing (#152, @PoGibas). It no longer includes dependencies that
  are needed by the tidyverse package but are not strictly part of the
  tidyverse (i.e. cli, crayon, and rstudioapi).

* `tidyverse_deps()` and `tidyverse_update()` gain a new `repos` argument
  that gets passed to the base function `available.packages()` (@zkamvar, #82)

* `tidyverse_packages()` corrently returns packages names (#93, #106, @coatless)

* `tidyverse_sitrep()` gives you a situtation report on your install of 
  the tidyverse (#203)

# tidyverse 1.2.1

* Require modern versions of all packages (#85)

* Work with RStudio 1.0 and earlier (#88).

# tidyverse 1.2.0

## Changes to tidyverse membership

* stringr and forcats have been added to the core tidyverse, so they are
  attached by `library(tidyverse)`.

* reprex joins the tidyverse to make it easier to create reproducible
  examples (#47)

## Other improvements

* On attach, tidyverse now makes better use of the horizontal space, 
  printing packages and versions in two columns (#59). It only prints
  packages that it attaches, not packages that you've already attached.
  Development versions are highlighted in red.
  
    You can now suppress this startup message by setting 
    `options(tidyverse.quiet = TRUE)`

* `tidyverse_conflicts()` now prints all conflicts that involve at least
  one tidyverse package; Previously it only omitted any intra-tidyverse
  conflicts (#26). I've also tweaked the display of conflicts to hopefully 
  make it more clear which function is the "winner".

* `tidyverse_update()` now just gives you the code you need to update the 
  packges, since in general it's not possible to update packages that are
  already loaded.

* feather is now _actually_ in suggests.

# tidyverse 1.1.1

* Moved feather from Imports to Suggests - feather is part of the tidyverse
  but it's installation requirements (C++11 + little-endian) make it painful
  in many scenarios (#36).

# tidyverse 1.1.0

* Added a `NEWS.md` file to track changes to the package.

* Membership changes:
  
  * Removed DBI (since very different API, #16)
  * Added feather (#15)

* `tidyverse_deps()` and `tidyverse_packages()` are now exported so you can
  more easily see the make up of the tidyverse, and what package versions
  you have (#18, #23)

* `suppressPackageStartupMessages()` now suppresses all messages during
   loading (#19). `suppressPackageStartupMessages()` is called automatically
   for all tidyverse packages (#27).
