# tidyverse 1.1.0

* `tidyverse_deps()` and `tidyverse_packages()` are now exported so you can
  more easily see the make up of the tidyverse, and what package versions
  you have (#18, #23)

* Added a `NEWS.md` file to track changes to the package.

* Membership changes:
  
  * Removed DBI (since very different API, #16)
  * Added feather (#15)

* `suppressPackageStartupMessages()` now suppresses all messages during
   loading (#19). `suppressPackageStartupMessages()` is called automatically
   for all tidyverse packages (#27).
