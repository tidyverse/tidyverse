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
