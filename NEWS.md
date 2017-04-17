# tidyverse 1.1.1.9000

* stringr is now part of the core tidyverse, so is attached when you 
  `library(tidyverse)`.

* Added reprex to the tidyverse (#47)

* On attach, tidyverse now makes better use of the horizontal space, 
  printing package versions and some useful info about your session (#59)

* Actually move feather to suggests

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
