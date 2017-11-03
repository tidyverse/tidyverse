# tidyverse 1.1.1.9000

* `tidyverse_conflicts()` now prints all conflicts that involve at least
  one tidyverse package. Previously it only omitted any intra-tidyverse
  conflicts (#26).

* `tidyverse_update()` now just gives you the code you need to update the 
  packges, since in general it's not possible to update packages that are
  already loaded.

* You can now suppress the startup message by setting 
  `options(tidyverse.quiet = TRUE)`

* stringr is now part of the core tidyverse, so is attached when you 
  `library(tidyverse)`.

* Added reprex to the tidyverse (#47)

*   On attach, tidyverse now makes better use of the horizontal space, 
    printing package versions and some useful info about your session (#59).
    It's now more valuable inside reprexes or committed in rendered RMarkdown
    documents.
    
    I've also tweaked the display of conflicts to hopefully make it more
    clear which function is the "winner".

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
