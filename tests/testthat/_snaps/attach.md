# if no packages, shows nothing

    Code
      cat(tidyverse_attach_message(character()))

# message lists all core tidyverse packages

    Code
      cat(tidyverse_attach_message(core))
    Output
      -- Attaching core tidyverse packages ------------------------ tidyverse 1.0.0 --
      v dplyr     1.0.0     v readr     1.0.0
      v forcats   1.0.0     v stringr   1.0.0
      v ggplot2   1.0.0     v tibble    1.0.0
      v lubridate 1.0.0     v tidyr     1.0.0
      v purrr     1.0.0     

# highlights dev versions in red

    Code
      highlight_version(c("1.0.0", "1.0.0.9000", "0.9000.0.9000", "1.0.0-rc"))
    Output
      [1] "1.0.0"                                        
      [2] "1.0.0.\033[31m9000\033[39m"                   
      [3] "0.\033[31m9000\033[39m.0.\033[31m9000\033[39m"
      [4] "1.0.0-rc"                                     

