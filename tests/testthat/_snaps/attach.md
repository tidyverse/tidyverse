# if no packages, shows nothing

    Code
      cat(tidyverse_attach_message(character()))

# message lists all core tidyverse packages

    Code
      cat(tidyverse_attach_message(core))
    Output
      -- Attaching packages --------------------------------------- tidyverse 1.0.0 --
      v ggplot2   1.0.0     v dplyr     1.0.0
      v tibble    1.0.0     v stringr   1.0.0
      v tidyr     1.0.0     v forcats   1.0.0
      v readr     1.0.0     v lubridate 1.0.0
      v purrr     1.0.0     

