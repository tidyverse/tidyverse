# useful conflicts message

    Code
      tidyverse_conflicts(c("base", "stats"))
    Output
      -- Conflicts ------------------------------------------ tidyverse_conflicts() --
      x dplyr::filter() masks stats::filter()
      x dplyr::lag()    masks stats::lag()
      i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

