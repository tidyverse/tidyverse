# covid19br

<details>

* Version: 0.1.4
* GitHub: https://github.com/fndemarqui/covid19br
* Source code: https://github.com/cran/covid19br
* Date/Publication: 2022-03-30 23:40:05 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "covid19br")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘covid19br.Rmd’ using rmarkdown
    ── Attaching packages ────────────────────────────────── tidyverse 1.3.1.9000 ──
    ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ✔ readr   2.1.2     ✔ forcats 0.5.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ...
    Quitting from lines 23-38 (maps.Rmd) 
    Error: processing vignette 'maps.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘maps.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘covid19br.Rmd’ ‘election2018.Rmd’ ‘maps.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   6.2Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2385 marked UTF-8 strings
    ```

# dumbbell

<details>

* Version: 0.1
* GitHub: https://github.com/foocheung2/dumbbell
* Source code: https://github.com/cran/dumbbell
* Date/Publication: 2021-02-25 09:10:02 UTC
* Number of recursive dependencies: 129

Run `cloud_details(, "dumbbell")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dumbbell-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dumbbell
    > ### Title: Dumbbell Plot
    > ### Aliases: dumbbell
    > ### Keywords: dumbbell
    > 
    > ### ** Examples
    > 
    ...
    > 
    > d$Subject<-factor(d$Subject, levels = e$Subject)
    > 
    > 
    > ## Basic plot
    > dumbbell(xdf=d,id= "Subject",key="analysis.x",column1 = "result.x",column2 = "result.y") 
    Error in UseMethod("gather") : 
      no applicable method for 'gather' applied to an object of class "c('dtplyr_step_mutate', 'dtplyr_step')"
    Calls: dumbbell ... structure -> tbl_vars_dispatch -> group_by -> gather
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘dumbbell.Rmd’ using rmarkdown
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-2'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    Quitting from lines 26-71 (dumbbell.Rmd) 
    Error: processing vignette 'dumbbell.Rmd' failed with diagnostics:
    no applicable method for 'gather' applied to an object of class "c('dtplyr_step_mutate', 'dtplyr_step')"
    --- failed re-building ‘dumbbell.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘dumbbell.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# LexFindR

<details>

* Version: 1.0.2
* GitHub: https://github.com/maglab-uconn/LexFindR
* Source code: https://github.com/cran/LexFindR
* Date/Publication: 2021-10-29 06:30:08 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "LexFindR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    Quitting from lines 156-176 (my-vignette.Rmd) 
    Error: processing vignette 'my-vignette.Rmd' failed with diagnostics:
    no applicable method for 'rowwise' applied to an object of class "c('dtplyr_step_mutate', 'dtplyr_step')"
    --- failed re-building ‘my-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘my-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# RaMS

<details>

* Version: 1.0.0
* GitHub: https://github.com/wkumler/RaMS
* Source code: https://github.com/cran/RaMS
* Date/Publication: 2021-03-22 16:00:02 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "RaMS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Intro-to-RaMS.Rmd’ using rmarkdown
    
    Attaching package: 'data.table'
    
    The following objects are masked from 'package:dplyr':
    
        between, first, last
    
    The following object is masked from 'package:purrr':
    ...
    --- failed re-building ‘Intro-to-RaMS.Rmd’
    
    --- re-building ‘RaMS-and-friends.Rmd’ using rmarkdown
    --- finished re-building ‘RaMS-and-friends.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Intro-to-RaMS.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# SPARSEMODr

<details>

* Version: 1.1.0
* GitHub: https://github.com/NAU-CCL/SPARSEMODr
* Source code: https://github.com/cran/SPARSEMODr
* Date/Publication: 2021-07-01 17:50:02 UTC
* Number of recursive dependencies: 119

Run `cloud_details(, "SPARSEMODr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘covid-19-model-vary-hosp.Rmd’ using rmarkdown
    Loading required package: future
    ── Attaching packages ────────────────────────────────── tidyverse 1.3.1.9000 ──
    ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ✔ readr   2.1.2     ✔ forcats 0.5.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ...
    Quitting from lines 238-271 (seir-model.Rmd) 
    Error: processing vignette 'seir-model.Rmd' failed with diagnostics:
    'dimnames' applied to non-array
    --- failed re-building ‘seir-model.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘covid-19-model-vary-hosp.Rmd’ ‘covid-19-model.Rmd’ ‘seir-model.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘future’ ‘geosphere’ ‘lubridate’ ‘tidyverse’ ‘viridis’
      All declared Imports should be used.
    ```

