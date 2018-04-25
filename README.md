
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyverse <img src="man/figures/logo.png" align="right" />
==========================================================

[![Travis-CI Build Status](https://travis-ci.org/tidyverse/tidyverse.svg?branch=master)](https://travis-ci.org/tidyverse/tidyverse)

Overview
--------

The tidyverse is a set of packages that work in harmony because they share common data representations and API design. The **tidyverse** package is designed to make it easy to install and load core packages from the tidyverse in a single command.

If you'd like to learn how to use the tidyverse effectively, the best place to start is [R for data science](http://r4ds.had.co.nz).

Installation
------------

``` r
# Install from CRAN
install.packages("tidyverse")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("hadley/tidyverse")
```

Usage
-----

`library(tidyverse)` will load the core tidyverse packages:

-   [ggplot2](http://ggplot2.tidyverse.org), for data visualisation.
-   [dplyr](http://dplyr.tidyverse.org), for data manipulation.
-   [tidyr](http://tidyr.tidyverse.org), for data tidying.
-   [readr](http://readr.tidyverse.org), for data import.
-   [purrr](http://purrr.tidyverse.org), for functional programming.
-   [tibble](http://tibble.tidyverse.org), for tibbles, a modern re-imagining of data frames.
-   [stringr](https://github.com/tidyverse/stringr), for strings.
-   [forcats](https://github.com/hadley/forcats), for factors.
    
You also get a condensed summary of conflicts with other packages you have loaded:

``` r
library(tidyverse)
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats
```

You can see conflicts created later with `tidyverse_conflicts()`:

``` r
library(MASS)
#> 
#> Attaching package: 'MASS'
#> The following object is masked from 'package:dplyr':
#> 
#>     select
tidyverse_conflicts()
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats
#> select(): dplyr, MASS
```

And you can check that all tidyverse packages are up-to-date with `tidyverse_update()`:

``` r
tidyverse_update()
#> The following packages are out of date:
#>  * broom (0.4.0 -> 0.4.1)
#>  * DBI   (0.4.1 -> 0.5)
#>  * Rcpp  (0.12.6 -> 0.12.7)
#> Update now?
#> 
#> 1: Yes
#> 2: No
```

Packages
--------

As well as the core tidyverse, installing this package also installs a selection of other packages that you're likely to use frequently, but probably not in every analysis. This includes packages for:

-   Working with specific types of vectors:

    -   [hms](https://github.com/rstats-db/hms), for times.
    -   [lubridate](https://github.com/hadley/lubridate), for date/times.
-   Importing other types of data:

    -   [feather](http://github.com/wesm/feather), for sharing with Python and other languages.
    -   [haven](https://github.com/hadley/haven), for SPSS, SAS and Stata files.
    -   [httr](https://github.com/hadley/httr), for web apis.
    -   [jsonlite](https://github.com/jeroenooms/jsonlite) for JSON.
    -   [readxl](https://github.com/hadley/readxl), for `.xls` and `.xlsx` files.
    -   [rvest](https://github.com/hadley/rvest), for web scraping.
    -   [xml2](https://github.com/hadley/xml2), for XML.
-   Modelling

    -   [modelr](https://github.com/hadley/modelr), for modelling within a pipeline
    -   [broom](https://github.com/dgrtwo/broom), for turning models into tidy data
