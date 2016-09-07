
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyverse
=========

[![Travis-CI Build Status](https://travis-ci.org/hadley/tidyverse.svg?branch=master)](https://travis-ci.org/hadley/tidyverse)

The tidyverse is a set of packages that work in harmony because they share common data representations and API design. The **tidyverse** package is designed to make it easy to install and load core packages from the tidyverse in a single command.

If you'd like to learn how to use the tidyverse effectively, the best place to start is [R for data science](http://r4ds.had.co.nz).

Installation
------------

You can install tidyverse from github with:

``` r
# install.packages("devtools")
devtools::install_github("hadley/tidyverse")
```

This will install the **core** tidyverse packages that you are likely to use in almost every analysis:

-   ggplot2, for data visualisation.
-   dplyr, for data manipulation.
-   tidyr, for data tidying.
-   readr, for data import.
-   purrr, for functional programming.
-   tibble, for tibbles, a modern re-imagining of data frames.

It also installs a selection of other tidyverse packages that you're likely to use frequently, but probably not in every analysis. This includes packages for:

-   Working with specific types of vectors:

    -   hms, for times.
    -   stringr, for strings.
    -   lubridate, for date/times.
    -   forcats, for factors.
-   Importing other types of data:

    -   DBI, for databases.
    -   haven, for SPSS, SAS and Stata files.
    -   httr, for web apis.
    -   [jsonlite](https://github.com/jeroenooms/jsonlite) for JSON.
    -   readxl, for `.xls` and `.xlsx` files.
    -   rvest, for web scraping.
    -   xml2, for XML.
-   Modelling

    -   modelr, for modelling within a pipeline
    -   [broom](https://github.com/dgrtwo/broom), for turning models into tidy data

These packages will be installed along with tidyverse, but you'll load them explicitly with `library()`.

Usage
-----

`library(tidyverse)` will load the core tidyverse packages: ggplot2, tibble, tidyr, readr, purrr, and dplyr. You also get a condensed summary of conflicts with other packages you have loaded:

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
