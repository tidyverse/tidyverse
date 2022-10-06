
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyverse <a href='https://tidyverse.tidyverse.org'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidyverse)](https://cran.r-project.org/package=tidyverse)
[![R-CMD-check](https://github.com/tidyverse/tidyverse/workflows/R-CMD-check/badge.svg)](https://github.com/tidyverse/tidyverse/actions)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/tidyverse/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/tidyverse?branch=main)
<!-- badges: end -->

## Overview

The tidyverse is a set of packages that work in harmony because they
share common data representations and API design. The **tidyverse**
package is designed to make it easy to install and load core packages
from the tidyverse in a single command.

If you’d like to learn how to use the tidyverse effectively, the best
place to start is [R for data science](https://r4ds.had.co.nz).

## Installation

<div class=".pkgdown-release">

``` r
# Install from CRAN
install.packages("tidyverse")
```

</div>

<div class=".pkgdown-devel">

``` r
# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("tidyverse/tidyverse")
```

</div>

If you’re compiling from source, you can run
`pak::pkg_system_requirements("tidyverse")`, to see the complete set of
system packages needed on your machine.

If you’re compiling from source, you can run
`pak::pkg_system_requirements("tidyverse")`, to see the complete set of
system packages needed on your machine.

## Usage

`library(tidyverse)` will load the core tidyverse packages:

- [ggplot2](https://ggplot2.tidyverse.org), for data visualisation.
- [dplyr](https://dplyr.tidyverse.org), for data manipulation.
- [tidyr](https://tidyr.tidyverse.org), for data tidying.
- [readr](https://readr.tidyverse.org), for data import.
- [purrr](https://purrr.tidyverse.org), for functional programming.
- [tibble](https://tibble.tidyverse.org), for tibbles, a modern
  re-imagining of data frames.
- [stringr](https://github.com/tidyverse/stringr), for strings.
- [forcats](https://github.com/tidyverse/forcats), for factors.
- [lubridate](https://github.com/tidyverse/lubridate), for date/times.

You also get a condensed summary of conflicts with other packages you
have loaded:

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ─────────────────── tidyverse 1.3.2.9000 ──
#> ✔ dplyr     1.0.10     ✔ readr     2.1.3 
#> ✔ forcats   0.5.2      ✔ stringr   1.4.1 
#> ✔ ggplot2   3.3.6      ✔ tibble    3.1.8 
#> ✔ lubridate 1.8.0      ✔ tidyr     1.2.1 
#> ✔ purrr     0.3.5      
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors
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
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ✖ MASS::select()  masks dplyr::select()
#> ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors
```

And you can check that all tidyverse packages are up-to-date with
`tidyverse_update()`:

``` r
tidyverse_update()
#> The following packages are out of date:
#>  * broom (0.4.0 -> 0.4.1)
#>  * DBI   (0.4.1 -> 0.5)
#>  * Rcpp  (0.12.6 -> 0.12.7)
#>  
#> Start a clean R session then run:
#> install.packages(c("broom", "DBI", "Rcpp"))
```

## Packages

As well as the core tidyverse, installing this package also installs a
selection of other packages that you’re likely to use frequently, but
probably not in every analysis. This includes packages for:

- Working with specific types of vectors:

  - [hms](https://github.com/tidyverse/hms), for times.

- Importing other types of data:

  - [feather](https://github.com/wesm/feather), for sharing with Python
    and other languages.
  - [haven](https://github.com/tidyverse/haven), for SPSS, SAS and Stata
    files.
  - [httr](https://github.com/r-lib/httr), for web apis.
  - [jsonlite](https://github.com/jeroen/jsonlite) for JSON.
  - [readxl](https://github.com/tidyverse/readxl), for `.xls` and
    `.xlsx` files.
  - [rvest](https://github.com/tidyverse/rvest), for web scraping.
  - [xml2](https://github.com/r-lib/xml2), for XML.

- Modelling

  - [modelr](https://github.com/tidyverse/modelr), for modelling within
    a pipeline
  - [broom](https://github.com/tidymodels/broom), for turning models
    into tidy data

## Code of Conduct

Please note that the tidyverse project is released with a [Contributor
Code of Conduct](https://tidyverse.tidyverse.org/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
