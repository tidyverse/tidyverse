
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyverse
=========

[![Travis-CI Build Status](https://travis-ci.org/hadley/tidyverse.svg?branch=master)](https://travis-ci.org/hadley/tidyverse)

The goal of tidyverse is to make it easy to install and load multiple tidyverse packages in a single command.

Installation
------------

You can install tidyverse from github with:

``` r
# install.packages("devtools")
devtools::install_github("hadley/tidyverse")
```

This will also install tidyverse packages broom, dplyr, forcats, ggplot2, lubridate, magrittr, modelr, purrr, readr, stringr, tibble, and tidyr.

Usage
-----

Use `library(tidyverse)` to automatically load the tidyverse packages you are likely to use in almost every analysis: ggplot2, tibble, tidyr, readr, purrr, and dplyr. You also get a condensed summary of conflicts with other packages you have loaded:

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
