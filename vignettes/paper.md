---
title: 'Welcome to the Tidyverse'
output:
  rmarkdown::html_vignette:
    keep_md: TRUE
tags:
- data science
- R
authors:
- name: Hadley Wickham
  orcid: 0000-0003-4757-117X
  affiliation: 1
- name: Mara Averick
  orcid: 0000-0001-9659-6192
  affiliation: 1
- name: Jennifer Bryan
  orcid: 0000-0002-6983-2759
  affiliation: 1
- name: Winston Chang
  orcid: 0000-0002-1576-2126
  affiliation: 1
- name: Lucy D'Agostino McGowan
  orcid: 0000-0001-7297-9359
  affiliation: 8
- name: Romain François
  orcid: 0000-0002-2444-4226
  affiliation: 1
- name: Garrett Grolemund
  orcid: 0000-0002-7765-6011
  affiliation: 1
- name: Alex Hayes
  orcid: 0000-0002-4985-5160
  affiliation: 12
- name: Lionel Henry
  orcid: 0000-0002-8143-5908
  affiliation: 1
- name: Jim Hester
  orcid: 0000-0002-2739-7082
  affiliation: 1
- name: Max Kuhn
  orcid: 0000-0003-2402-136X
  affiliation: 1
- name: Thomas Lin Pedersen
  orcid: 0000-0002-5147-4711
  affiliation: 1
- name: Evan Miller
- name: Stephan Milton Bache
  affiliation: 3
- name: Kirill Müller
  orcid: 0000-0002-1416-3412
  affiliation: 2
- name: David Robinson
  affiliation: 5
- name: Dana Paige Seidel
  orcid: 0000-0002-9088-5767
  affiliation: 10
- name: Vitalie Spinu
  orcid: 0000-0002-2138-3413
  affiliation: 4
- name: Kohske Takahashi
  orcid: 0000-0001-6076-4828
  affiliation: 9
- name: Davis Vaughan
  orcid: 0000-0003-4777-038X
  affiliation: 1
- name: Claus Wilke
  orcid: 0000-0002-7470-9261
  affiliation: 6
- name: Kara Woo
  orcid: 0000-0002-5125-4188
  affiliation: 7
- name: Hiroaki Yutani
  orcid: 0000-0002-3385-7233
  affiliation: 11
affiliations:
 - name: RStudio
   index: 1
 - name: cynkra
   index: 2
 - name: Redbubble
   index: 3
 - name: Erasmus University Rotterdam
   index: 4
 - name: Flatiron Health
   index: 5
 - name: Department of Integrative Biology, The University of Texas at Austin
   index: 6
 - name: Sage Bionetworks
   index: 7
 - name: Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
   index: 8
 - name: Chukyo University, Japan
   index: 9
 - name: Department of Environmental Science, Policy, & Management, University of California, Berkeley
   index: 10
 - name: LINE Corporation
   index: 11
 - name: University of Wisconsin, Madison
   index: 12
date: 17 May 2019
bibliography: paper.bib
---



# Summary

![](tidyverse-logo.png)

At a high level, the tidyverse is a language for solving data science challenges with R code. Its primary goal is to facilitate a conversation between a human and a computer about data. Less abstractly, the tidyverse is a collection of R packages that share a high-level design philosophy and low-level grammar and data structures, so that learning one package makes it easier to learn the next. 

The tidyverse encompasses the repeated tasks at the heart of every data science project: data import, tidying, manipulation, visualisation, and programming. We expect that almost every project will use multiple domain-specific packages outside of the tidyverse: our goal is to provide tooling for the most common challenges, not to solve every possible problem. Notably, the tidyverse doesn't include tools for statistical modelling or communication. These toolkits are critical for data science, but are so large that they merit separate treatment.

This paper describes the tidyverse package, the components of the tidyverse, and some of the underlying philosophy.

# Tidyverse package

The tidyverse is a collection of packages that can easily be installed with a single "meta"-package, which called "tidyverse". This provides a convenient way of downloading all tidyverse packages with a single R command:


```r
install.packages("tidyverse")
```

The core tidyverse includes the packages that you're likely to use in everyday data analyses, and these are attached when you attach the tidyverse package:


```r
library(tidyverse)
#> ── Attaching packages ───────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.0.9000      ✔ purrr   0.3.2      
#> ✔ tibble  2.1.3           ✔ dplyr   0.8.2      
#> ✔ tidyr   0.8.99.9000     ✔ stringr 1.4.0      
#> ✔ readr   1.3.1           ✔ forcats 0.4.0
#> ── Conflicts ──────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
```

As of tidyverse 1.2.0, the core packages include ggplot2 [@ggplot2], dplyr [@R-dplyr], tidyr [@R-tidyr], readr [@R-readr], purrr [@R-purrr], tibble [@R-tibble], stringr [@R-stringr], and forcats [@R-forcats]. 

Non-core packages are installed with `install.packages("tidyverse")`, but are not attached by `library(tidyverse)`. They play more specialised roles, so will be attached by the analyst as needed.

# Components

How do the component packages of the tidyverse fit together? We use the model of data science tools from "R for Data Science" [@r4ds]:

![](data-science.png)

Every analysis starts with data __import__: if you can't get your data into R, you can't do data science on it! Data import takes data stored in a file, database, or web API, and reads it into a data frame in R. Data import is supported by the core [readr](https://readr.tidyverse.org/) [@R-readr] package for flat files (like csv, tsv, and fwf). Additional non-core packages, such as [readxl](https://readxl.tidyverse.org) [@R-readxl], [haven](https://haven.tidyverse.org) [@R-haven], and [rvest](https://rvest.tidyverse.org/) [@R-rvest], make it possible to import data stored in other common formats or retrieve it directly from the web.

Next, we recommend that you __tidy__ your data, getting it into a consistent form that makes the rest of the analysis easier. Most functions in the tidyverse work with tidy data [@tidy-data], where every column is a variable, every row is an observation, and every cell contains a single value. If your data is not already in this form (almost always!), the core [tidyr](https://tidyr.tidyverse.org/) [@R-tidyr] provides tools to tidy it up.



Data __transformation__ is supported by the core [dplyr](https://dplyr.tidyverse.org/) [@R-dplyr] package. dplyr provides verbs that work with whole data frames, such as `mutate()` to create new variables, `filter()` to find observations matching given criteria, and `left_join()` and friends to combine multiple tables. dplyr is paired with packages that provide tools for specific column types: 

* [stringr](https://stringr.tidyverse.org) for strings.
* [forcats](https://forcats.tidyverse.org) for factors, R's categorical data 
  type.
* [lubridate](https://lubridate.tidyverse.org) [@R-lubridate] for dates and 
  date-times.
* [hms](https://hms.tidyverse.org/) [@R-hms] for clock times.

There are two main tools for understanding data: __visualisation__ and __modelling__. The tidyverse provides the [ggplot2](https://ggplot2.tidyverse.org/) [@ggplot2] package for visualisation. ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics [@wilkinson2005]. You provide the data, tell ggplot2 how to map variables to aesthetics, what graphical primitives to use, and it takes care of the details. Modelling is outside the scope of this paper, but is part of the closely affiliated [tidymodels](https://github.com/tidymodels) [@R-tidymodels] project, which shares interface design and data structures with the tidyverse.

Finally, you'll need to communicate your results to someone else. __Communication__ is one of the most important parts of data science, but is not included within tidyverse. Instead, we expect people will use other R packages, like [rmarkdown](https://rmarkdown.rstudio.com/) [@R-rmarkdown] and [shiny](https://shiny.rstudio.com/) [@R-shiny], which support dozens of static and dynamic output formats.

Surrounding all these tools is __programming__. Programming is a cross-cutting tool that you use in every part of a data science project. Programming tools in the tidyverse include:

* [purrr](https://purrr.tidyverse.org/) [@R-purrr], which enhances R’s
  functional programming toolkit.

* [tibble](https://tibble.tidyverse.org/) [@R-tibble], which provides
  a modern re-imagining of the venerable data frame, keeping what time has
  proven to be effective, and throwing out what it has not. 

* [reprex](https://reprex.tidyverse.org/) [@R-reprex], which helps 
  programmers get help when they get stuck by easing the creation of 
  reproducible examples.

* [magrittr](https://magrittr.tidyverse.org) [@R-magrittr], which provides 
  the pipe, `%>%`, used throughout the tidyverse. The pipe is a tool for
  function composition, making it easier to solve large problems by breaking 
  them into small pieces.

# Philosophy

We are still working to explicitly describe the unifying principles that make the tidyverse consistent, but you can read our latest thoughts at <https://principles.tidyverse.org/>. There is one particularly important principle that we want to call out here: the tidyverse is fundamentally __human centred__. That is, the tidyverse is designed to support the activities of a human data analyst, so to be effective tool builders, we must explicitly recognise and acknowledge the strengths and weaknesses of human cognition.

This is particularly important for R, because it’s a language that’s used primarily by non-programmers, and we want to make it as easy as possible for first-time and end-user programmers to learn the tidyverse. We believe deeply in the motivations that lead to the creation of S: "to turn ideas into software, quickly and faithfully" [@programming-with-data]. This means that we spend a lot of time thinking about interface design, and have recently started experimenting with [surveys](https://github.com/hadley/table-shapes) to help guide interface choices. 

Similarly, the tidyverse is not just the collection of packages --- it is also the community of people who use them. We want the tidyverse to be a diverse, inclusive, and welcoming community. We are still developing our skills in this area, but our existing approaches include active use of Twitter to [solicit feedback](https://twitter.com/hadleywickham/status/948722811232751617), announce updates, and generally listen to the community. We also keep users apprised of major upcoming changes through the [tidyverse blog](https://www.tidyverse.org/articles/), run [developer days](https://www.tidyverse.org/articles/2018/08/tidyverse-developer-day/), and support lively discussions on [RStudio community](https://community.rstudio.com).

# Acknowledgments

The tidyverse would not be possible without the immense work of the [R-core team](https://www.r-project.org/contributors.html) who maintain the R language and we are deeply indebted to them. We are also grateful for the financial support of [RStudio, Inc](https://www.rstudio.com/).

# References
