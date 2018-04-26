### Initial transfer

  * [ ] Move to tidyverse organisation.
  * [ ] Search for `hadley/` and replace with `tidyverse/`.
  * [ ] Modify `.git/config`

### travis

  * [ ] Make sure includes:

```yaml
r:
 - oldrel
 - release
 - devel
```

### pkgdown

  * [ ] Add template `_pkgdown.yml`

```yaml
template:
  package: tidytemplate
  default_assets: false
```

  * [ ] Add to `.Rbuildignore`

  * [ ] `build_site()` then push.

  * [ ] In admin use `docs/` for documentation and turn off wiki

  * [ ] create `docs/CNAME` consisting of `PACKAGE.tidyverse.org`

  * [ ] Update url on github repo page.

  * [ ] Add url to DESCRIPTION.

### Logo

  * [ ] Copy in hex logo (120 x 139) and add to `.Rbuildignore`. *The
    location of `logo.png` needs some thought to simultaneously
    satisfy CRAN re: local images in README and comply with pkgdown's
    expectations.*

```r
library(magick)

image_read("clippy/readxl.png") %>% 
  image_scale(geometry = "120") %>%
  image_write("tools/logo.png")
 ```

  * [ ] Package name + hex logo.

```
# {{package name}} <img src="logo.png" align="right" />
```

  * [ ] Add image + link on <http://tidyverse.org>

### `README.Rmd`

  * [ ] Switch from `.md` to `.Rmd`

  * [ ] Install instructions:

````r
```{r, eval = FALSE}
# The easiest way to get readr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just readr:
 install.packages("readr")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/readr")
```
````

  * [ ] Overview: 1-2 paragraphs including brief description/goals. Add h2, and set:

   ```yaml
   home:
     strip_header: true
   ```

  * [ ]  Link to appropriate R4DS chapter:

   ```md
   If you are new to readr, the best place to start is the
   [data import chapter](http://r4ds.had.co.nz/data-import.html) in R
   for data science.
   ```

  * [ ] Add link to `_pkgdown.yaml`

   ```yaml
     links:
     - text: Learn more
       href: http://r4ds.had.co.nz/data-import.html
   ```

  * [ ]  Usage section shows a few examples.

### Package help

  * [ ] Review package title and description in DESCRIPTION.

  * [ ] Ensure that RStudio is listed as funder.

  * [ ] Add package help page.

   ```r
   #' @keywords internal
   "_PACKAGE"
   ```

  * [ ] Switch to markdown.

### Reference index

  * [ ] Group functions deliberately in function reference.

  * [ ] Review topic titles. Check for trailing full stops.

### Articles

  * [ ] Ensure all vignettes neither date, nor author.

  * [ ] Write `vignette/packagename.Rmd` which expands on README.

  * [ ] Add direct links to navbar.

   ```yaml
   - text: Intro
     href: articles/stringr.html
   - text: RegEx
     href: articles/regular-expressions.html
   ```

  * [ ] Translate blog posts to `vignettes/releases`. Update navbar.

   ```yaml
   - text: News
     menu:
     - text: "Release notes"
     - text: "Version 1.1.0"
       href: articles/releases/stringr-1.1.0.html
     - text: "Version 1.0.0"
       href: articles/releases/stringr-1.0.0.html
     - text: "------------------"
     - text: "Change log"
       href: news/index.html
   ```
