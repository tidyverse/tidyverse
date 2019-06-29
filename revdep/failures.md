# CNPBayes

<details>

* Version: 1.12.0
* Source code: https://github.com/cran/CNPBayes
* URL: https://github.com/scristia/CNPBayes
* BugReports: https://github.com/scristia/CNPBayes/issues
* Date/Publication: 2018-10-30
* Number of recursive dependencies: 155

Run `revdep_details(,"CNPBayes")` for more info

</details>

## In both

*   R CMD check timed out
    

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R     2.0Mb
        doc   3.5Mb
    ```

# CVE

<details>

* Version: 1.8.0
* Source code: https://github.com/cran/CVE
* Date/Publication: 2018-10-30
* Number of recursive dependencies: 180

Run `revdep_details(,"CVE")` for more info

</details>

## In both

*   checking whether package ‘CVE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CVE/new/CVE.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CVE’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘CVE’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CVE/new/CVE.Rcheck/CVE’

```
### CRAN

```
* installing *source* package ‘CVE’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘CVE’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CVE/old/CVE.Rcheck/CVE’

```
# heatwaveR

<details>

* Version: 0.3.6
* Source code: https://github.com/cran/heatwaveR
* URL: https://robwschlegel.github.io/heatwaveR/index.html, https://github.com/robwschlegel/heatwaveR
* BugReports: https://github.com/robwschlegel/heatwaveR/issues
* Date/Publication: 2019-01-16 20:30:03 UTC
* Number of recursive dependencies: 111

Run `revdep_details(,"heatwaveR")` for more info

</details>

## In both

*   checking whether package ‘heatwaveR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/heatwaveR/new/heatwaveR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘heatwaveR’ ...
** package ‘heatwaveR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/heatwaveR/new/heatwaveR.Rcheck/heatwaveR’

```
### CRAN

```
* installing *source* package ‘heatwaveR’ ...
** package ‘heatwaveR’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/heatwaveR/old/heatwaveR.Rcheck/heatwaveR’

```
# martini

<details>

* Version: 1.2.0
* Source code: https://github.com/cran/martini
* Date/Publication: 2018-10-30
* Number of recursive dependencies: 109

Run `revdep_details(,"martini")` for more info

</details>

## In both

*   checking whether package ‘martini’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/martini/new/martini.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘martini’ ...
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DAS_GINLIB -DAS_RGINLIB -fopenmp -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/Rgin/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘martini’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/martini/new/martini.Rcheck/martini’

```
### CRAN

```
* installing *source* package ‘martini’ ...
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DAS_GINLIB -DAS_RGINLIB -fopenmp -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/Rgin/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘martini’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/martini/old/martini.Rcheck/martini’

```
# specklestar

<details>

* Version: 0.0.1.7
* Source code: https://github.com/cran/specklestar
* URL: https://drastega.github.io/docs/specklestar_vignette.html
* BugReports: https://github.com/drastega/specklestar/issues
* Date/Publication: 2018-02-08 18:14:49 UTC
* Number of recursive dependencies: 115

Run `revdep_details(,"specklestar")` for more info

</details>

## In both

*   checking whether package ‘specklestar’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/specklestar/new/specklestar.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘specklestar’ ...
** package ‘specklestar’ successfully unpacked and MD5 sums checked
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c image_helper.cpp -o image_helper.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c middle_frame.cpp -o middle_frame.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c speckle_acf.cpp -o speckle_acf.o
speckle_acf.cpp:4:10: fatal error: 'fftw3.h' file not found
#include <fftw3.h>
         ^~~~~~~~~
1 error generated.
make: *** [speckle_acf.o] Error 1
ERROR: compilation failed for package ‘specklestar’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/specklestar/new/specklestar.Rcheck/specklestar’

```
### CRAN

```
* installing *source* package ‘specklestar’ ...
** package ‘specklestar’ successfully unpacked and MD5 sums checked
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c image_helper.cpp -o image_helper.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c middle_frame.cpp -o middle_frame.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I/usr/local/include   -fPIC  -Wall -g -O2  -c speckle_acf.cpp -o speckle_acf.o
speckle_acf.cpp:4:10: fatal error: 'fftw3.h' file not found
#include <fftw3.h>
         ^~~~~~~~~
1 error generated.
make: *** [speckle_acf.o] Error 1
ERROR: compilation failed for package ‘specklestar’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/specklestar/old/specklestar.Rcheck/specklestar’

```
# updog

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/updog
* BugReports: http://github.com/dcgerard/updog/issues
* Date/Publication: 2018-07-27 17:50:06 UTC
* Number of recursive dependencies: 109

Run `revdep_details(,"updog")` for more info

</details>

## In both

*   checking whether package ‘updog’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/updog/new/updog.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘updog’ ...
** package ‘updog’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/updog/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘updog’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/updog/new/updog.Rcheck/updog’

```
### CRAN

```
* installing *source* package ‘updog’ ...
** package ‘updog’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/updog/RcppArmadillo/include" -I/usr/local/include  -fopenmp -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘updog’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/updog/old/updog.Rcheck/updog’

```
# weibulltools

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/weibulltools
* Date/Publication: 2019-01-29 16:10:03 UTC
* Number of recursive dependencies: 105

Run `revdep_details(,"weibulltools")` for more info

</details>

## In both

*   checking whether package ‘weibulltools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/weibulltools/new/weibulltools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘weibulltools’ ...
** package ‘weibulltools’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/weibulltools/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘weibulltools’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/weibulltools/new/weibulltools.Rcheck/weibulltools’

```
### CRAN

```
* installing *source* package ‘weibulltools’ ...
** package ‘weibulltools’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/weibulltools/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘weibulltools’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/weibulltools/old/weibulltools.Rcheck/weibulltools’

```
