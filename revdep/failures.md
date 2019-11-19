# CNPBayes

<details>

* Version: 1.13.5
* Source code: https://github.com/cran/CNPBayes
* URL: https://github.com/scristia/CNPBayes
* BugReports: https://github.com/scristia/CNPBayes/issues
* Date/Publication: 2019-01-05
* Number of recursive dependencies: 163

Run `revdep_details(,"CNPBayes")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘CNPBayes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggChains
    > ### Title: Trace plots of MCMC chains and mixture model densities
    > ### Aliases: ggChains ggMixture ggMixture,MultiBatchCopyNumber-method
    > ###   ggMixture,MultiBatchCopyNumberPooled-method
    > ###   ggMixture,MultiBatchModel-method ggMixture,MultiBatch-method
    > ###   ggMixture,MultiBatchPooled-method ggChains,MultiBatchModel-method
    > ###   ggChains,MultiBatchPooled-method
    > 
    > ### ** Examples
    > 
    >   sb <- SingleBatchModelExample
    >   iter(sb) <- 1000
    >   burnin(sb) <- 100
    >   sb <- posteriorSimulation(sb)
    >   fig.chains <- ggChains(sb)
    Error: 1 components of `...` had unexpected names.
    
    We detected these problematic arguments:
    ```

*   R CMD check timed out
    

*   checking whether package ‘CNPBayes’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘IRanges’ was built under R version 3.6.1
      Warning: package ‘S4Vectors’ was built under R version 3.6.1
      Warning: package ‘GenomicRanges’ was built under R version 3.6.1
    See ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00install.out’ for details.
    ```

*   checking for missing documentation entries ... WARNING
    ```
    ...
      generic 'sigma' and siglist 'MultiBatchPooled'
      generic 'sigma<-' and siglist 'MixtureModel'
      generic 'sigma<-' and siglist 'MultiBatchPooled'
      generic 'tau2' and siglist 'MultiBatch'
      generic 'theta' and siglist 'MultiBatch'
      generic 'theta<-' and siglist 'McmcChains,ANY'
      generic 'theta<-' and siglist 'MixtureModel,ANY'
      generic 'theta<-' and siglist 'MultiBatch,matrix'
      generic 'theta<-' and siglist 'MultiBatchModel,ANY'
      generic 'thin' and siglist 'MultiBatch'
      generic 'thin' and siglist 'MultiBatchList'
      generic 'thin<-' and siglist 'McmcParams,numeric'
      generic 'thin<-' and siglist 'MultiBatch,numeric'
      generic 'thin<-' and siglist 'MultiBatchList,numeric'
      generic 'triodata_lrr' and siglist 'TrioBatchModel'
      generic 'z' and siglist 'MultiBatch'
      generic 'zFreq' and siglist 'MultiBatch'
    All user-level objects in a package (including S4 classes and methods)
    should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Slots for class 'MultiBatch'
      Code: chains current_values data down_sample flags parameters specs
            summaries
      Docs: chains current_values data down_sample flags parameters
            summaries
    
    S4 class codoc mismatches from documentation object 'MultiBatchModel-class':
    Slots for class 'MultiBatchModel'
      Code: .internal.constraint .internal.counter batch batchElements data
            data.mean data.prec hyperparams k label_switch loglik logprior
            marginal_lik mcmc.chains mcmc.params modes mu nu.0 pi
            predictive probz sigma2 sigma2.0 tau2 theta u z zfreq zstar
      Inherited: k hyperparams theta sigma2 nu.0 sigma2.0 pi mu tau2
            predictive zstar data data.mean data.prec z zfreq probz u
            logprior loglik mcmc.chains batch batchElements modes
            mcmc.params label_switch marginal_lik .internal.constraint
            .internal.counter
      Docs: .internal.constraint batch batchElements data data.mean
            data.prec hyperparams is_mendelian k label_switch loglik
            logprior mcmc.chains mcmc.params modes mu nu.0 pi probz sigma2
            sigma2.0 tau2 theta z zfreq
    ```

*   checking Rd \usage sections ... WARNING
    ```
    ...
    
    Documented arguments not in \usage in documentation object 'iter<-':
      ‘force’
    
    Documented arguments not in \usage in documentation object 'mcmcParams':
      ‘force’
    
    Undocumented arguments in documentation object 'sigma<-'
      ‘value’
    
    Undocumented arguments in documentation object 'singleBatchGuided,MultiBatchList,MultiBatch-method'
      ‘x’ ‘guide’
    
    Undocumented arguments in documentation object 'theta'
      ‘value’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        R     3.0Mb
        doc   3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppArmadillo’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    findSurrogates,MultiBatch: no visible binding for global variable ‘id’
      (/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:127-133)
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘provisional_batch’
      (/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:127-133)
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘batch_labels’
      (/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:127-133)
    findSurrogates,MultiBatch: no visible binding for global variable ‘id’
      (/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-SummarizedExperiment.R:138-142)
    sigma,MultiBatchCopyNumberPooled: no visible binding for global
      variable ‘s2’
      (/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/CNPBayes/new/CNPBayes.Rcheck/00_pkg_src/CNPBayes/R/methods-MultiBatchPooled.R:161)
    Undefined global functions or variables:
      . .gibbs_trios_mcmc2 .gibbs_trios_mcmc3 := batch_index batch_labels
      batches bk copy_number father id log_ratio maplabel medians model
      mother mprob nhom parents prec provisional_batch s s2 snpdat spec
      spec<- t.test value
    Consider adding
      importFrom("stats", "t.test")
    to your NAMESPACE file.
    ```

# heatwaveR

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/heatwaveR
* URL: https://robwschlegel.github.io/heatwaveR/index.html, https://github.com/robwschlegel/heatwaveR
* BugReports: https://github.com/robwschlegel/heatwaveR/issues
* Date/Publication: 2019-09-09 20:30:03 UTC
* Number of recursive dependencies: 127

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
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/heatwaveR/new/heatwaveR.Rcheck/heatwaveR’

```
### CRAN

```
* installing *source* package ‘heatwaveR’ ...
** package ‘heatwaveR’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/heatwaveR/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘heatwaveR’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/heatwaveR/old/heatwaveR.Rcheck/heatwaveR’

```
# martini

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/martini
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 115

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
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DAS_GINLIB -DAS_RGINLIB -fopenmp -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/Rgin/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘martini’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/martini/new/martini.Rcheck/martini’

```
### CRAN

```
* installing *source* package ‘martini’ ...
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -DAS_GINLIB -DAS_RGINLIB -fopenmp -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/Rgin/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/martini/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
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
* Number of recursive dependencies: 118

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
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c image_helper.cpp -o image_helper.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c middle_frame.cpp -o middle_frame.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c speckle_acf.cpp -o speckle_acf.o
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
** using staged installation
** libs
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c image_helper.cpp -o image_helper.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c middle_frame.cpp -o middle_frame.o
ccache clang++ -Qunused-arguments  -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c speckle_acf.cpp -o speckle_acf.o
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

* Version: 1.1.1
* Source code: https://github.com/cran/updog
* BugReports: http://github.com/dcgerard/updog/issues
* Date/Publication: 2019-09-09 23:30:02 UTC
* Number of recursive dependencies: 115

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
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/updog/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘updog’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/updog/new/updog.Rcheck/updog’

```
### CRAN

```
* installing *source* package ‘updog’ ...
** package ‘updog’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/updog/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
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
* Number of recursive dependencies: 108

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
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/new/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/weibulltools/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘weibulltools’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/weibulltools/new/weibulltools.Rcheck/weibulltools’

```
### CRAN

```
* installing *source* package ‘weibulltools’ ...
** package ‘weibulltools’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/tidyverse/old/Rcpp/include" -I"/Users/hadley/Documents/tidyverse/tidyverse/revdep/library.noindex/weibulltools/RcppArmadillo/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include -fopenmp  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘weibulltools’
* removing ‘/Users/hadley/Documents/tidyverse/tidyverse/revdep/checks.noindex/weibulltools/old/weibulltools.Rcheck/weibulltools’

```
