---
date: "2022-02-12"
title: "Class 1"
menu:
  example:
    parent: Labs
weight: 1
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:text/x-markdown;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyAxIgotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQpgYGAKCiMjIEludHJvZHVjdGlvbgoKRm9yIHRoaXMgY2xhc3MsIHdlJ2xsIHJldmlldyBjb2RlIGV4YW1wbGVzIGZvdW5kIGluIHRoZSBQcmVmYWNlLgoKVGhpcyBhc3N1bWVzIHRoYXQgeW91IGhhdmUgYWxyZWFkeSBpbnN0YWxsZWQgdGhlIGByZXRoaW5raW5nYCBwYWNrYWdlLgoKSWYgeW91IG5lZWQgaGVscCwgYmUgc3VyZSB0byByZW1lbWJlciB0aGUgcmVmZXJlbmNlcyBpbiB0aGUgW1Jlc291cmNlc10oL3Jlc291cmNlLyk6CgoqIFtJbnN0YWxsaW5nIFIvUlN0dWRpb10oL3Jlc291cmNlL2luc3RhbGwvKQoqIFtJbnN0YWxsaW5nIGByZXRoaW5raW5nYCBwYWNrYWdlXSgvcmVzb3VyY2UvaW5zdGFsbC1yZXRoaW5raW5nLykKKiBbUm1hcmtkb3duXSgvcmVzb3VyY2Uvcm1hcmtkb3duLykKKiBbUiBTdHlsZSBndWlkZV0oL3Jlc291cmNlL3N0eWxlLykKCiMjIyBSIFByaW5jaXBsZXMKCmBgYHtyfQojIyBSIGNvZGUgMC4xCnByaW50KCAiQWxsIG1vZGVscyBhcmUgd3JvbmcsIGJ1dCBzb21lIGFyZSB1c2VmdWwuIiApCmBgYAoKYGBge3J9CiMjIFIgY29kZSAwLjIKeCA8LSAxOjIKeCA8LSB4KjEwCnggPC0gbG9nKHgpCnggPC0gc3VtKHgpCnggPC0gZXhwKHgpCngKYGBgCgpgYGB7cn0KIyMgUiBjb2RlIDAuMwooIGxvZyggMC4wMV4yMDAgKSApCiggMjAwICogbG9nKDAuMDEpICkKYGBgCgpgYGB7cn0KIyMgUiBjb2RlIDAuNAojIExvYWQgdGhlIGRhdGE6CiMgY2FyIGJyYWtpbmcgZGlzdGFuY2VzIGluIGZlZXQgcGFpcmVkIHdpdGggc3BlZWRzIGluIGttL2gKIyBzZWUgP2NhcnMgZm9yIGRldGFpbHMKZGF0YShjYXJzKQpgYGAKCmBgYHtyfQojIGZpdCBhIGxpbmVhciByZWdyZXNzaW9uIG9mIGRpc3RhbmNlIG9uIHNwZWVkCm0gPC0gbG0oIGRpc3QgfiBzcGVlZCAsIGRhdGE9Y2FycyApCgojIGVzdGltYXRlZCBjb2VmZmljaWVudHMgZnJvbSB0aGUgbW9kZWwKY29lZihtKQpgYGAKCmBgYHtyfQojIHBsb3QgcmVzaWR1YWxzIGFnYWluc3Qgc3BlZWQKcGxvdCggcmVzaWQobSkgfiBzcGVlZCAsIGRhdGE9Y2FycyApCmBgYAoKIyMgUGFja2FnZSB2ZXJzaW9ucwoKYGBge3J9CnNlc3Npb25JbmZvKCkKYGBg" download="01-class.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010_examples" target="_blank"><img src="https://gitpod.io/button/open-in-gitpod.svg" style="display: block; margin: auto auto auto 0;" /></a>

## Introduction

For this class, we’ll review code examples found in the Preface.

This assumes that you have already installed the `rethinking` package.

If you need help, be sure to remember the references in the [Resources](/resource/):

-   [Installing R/RStudio](/resource/install/)
-   [Installing `rethinking` package](/resource/install-rethinking/)
-   [Rmarkdown](/resource/rmarkdown/)
-   [R Style guide](/resource/style/)

### R Principles

``` r
## R code 0.1
print( "All models are wrong, but some are useful." )
```

``` language-r
## [1] "All models are wrong, but some are useful."
```

``` r
## R code 0.2
x <- 1:2
x <- x*10
x <- log(x)
x <- sum(x)
x <- exp(x)
x
```

``` language-r
## [1] 200
```

``` r
## R code 0.3
( log( 0.01^200 ) )
```

``` language-r
## [1] -Inf
```

``` r
( 200 * log(0.01) )
```

``` language-r
## [1] -921.034
```

``` r
## R code 0.4
# Load the data:
# car braking distances in feet paired with speeds in km/h
# see ?cars for details
data(cars)
```

``` r
# fit a linear regression of distance on speed
m <- lm( dist ~ speed , data=cars )

# estimated coefficients from the model
coef(m)
```

``` language-r
## (Intercept)       speed 
##  -17.579095    3.932409
```

``` r
# plot residuals against speed
plot( resid(m) ~ speed , data=cars )
```

<img src="/lab/01-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

## Package versions

``` r
sessionInfo()
```

``` language-r
## R version 4.1.1 (2021-08-10)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Monterey 12.1
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices datasets  utils     methods  
## [8] base     
## 
## other attached packages:
## [1] rethinking_2.21      cmdstanr_0.4.0.9001  rstan_2.21.3        
## [4] ggplot2_3.3.5        StanHeaders_2.21.0-7
## 
## loaded via a namespace (and not attached):
##  [1] sass_0.4.0           jsonlite_1.7.2       bslib_0.3.1         
##  [4] RcppParallel_5.1.4   assertthat_0.2.1     posterior_1.1.0     
##  [7] distributional_0.2.2 highr_0.9            stats4_4.1.1        
## [10] tensorA_0.36.2       renv_0.14.0          yaml_2.2.1          
## [13] pillar_1.6.4         backports_1.4.1      lattice_0.20-44     
## [16] glue_1.6.0           uuid_1.0-3           digest_0.6.29       
## [19] checkmate_2.0.0      colorspace_2.0-2     htmltools_0.5.2     
## [22] pkgconfig_2.0.3      bookdown_0.24        purrr_0.3.4         
## [25] mvtnorm_1.1-3        scales_1.1.1         processx_3.5.2      
## [28] tibble_3.1.6         generics_0.1.1       farver_2.1.0        
## [31] ellipsis_0.3.2       withr_2.4.3          cli_3.1.0           
## [34] mime_0.12            magrittr_2.0.1       crayon_1.4.2        
## [37] evaluate_0.14        ps_1.6.0             fs_1.5.0            
## [40] fansi_0.5.0          MASS_7.3-54          pkgbuild_1.3.1      
## [43] blogdown_1.5         tools_4.1.1          loo_2.4.1           
## [46] prettyunits_1.1.1    lifecycle_1.0.1      matrixStats_0.61.0  
## [49] stringr_1.4.0        munsell_0.5.0        callr_3.7.0         
## [52] compiler_4.1.1       jquerylib_0.1.4      rlang_0.4.12        
## [55] grid_4.1.1           rstudioapi_0.13      base64enc_0.1-3     
## [58] rmarkdown_2.11       xaringanExtra_0.5.5  gtable_0.3.0        
## [61] codetools_0.2-18     inline_0.3.19        abind_1.4-5         
## [64] DBI_1.1.1            R6_2.5.1             gridExtra_2.3       
## [67] lubridate_1.8.0      knitr_1.36           dplyr_1.0.7         
## [70] fastmap_1.1.0        utf8_1.2.2           downloadthis_0.2.1  
## [73] bsplus_0.1.3         shape_1.4.6          stringi_1.7.6       
## [76] Rcpp_1.0.7           vctrs_0.3.8          tidyselect_1.1.1    
## [79] xfun_0.28            coda_0.19-4
```
