---
date: "2022-01-27"
title: "Class 4"
menu:
  example:
    parent: Examples
weight: 4
toc: true
type: docs
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:text/x-markdown;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyA0IgotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQpgYGAKCiMjIEludHJvZHVjdGlvbgoKRm9yIHRoaXMgY2xhc3MsIHdlJ2xsIHJldmlldyBjb2RlIGV4YW1wbGVzIGZvdW5kIGluIENoYXB0ZXIgNCBhbmQgc29tZSBvZiBDaGFwdGVyIDUuCgojIyBDaGFwdGVyIDQgCgpgYGB7cn0KbGlicmFyeShyZXRoaW5raW5nKQpkYXRhKCJIb3dlbGwxIikKZCA8LSBIb3dlbGwxW0hvd2VsbDEkYWdlPj0xOCxdCmBgYAoKTGV0J3MgYWdhaW4gY29uc2lkZXIgdGhlIDE4KyB5ZWFyIG9sZCBmcm9tIHRoZSBIb3dlbGwgZGF0YXNldCwgYnV0IG5vdyBsb29rIGF0IHRoZSByb2xlIG9mIGEgdGhpcmQgdmFyaWFibGU6IHNleC4KCmBgYHtyfQpwbG90KGQkaGVpZ2h0LCBkJHdlaWdodCwgY29sID0gaWZlbHNlKGQkbWFsZSw0LDIpLCAgeGxhYiA9ICJoZWlnaHQgKGNtKSIsIHlsYWIgPSAid2VpZ2h0IChrZykiLCAgbHdkPTMpCmxlZ2VuZCgxMzgsIDYzLCBsZWdlbmQ9YygiRmVtYWxlIiwgIk1hbGUiKSwKICAgICAgIGNvbD1jKDIsNCksIGx0eT0xOjEsIGNleD0wLjgpCmBgYAoKYGBge3J9CiMgbmV3IGhlaWdodCwgd2VpZ2h0LCBzZXggY2F0ZWdvcmljYWwgdmFyaWFibGUgZXhhbXBsZQpkZW5zKGQkaGVpZ2h0W2QkbWFsZT09MV0sbHdkPTMsY29sPTQseGxhYj0iaGVpZ2h0IChjbSkiKQpkZW5zKGQkaGVpZ2h0W2QkbWFsZT09MF0sbHdkPTMsY29sPTIsYWRkPVRSVUUpCmBgYAoKYGBge3J9CmRlbnMoZCR3ZWlnaHRbZCRtYWxlPT0xXSxsd2Q9Myxjb2w9NCx4bGFiPSJ3ZWlnaHQgKGtnKSIpCmRlbnMoZCR3ZWlnaHRbZCRtYWxlPT0wXSxsd2Q9Myxjb2w9MixhZGQ9VFJVRSkKYGBgCgpgYGB7cn0KIyBXIH4gUwpkYXQgPC0gbGlzdCgKICAgIFcgPSBkJHdlaWdodCwKICAgIFMgPSBkJG1hbGUgKyAxICkgIyBTPTEgZmVtYWxlLCBTPTIgbWFsZQoKbV9TVyA8LSBxdWFwKAogICAgYWxpc3QoCiAgICAgICAgVyB+IGRub3JtKG11LHNpZ21hKSwKICAgICAgICBtdSA8LSBhW1NdLAogICAgICAgIGFbU10gfiBkbm9ybSg2MCwxMCksCiAgICAgICAgc2lnbWEgfiBkdW5pZigwLDEwKQogICAgKSwgZGF0YT1kYXQgKQpgYGAKCmBgYHtyfQojIHBvc3RlcmlvciBtZWFucwpwb3N0IDwtIGV4dHJhY3Quc2FtcGxlcyhtX1NXKQpkZW5zKCBwb3N0JGFbLDFdICwgeGxpbT1jKDM5LDUwKSAsIGx3ZD0zICwgY29sPTIgLCB4bGFiPSJwb3N0ZXJpb3IgbWVhbiB3ZWlnaHQgKGtnKSIgKQpkZW5zKCBwb3N0JGFbLDJdICwgbHdkPTMgLCBjb2w9NCAsIGFkZD1UUlVFICkKYGBgCgpgYGB7cn0KIyBwb3N0ZXJpb3IgVyBkaXN0cmlidXRpb25zClcxIDwtIHJub3JtKCAxMDAwICwgcG9zdCRhWywxXSAsIHBvc3Qkc2lnbWEgKQpXMiA8LSBybm9ybSggMTAwMCAsIHBvc3QkYVssMl0gLCBwb3N0JHNpZ21hICkKZGVucyggVzEgLCB4bGltPWMoMjAsNzApICwgeWxpbT1jKDAsMC4wODUpICwgbHdkPTMgLCBjb2w9MiAsIHhsYWI9InBvc3RlcmlvciBwcmVkaWN0ZWQgd2VpZ2h0IChrZykiICkKZGVucyggVzIgLCBsd2Q9MyAsIGNvbD00ICwgYWRkPVRSVUUgKQpgYGAKCmBgYHtyfQojIGNhdXNhbCBjb250cmFzdCAoaW4gbWVhbnMpCm11X2NvbnRyYXN0IDwtIHBvc3QkYVssMl0gLSBwb3N0JGFbLDFdCmRlbnMoIG11X2NvbnRyYXN0ICwgeGxpbT1jKDMsMTApICwgbHdkPTMgLCBjb2w9MSAsIHhsYWI9InBvc3RlcmlvciBtZWFuIHdlaWdodCBjb250cmFzdCAoa2cpIiApCmBgYAoKYGBge3J9CiMgVyBjb250cmFzdApXX2NvbnRyYXN0IDwtIFcyIC0gVzEKZGVucyggV19jb250cmFzdCAsIHhsaW09YygtMjUsMzUpICwgbHdkPTMgLCBjb2w9MSAsIHhsYWI9InBvc3RlcmlvciB3ZWlnaHQgY29udHJhc3QgKGtnKSIgKQoKV2RlbnMgPC0gZGVuc2l0eShXX2NvbnRyYXN0LGFkaj0wLjUpCnBvbHlnb24oYyhXZGVucyR4W1dkZW5zJHg+MF0sIG1heChXZGVucyR4KSwgMCksIGMoV2RlbnMkeVtXZGVucyR4PjBdLCAwLCAwKSwgY29sID0gNCwgYm9yZGVyID0gTkEgKQpwb2x5Z29uKGMoV2RlbnMkeFtXZGVucyR4PDBdLCAwLCBtaW4oV2RlbnMkeCkpLCBjKFdkZW5zJHlbV2RlbnMkeDwwXSwgMCwgMCksIGNvbCA9IDIsIGJvcmRlciA9IE5BICkKYGBgCgpgYGB7cn0KIyBwcm9wb3J0aW9uIGFib3ZlIHplcm8Kc3VtKCBXX2NvbnRyYXN0ID4gMCApIC8gMTAwMAojIHByb3BvcnRpb24gYmVsb3cgemVybwpzdW0oIFdfY29udHJhc3QgPCAwICkgLyAxMDAwCmBgYAoKYGBge3J9CiMgVyB+IFMgKyBICmRhdCA8LSBsaXN0KAogICAgVyA9IGQkd2VpZ2h0LAogICAgSCA9IGQkaGVpZ2h0LAogICAgSGJhciA9IG1lYW4oZCRoZWlnaHQpLAogICAgUyA9IGQkbWFsZSArIDEgKSAjIFM9MSBmZW1hbGUsIFM9MiBtYWxlCgptX1NIVyA8LSBxdWFwKAogICAgYWxpc3QoCiAgICAgICAgVyB+IGRub3JtKG11LHNpZ21hKSwKICAgICAgICBtdSA8LSBhW1NdICsgYltTXSooSC1IYmFyKSwKICAgICAgICBhW1NdIH4gZG5vcm0oNjAsMTApLAogICAgICAgIGJbU10gfiBkbG5vcm0oMCwxKSwKICAgICAgICBzaWdtYSB+IGR1bmlmKDAsMTApCiAgICApLCBkYXRhPWRhdCApCmBgYCAgICAKCmBgYHtyfQojIGZ1bGwgc3lzdGVtIGFzIFNDTQpkYXQgPC0gbGlzdCgKICAgIFcgPSBkJHdlaWdodCwKICAgIEggPSBkJGhlaWdodCwKICAgIEhiYXIgPSBtZWFuKGQkaGVpZ2h0KSwKICAgIFMgPSBkJG1hbGUgKyAxICkgIyBTPTEgZmVtYWxlLCBTPTIgbWFsZQoKbV9TSFdfZnVsbCA8LSBxdWFwKAogICAgYWxpc3QoCgogICAgICAgICMgd2VpZ2h0CiAgICAgICAgVyB+IGRub3JtKG11LHNpZ21hKSwKICAgICAgICBtdSA8LSBhW1NdICsgYltTXSooSC1IYmFyKSwKICAgICAgICBhW1NdIH4gZG5vcm0oNjAsMTApLAogICAgICAgIGJbU10gfiBkbG5vcm0oMCwxKSwKICAgICAgICBzaWdtYSB+IGR1bmlmKDAsMTApLAoKICAgICAgICAjIGhlaWdodAogICAgICAgIEggfiBkbm9ybShudSx0YXUpLAogICAgICAgIG51IDwtIGhbU10sCiAgICAgICAgaFtTXSB+IGRub3JtKDE2MCwxMCksCiAgICAgICAgdGF1IH4gZHVuaWYoMCwxMCkKCiAgICApLCBkYXRhPWRhdCApCmBgYAoKYGBge3J9CiMgY29tcHV0ZSB0b3RhbCBjYXVzYWwgZWZmZWN0IG9mIFMgb24gVwpwb3N0IDwtIGV4dHJhY3Quc2FtcGxlcyhtX1NIV19mdWxsKQpIYmFyIDwtIGRhdCRIYmFyCm4gPC0gMWU0Cgp3aXRoKCBwb3N0ICwgewojIHNpbXVsYXRlIFcgZm9yIFM9MQpIX1MxIDwtIHJub3JtKG4sIGhbLDFdICwgdGF1ICkKV19TMSA8LSBybm9ybShuLCBhWywxXSArIGJbLDFdKihIX1MxLUhiYXIpICwgc2lnbWEpCiMgc2ltdWxhdGUgVyBmb3IgUz0yCkhfUzIgPC0gcm5vcm0obiwgaFssMl0gLCB0YXUpCldfUzIgPC0gcm5vcm0obiwgYVssMl0gKyBiWywyXSooSF9TMi1IYmFyKSAsIHNpZ21hKQojIGNvbXB1dGUgY29udHJhc3QKV19kb19TIDw8LSBXX1MyIC0gV19TMQojIGlmIHlvdSB3YW50IHRvIGxlYXJuIDw8LSAoc2NvcGluZyBhc3NpZ25tZW50KQojaHR0cHM6Ly9zdGFja292ZXJmbG93LmNvbS9xdWVzdGlvbnMvMjYyODYyMS9ob3ctZG8teW91LXVzZS1zY29waW5nLWFzc2lnbm1lbnQtaW4tcgp9KQpgYGAKCmBgYHtyfQojIGF1dG9tYXRlZCB3YXkKSFdzaW0gPC0gc2ltKG1fU0hXX2Z1bGwsCiAgICAgICAgICAgICBkYXRhPWxpc3QoUz1jKDEsMikpLAogICAgICAgICAgICAgdmFycz1jKCJIIiwiVyIpKQpXX2RvX1NfYXV0byA8LSBIV3NpbSRXWywyXSAtIEhXc2ltJFdbLDFdCmBgYAoKCiMjIFBhY2thZ2UgdmVyc2lvbnMKCmBgYHtyfQpzZXNzaW9uSW5mbygpCmBgYA==" download="04-class.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010_examples" target="_blank"><img src="https://gitpod.io/button/open-in-gitpod.svg" style="display: block; margin: auto auto auto 0;" /></a>

## Introduction

For this class, we’ll review code examples found in Chapter 4 and some of Chapter 5.

## Chapter 4

``` r
set.seed(100) # fyi, in code seed wasn't set so may be slightly different
library(rethinking)
data("Howell1")
d <- Howell1[Howell1$age>=18,]
```

Let’s again consider the 18+ year old from the Howell dataset, but now look at the role of a third variable: sex.

``` r
plot(d$height, d$weight, col = ifelse(d$male,4,2),  xlab = "height (cm)", ylab = "weight (kg)",  lwd=3)
legend(138, 63, legend=c("Female", "Male"),
       col=c(2,4), lty=1:1, cex=0.8)
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-4-1.png" width="672" />

``` r
# new height, weight, sex categorical variable example
dens(d$height[d$male==1],lwd=3,col=4,xlab="height (cm)")
dens(d$height[d$male==0],lwd=3,col=2,add=TRUE)
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-5-1.png" width="672" />

``` r
dens(d$weight[d$male==1],lwd=3,col=4,xlab="weight (kg)")
dens(d$weight[d$male==0],lwd=3,col=2,add=TRUE)
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-6-1.png" width="672" />

### Causal effect of S on W?

``` r
# W ~ S
dat <- list(
    W = d$weight,
    S = d$male + 1 ) # S=1 female, S=2 male

m_SW <- quap(
    alist(
        W ~ dnorm(mu,sigma),
        mu <- a[S],
        a[S] ~ dnorm(60,10),
        sigma ~ dunif(0,10)
    ), data=dat )
```

``` r
# posterior means
post <- extract.samples(m_SW)
dens( post$a[,1] , xlim=c(39,50) , lwd=3 , col=2 , xlab="posterior mean weight (kg)" )
dens( post$a[,2] , lwd=3 , col=4 , add=TRUE )
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

``` r
# posterior W distributions
W1 <- rnorm( 1000 , post$a[,1] , post$sigma )
W2 <- rnorm( 1000 , post$a[,2] , post$sigma )
dens( W1 , xlim=c(20,70) , ylim=c(0,0.085) , lwd=3 , col=2 , xlab="posterior predicted weight (kg)" )
dens( W2 , lwd=3 , col=4 , add=TRUE )
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-9-1.png" width="672" />

``` r
# causal contrast (in means)
mu_contrast <- post$a[,2] - post$a[,1]
dens( mu_contrast , xlim=c(3,10) , lwd=3 , col=1 , xlab="posterior mean weight contrast (kg)" )
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-10-1.png" width="672" />

``` r
# W contrast
W_contrast <- W2 - W1
dens( W_contrast , xlim=c(-25,35) , lwd=3 , col=1 , xlab="posterior weight contrast (kg)" )

Wdens <- density(W_contrast,adj=0.5)
polygon(c(Wdens$x[Wdens$x>0], max(Wdens$x), 0), c(Wdens$y[Wdens$x>0], 0, 0), col = 4, border = NA )
polygon(c(Wdens$x[Wdens$x<0], 0, min(Wdens$x)), c(Wdens$y[Wdens$x<0], 0, 0), col = 2, border = NA )
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-11-1.png" width="672" />

``` r
# proportion above zero
sum( W_contrast > 0 ) / 1000
```

``` language-r
## [1] 0.784
```

``` r
# proportion below zero
sum( W_contrast < 0 ) / 1000
```

``` language-r
## [1] 0.216
```

### Direct causal effect of S on W?

About minute 38 in Lecture 4:

``` r
# W ~ S + H
dat <- list(
    W = d$weight,
    H = d$height,
    Hbar = mean(d$height),
    S = d$male + 1 ) # S=1 female, S=2 male

m_SHW <- quap(
    alist(
        W ~ dnorm(mu,sigma),
        mu <- a[S] + b[S]*(H-Hbar),
        a[S] ~ dnorm(60,10),
        b[S] ~ dlnorm(0,1),
        sigma ~ dunif(0,10)
    ), data=dat )
```

Let’s now get the posterior predictives for the contrasts.

``` r
xseq <- seq(from=130,to=190,len=50)

muF <- link(m_SHW,data=list(S=rep(1,50),H=xseq,Hbar=mean(d$height)))
muM <- link(m_SHW,data=list(S=rep(2,50),H=xseq,Hbar=mean(d$height)))
mu_contrast <- muF - muM
plot( NULL, xlim=range(xseq) , ylim=c(-6,8) , xlab = "height (cm)", ylab = "weight contrast (F-M)")
for ( p in c(0.5,0.6,0.7,0.8,0.9,0.99))
  shade( apply(mu_contrast,2,PI,prob=p), xseq)
abline(h=0,lty=2)
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-14-1.png" width="672" />

### Full Luxury Bayes

In this setup, we’ll run height and weight simulatenously.

``` r
# full system as SCM
dat <- list(
    W = d$weight,
    H = d$height,
    Hbar = mean(d$height),
    S = d$male + 1 ) # S=1 female, S=2 male

m_SHW_full <- quap(
    alist(

        # weight
        W ~ dnorm(mu,sigma),
        mu <- a[S] + b[S]*(H-Hbar),
        a[S] ~ dnorm(60,10),
        b[S] ~ dlnorm(0,1),
        sigma ~ dunif(0,10),

        # height
        H ~ dnorm(nu,tau),
        nu <- h[S],
        h[S] ~ dnorm(160,10),
        tau ~ dunif(0,10)

    ), data=dat )
```

We’ll simulate 1000 synthetic women in order. We focus on height first since it’s a function of weight. Then simulate weights by using the simulation heights. Then repeat for 1000 synthetic men in the similar order.

``` r
# compute total causal effect of S on W
post <- extract.samples(m_SHW_full)
Hbar <- dat$Hbar
n <- 1e4

with( post , {
# simulate W for S=1
  H_S1 <- rnorm(n, h[,1] , tau )
  W_S1 <- rnorm(n, a[,1] + b[,1]*(H_S1-Hbar) , sigma)
# simulate W for S=2
  H_S2 <- rnorm(n, h[,2] , tau)
  W_S2 <- rnorm(n, a[,2] + b[,2]*(H_S2-Hbar) , sigma)
# compute contrast (do operator); should hold results from intervening in sex
  W_do_S <<- W_S2 - W_S1
# <<- (scoping assignment)
#https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r
})
```

``` r
dens( W_do_S , xlim=c(-25,35) , lwd=3 , col=1 , xlab="posterior weight contrast (kg)" )

Wdens <- density(W_do_S,adj=0.5)
polygon(c(Wdens$x[Wdens$x>0], max(Wdens$x), 0), c(Wdens$y[Wdens$x>0], 0, 0), col = 4, border = NA )
polygon(c(Wdens$x[Wdens$x<0], 0, min(Wdens$x)), c(Wdens$y[Wdens$x<0], 0, 0), col = 2, border = NA )
```

<img src="/example/04-class_files/figure-html/unnamed-chunk-17-1.png" width="672" />

``` r
# automated way
HWsim <- sim(m_SHW_full,
             data=list(S=c(1,2)),
             vars=c("H","W"))
W_do_S_auto <- HWsim$W[,2] - HWsim$W[,1]
```

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
