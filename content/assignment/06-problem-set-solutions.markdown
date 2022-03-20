---
title: Problem Set 6
date: "2022-03-20"
menu:
  assignment:
    parent: Problem sets
    weight: 6
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is **optional** and not necessary to be submitted. We will try to work on it in class on March 14.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA2CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgojIyBRdWVzdGlvbiAxCgpUaGUgZGF0YSBpbiBgZGF0YShOV09HcmFudHMpYCBhcmUgb3V0Y29tZXMgZm9yIHNjaWVudGlmaWMgZnVuZGluZyBhcHBsaWNhdGlvbnMgZm9yIHRoZSBOZXRoZXJsYW5kcyBPcmdhbml6YXRpb24gZm9yIFNjaWVudGlmaWMgUmVzZWFyY2ggKE5XTykgZnJvbSAyMDEwLTIwMTIuIFRoZXNlIGRhdGEgaGF2ZSBhIHZlcnkgc2ltaWxhciBzdHJ1Y3R1cmUgdG8gdGhlIFVDQkFkbWl0IGRhdGEgZGlzY3Vzc2VkIGluIENoYXB0ZXIgMTEuIERyYXcgYSBEQUcgZm9yIHRoaXMgc2FtcGxlIGFuZCB0aGVuIHVzZSBvbmUgb3IgbW9yZSBiaW5vbWlhbCBHTE1zIHRvIGVzdGltYXRlIHRoZSAqKlRPVEFMKiogY2F1c2FsIGVmZmVjdCBvZiBnZW5kZXIgb24gZ3JhbnQgYXdhcmRzLgoKYGBge3J9CmxpYnJhcnkocmV0aGlua2luZykKZGF0YShOV09HcmFudHMpCmQgPC0gTldPR3JhbnRzCgpoZWFkKGQpCmBgYApMZXQncyBwbG90IHRoZSBEQUcuIAoKYGBge3J9CmxpYnJhcnkoZGFnaXR0eSkKCmcgPC0gZGFnaXR0eSgnZGFnIHsKYmI9IjAsMCwxLDEiCkcgW3Bvcz0iMC4yNTEsMC40ODEiXQpEIFtwb3M9IjAuMjUxLDAuMzUyIl0KQSBbcG9zPSIwLjQ4MSwwLjM1MiJdCkcgLT4gRApHIC0+IEEKRCAtPiBBCn0KJykKcGxvdChnKQpgYGAKClNpbmNlIERlcGFydG1lbnQgaXMgYSBtZWRpYXRvciwgd2Ugd291bGQgKipub3QqKiBjb25kaXRpb24gYnkgaXQgdG8gZmluZCB0aGUgdG90YWwgY2F1c2FsIGVmZmVjdCBvZiBnZW5kZXIgb24gYXdhcmRzLgoKYGBge3J9CmRhdCA8LSBsaXN0KAogICAgQSA9IGFzLmludGVnZXIoZCRhd2FyZHMpLAogICAgTiA9IGFzLmludGVnZXIoZCRhcHBsaWNhdGlvbnMpLAogICAgRyA9IGlmZWxzZSggZCRnZW5kZXI9PSJmIiAsIDFMICwgMkwgKSAsCiAgICBEID0gYXMuaW50ZWdlcihkJGRpc2NpcGxpbmUpIAopCgptMSA8LSB1bGFtKAogIGFsaXN0KAogICAgQSB+IGRiaW5vbSggTiwgcCApICwKICAgIGxvZ2l0KHApIDwtIGFbR10gLAogICAgYVtHXSB+IGRub3JtKDAsIDEuNSkKICApICwgZGF0YSA9IGRhdCwgY2hhaW5zID0gNAopCgpwcmVjaXMobTEsIGRlcHRoID0gMikKYGBgCgpCZWZvcmUgbG9va2luZyBhdCB0aGUgY29udHJhc3RzIChjYXVzYWwgZWZmZWN0KSwgd2UnbGwgbG9vayBhdCBjb252ZXJnZW5jZSBzdGF0aXN0aWNzLiBUaGUgUmhhdCdzIGFyZSAxIHdoaWNoIGFyZSBnb29kLiBMZXQncyBhbHNvIGxvb2sgYXQgdGhlIHRyYWNlIGFuZCB0cmFuayBwbG90cy4KCmBgYHtyfQp0cmFjZXBsb3QobTEpCmBgYAoKYGBge3J9CnRyYW5rcGxvdChtMSkKYGBgCgpUaGVyZWZvcmUsIG91ciBtb2RlbCBoYXMgYXBwcm9wcmlhdGUgY29udmVyZ2VuY2Ugc3RhdGlzdGljcy4KCldlJ2xsIG5lZWQgdG8gY2FsY3VsYXRlIHRoZSAqKmNvbnRyYXN0cyoqIHRvIG1ha2UgYSBkZXRlcm1pbmF0aW9uIG9mIHRoZSB0b3RhbCBjYXVzYWwgZWZmZWN0IG9mIGdlbmRlciBvbiBhd2FyZHMuCgpgYGB7cn0KcG9zdDEgPC0gZXh0cmFjdC5zYW1wbGVzKG0xKQpwb3N0MSRwR2YgPC0gaW52X2xvZ2l0KHBvc3QxJGFbLDFdKQpwb3N0MSRwR20gPC0gaW52X2xvZ2l0KHBvc3QxJGFbLDJdKQpwb3N0MSRHX2NvbnRyYXN0IDwtIHBvc3QxJHBHZiAtIHBvc3QxJHBHbQoKZGVucyggcG9zdDEkR19jb250cmFzdCwgbHdkPTQsIGNvbD0yLCB4bGFiPSJGLU0gY29udHJhc3QgKHRvdGFsKSIpCmFibGluZSggdj0wLCBsdHk9MykKYGBgCgpXZSBmaW5kIGFib3V0IGEgMyUgZGlmZmVyZW5jZSBvbiBhdmVyYWdlIGZvciBtZW4gb3ZlciB3b21lbi4gVGhpcyBjb3VsZCBiZSBzaWduaWZpY2FudC4KCldlIGNhbiBhbHNvIGRvIGEgcG9zdGVyaW9yIHByZWRpY3RpdmUgKHZhbGlkYXRpb24pIGNoZWNrIG9uIGVhY2ggb2YgdGhlIDE4IGluZGl2aWR1YWxzLgoKYGBge3J9CnBvc3RjaGVjayhtMSkKYGBgCgpGb3IgZWFjaCBpbmRpdmlkdWFsLCB0aGUgbW9kZWwgZml0cyBva2F5IGJ1dCBub3QgZ3JlYXQuIAoKIyMjIEFsdGVybmF0aXZlIGFwcHJvYWNoCgpZb3UgbWF5IGFsc28gdXNlIHRoZSBgYnJtc2AgcGFja2FnZSBmb3IgdGhpcyBtb2RlbCBhbG9uZyB3aXRoIGB0aWR5dmVyc2VgIGJhc2VkIHBhY2thZ2VzLgoKYGBge3J9CmxpYnJhcnkoYnJtcykKCm0xX2JybXMgPC0gYnJtKGF3YXJkcyB8IHRyaWFscyhhcHBsaWNhdGlvbnMpIH4gZ2VuZGVyLCBmYW1pbHkgPSBiaW5vbWlhbCgibG9naXQiKSwgcHJpb3IgPSBzZXRfcHJpb3IoIm5vcm1hbCgwLDEuNSkiLCBjbGFzcyA9ICJiIiksIGRhdGEgPSBkLCBjaGFpbnMgPSA0KQoKc3VtbWFyeShtMV9icm1zKQpgYGAKCgpgYGB7cn0KbGlicmFyeSh0aWR5YmF5ZXMpCmxpYnJhcnkobWFncml0dHIpICMgZm9yICU+JQpsaWJyYXJ5KGRwbHlyKQoKcG9zdGVyaW9yX2RyYXdzIDwtIG0xX2JybXMgJT4lCiAgc3ByZWFkX2RyYXdzKGJfZ2VuZGVybSxiX0ludGVyY2VwdCkgJT4lICMgc2FtcGxlL2RyYXcgcG9zdGVyaW9ycwogIG11dGF0ZShiX00gPSBiX0ludGVyY2VwdCArIGJfZ2VuZGVybSkgJT4lICMgZHVlIHRvIGR1bW15IHZhcmlhYmxlcwogIG11dGF0ZShDb250cmFzdCA9IGludl9sb2dpdChiX0ludGVyY2VwdCkgLSBpbnZfbG9naXQoYl9NKSkKCmdncGxvdChwb3N0ZXJpb3JfZHJhd3MsIGFlcyh4ID0gQ29udHJhc3QpKSArCiAgZ2VvbV9kZW5zaXR5KCkgKwogIGxhYnMoeCA9ICJGLU0gY29udHJhc3QgKHRvdGFsKSIpCmBgYAoKV2UgY2FuIGNoZWNrIHRoYXQgdGhlIHBvc3RlcmlvciBjb250cmFzdHMgZnJvbSBib3RoIGFwcHJvYWNoZXMgYXJlIHNpbWlsYXIuCgpgYGB7cn0KbWVhbihwb3N0MSRHX2NvbnRyYXN0KQpgYGAKCmBgYHtyfQptZWFuKHBvc3Rlcmlvcl9kcmF3cyRDb250cmFzdCkKYGBgCgpPbmUgbmljZSB0aGluZyBhYm91dCB1c2luZyBgYnJtc2AgaXMgdGhlcmUgYXJlIG1hbnkgb3RoZXIgaGVscGZ1bCBwYWNrYWdlcy4gRm9yIGV4YW1wbGUsIHdlIGNhbiBlYXNpbHkgY2FsY3VsYXRlIHBvc3RlcmlvciBwcmVkaWN0aXZlIGNoZWNrczoKCmBgYHtyfQpiYXllc3Bsb3Q6OnBwX2NoZWNrKG0xX2JybXMpCmBgYAoKIyMgUXVlc3Rpb24gMgoKTm93IGVzdGltYXRlIHRoZSAqKkRJUkVDVCoqIGNhdXNhbCBlZmZlY3Qgb2YgZ2VuZGVyIG9uIGdyYW50IGF3YXJkcy4gQ29tcGFyZSB0aGUgYXZlcmFnZSBkaXJlY3QgY2F1c2FsIGVmZmVjdCBvZiBnZW5kZXIsIHdlaWdodGluZyBlYWNoIGRpc2NpcGxpbmUgaW4gcHJvcG9ydGlvbiB0byB0aGUgbnVtYmVyIG9mIGFwcGxpY2F0aW9ucyBpbiB0aGUgc2FtcGxlLiBSZWZlciB0byB0aGUgbWFyZ2luYWwgZWZmZWN0IGV4YW1wbGUgaW4gTGVjdHVyZSA5IGZvciBoZWxwLgoKYGBge3J9Cm0yIDwtIHVsYW0oCiAgYWxpc3QoCiAgICBBIH4gZGJpbm9tKCBOLCBwICkgLAogICAgbG9naXQocCkgPC0gYVtHLERdLAogICAgbWF0cml4W0csRF06YSB+IG5vcm1hbCgwLCAxLjUpCiAgKSAsIGRhdGEgPSBkYXQsIGNoYWlucyA9IDQKKQoKIyBmeWk6IGFuIGlkZW50aWNhbCB3YXkgdG8gc3BlY2lmeSBwcmlvcnMgaW4gdWxhbQojIHdlIHVzZSBhYm92ZSBhcyBlYXNpZXIgZm9yIHBvc3QtbW9kZWxpbmcgY2FsY3VsYXRpb25zCiMgbG9naXQocCkgPC0gYVtHXSArIGRlbHRhW0RdICwKIyBhW0ddIH4gZG5vcm0oMCwgMS41KSAsCiMgZGVsdGFbRF0gfiBkbm9ybSgwLCAxLjUpCmBgYAoKYGBge3J9CnByZWNpcyhtMiwgZGVwdGggPSAyKQpgYGAKCkxldCdzIGNhbGN1bGF0ZSB0aGUgY29udHJhc3QgZm9yIGdlbmRlci4KCmBgYHtyfQp0b3RhbF9hcHBzIDwtIHN1bShkYXQkTikKYXBwc19wZXJfZGlzYyA8LSBzYXBwbHkoIDE6OSAsIGZ1bmN0aW9uKGkpIHN1bShkYXQkTltkYXQkRD09aV0pICkKCnBHMSA8LSBsaW5rKG0yLGRhdGE9bGlzdCgKICAgIEQ9cmVwKDE6OSx0aW1lcz1hcHBzX3Blcl9kaXNjKSwKICAgIE49cmVwKDEsdG90YWxfYXBwcyksCiAgICBHPXJlcCgxLHRvdGFsX2FwcHMpKSkKCnBHMiA8LSBsaW5rKG0yLGRhdGE9bGlzdCgKICAgIEQ9cmVwKDE6OSx0aW1lcz1hcHBzX3Blcl9kaXNjKSwKICAgIE49cmVwKDEsdG90YWxfYXBwcyksCiAgICBHPXJlcCgyLHRvdGFsX2FwcHMpKSkKCmRlbnMoIHBHMSAtIHBHMiAsIGx3ZD00ICwgY29sPTIgLCB4bGFiPSJGLU0gY29udHJhc3QgKG1hcmdpbmFsKSIgLCB4bGltPWMoLTAuMywwLjMpICkKYWJsaW5lKCB2PTAgLCBsdHk9MyApCmBgYAoKCmBgYHtyfQpwbG90KCBOVUxMICwgeGxpbT1jKC0wLjQsMC40KSAsIHlsaW09YygwLDE4KSAsIHhsYWI9IkYtTSBjb250cmFzdCBmb3IgZWFjaCBkaXNjaXBsaW5lIiAsIHlsYWI9IkRlbnNpdHkiICkKYWJsaW5lKCB2PTAgLCBsdHk9MyApCmRhdCRkaXNjIDwtIGFzLmNoYXJhY3RlcihkJGRpc2NpcGxpbmUpCmRpc2MgPC0gZGF0JGRpc2Nbb3JkZXIoZGF0JEQpXQpmb3IgKCBpIGluIDE6OSApIHsKICAgIHBHMURpIDwtIGxpbmsobTIsZGF0YT1saXN0KEQ9aSxOPTEsRz0xKSkKICAgIHBHMkRpIDwtIGxpbmsobTIsZGF0YT1saXN0KEQ9aSxOPTEsRz0yKSkKICAgIEdjb250IDwtIHBHMURpIC0gcEcyRGkKICAgIGRlbnMoIEdjb250ICwgYWRkPVRSVUUgLCBsd2Q9MyAsIGNvbD1pICkKICAgIHhsb2MgPC0gaWZlbHNlKCBtZWFuKEdjb250KSA8IDAgLCAtMC4zNSAsIDAuMzUgKQogICAgeHBvcyA8LSBpZmVsc2UoIG1lYW4oR2NvbnQpIDwgMCAsIDQgLCAyICkKICAgIHRleHQoIHhsb2MgKyAwLjUqbWVhbihHY29udCkgLCAxOC1pICwgZGlzY1syKmldICwgY29sPWkgLCBwb3M9eHBvcyAsIGZvbnQ9MiApCn0KYGBgCgojIyBRdWVzdGlvbiAzCgpDb25zaWRlcmluZyB0aGUgdG90YWwgZWZmZWN0IChwcm9ibGVtIDEpIGFuZCBkaXJlY3QgZWZmZWN0IChwcm9ibGVtIDIpIG9mIGdlbmRlciwgd2hhdCBjYXVzZXMgY29udHJpYnV0ZSB0byB0aGUgYXZlcmFnZSBkaWZmZXJlbmNlIGJldHdlZW4gd29tZW4gYW5kIG1lbiBpbiBhd2FyZCByYXRlIGluIHRoaXMgc2FtcGxlPyBJdCBpcyBub3QgbmVjZXNzYXJ5IHRvIHNheSB3aGV0aGVyIG9yIG5vdCB0aGVyZSBpcyBldmlkZW5jZSBvZiBkaXNjcmltaW5hdGlvbi4gU2ltcGx5IGV4cGxhaW4gaG93IHRoZSBkaXJlY3QgZWZmZWN0cyB5b3UgaGF2ZSBlc3RpbWF0ZWQgbWFrZSBzZW5zZSAob3Igbm90KSBvZiB0aGUgdG90YWwgZWZmZWN0LgoKYGBge3J9CnRvdGFsX2YgPC0gc3VtKGQkYXBwbGljYXRpb25zW2QkZ2VuZGVyPT0iZiJdKQpwRGYgPC0gZCRhcHBsaWNhdGlvbnNbZCRnZW5kZXI9PSJmIl0gLyB0b3RhbF9mCgp0b3RhbF9tIDwtIHN1bShkJGFwcGxpY2F0aW9uc1tkJGdlbmRlcj09Im0iXSkKcERtIDwtIGQkYXBwbGljYXRpb25zW2QkZ2VuZGVyPT0ibSJdIC8gdG90YWxfbQoKIyBvdmVyYWxsIGF3YXJkIHJhdGUgaW4gZWFjaCBkaXNjaXBsaW5lCm5fYXBwcyA8LSB4dGFicyggZGF0JE4gfiBkYXQkRCApCm5fYXdhcmRzIDwtIHh0YWJzKCBkYXQkQSB+IGRhdCREICkKcF9hd2FyZCA8LSBuX2F3YXJkcyAvIG5fYXBwcwoKIyBmL20gYXdhcmQgcmF0ZSBpbiBlYWNoIGRpc2NpcGxpbmUKcG9zdDIgPC0gZXh0cmFjdC5zYW1wbGVzKG0yKQpwRiA8LSBhcHBseSggaW52X2xvZ2l0KHBvc3QyJGFbLDEsXSkgLCAyICwgbWVhbiApCnBNIDwtIGFwcGx5KCBpbnZfbG9naXQocG9zdDIkYVssMixdKSAsIDIgLCBtZWFuICkKCnBsb3QoIHBEZiAsIHBEbSAsIGx3ZD0zICwgY29sPWlmZWxzZShwRGY+cERtLDIsNCkgLCBwY2g9aWZlbHNlKHBGPnBNLCJGIiwiTSIpICkKYWJsaW5lKGE9MCxiPTEsIGx0eT0zKQoKaWRlbnRpZnkoIHBEZiAsIHBEbSAsIHJvdW5kKHBfYXdhcmQsMikgKQpgYGAK" download="06-problem-set-solutions.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

## Question 1

The data in `data(NWOGrants)` are outcomes for scientific funding applications for the Netherlands Organization for Scientific Research (NWO) from 2010-2012. These data have a very similar structure to the UCBAdmit data discussed in Chapter 11. Draw a DAG for this sample and then use one or more binomial GLMs to estimate the **TOTAL** causal effect of gender on grant awards.

``` r
library(rethinking)
data(NWOGrants)
d <- NWOGrants

head(d)
```

``` language-r
##          discipline gender applications awards
## 1 Chemical sciences      m           83     22
## 2 Chemical sciences      f           39     10
## 3 Physical sciences      m          135     26
## 4 Physical sciences      f           39      9
## 5           Physics      m           67     18
## 6           Physics      f            9      2
```

Let’s plot the DAG.

``` r
library(dagitty)

g <- dagitty('dag {
bb="0,0,1,1"
G [pos="0.251,0.481"]
D [pos="0.251,0.352"]
A [pos="0.481,0.352"]
G -> D
G -> A
D -> A
}
')
plot(g)
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Since Department is a mediator, we would **not** condition by it to find the total causal effect of gender on awards.

``` r
dat <- list(
    A = as.integer(d$awards),
    N = as.integer(d$applications),
    G = ifelse( d$gender=="m" , 1L , 2L ) ,
    D = as.integer(d$discipline) 
)

m1 <- ulam(
  alist(
    A ~ dbinom( N, p ) ,
    logit(p) <- a[G] ,
    a[G] ~ dnorm(0, 1.5)
  ) , data = dat, chains = 4
)
```

``` language-r
## Running MCMC with 4 sequential chains, with 1 thread(s) per chain...
## 
## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 1 finished in 0.0 seconds.
## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 2 finished in 0.0 seconds.
## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 3 finished in 0.0 seconds.
## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 4 finished in 0.0 seconds.
## 
## All 4 chains finished successfully.
## Mean chain execution time: 0.0 seconds.
## Total execution time: 0.7 seconds.
```

``` r
precis(m1, depth = 2)
```

``` language-r
##           mean         sd      5.5%     94.5%    n_eff     Rhat4
## a[1] -1.535284 0.06227377 -1.635018 -1.435144 1511.267 0.9989355
## a[2] -1.740657 0.07917990 -1.870740 -1.612703 1596.696 0.9994035
```

Before looking at the contrasts (causal effect), we’ll look at convergence statistics. The Rhat’s are 1 which are good. Let’s also look at the trace and trank plots.

``` r
traceplot(m1)
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-5-1.png" width="672" />

``` r
trankplot(m1)
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-6-1.png" width="672" />

Therefore, our model has appropriate convergence statistics.

We’ll need to calculate the **contrasts** to make a determination of the total causal effect of gender on awards.

``` r
post1 <- extract.samples(m1)
post1$pGf <- inv_logit(post1$a[,1])
post1$pGm <- inv_logit(post1$a[,2])
post1$G_contrast <- post1$pGf - post1$pGm

dens( post1$G_contrast, lwd=4, col=2, xlab="F-M contrast (total)")
abline( v=0, lty=3)
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-7-1.png" width="672" />

We find about a 3% difference on average for men over women. This could be significant.

We can also do a posterior predictive (validation) check on each of the 18 individuals.

``` r
postcheck(m1)
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-8-1.png" width="672" />

For each individual, the model fits okay but not great.

### Alternative approach

You may also use the `brms` package for this model along with `tidyverse` based packages.

``` r
library(brms)

m1_brms <- brm(awards | trials(applications) ~ gender, family = binomial("logit"), prior = set_prior("normal(0,1.5)", class = "b"), data = d, chains = 4)
```

``` language-r
## 
## SAMPLING FOR MODEL '07311d06eaed335b1371c9568f20ecc2' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 2.1e-05 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.21 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.014968 seconds (Warm-up)
## Chain 1:                0.015221 seconds (Sampling)
## Chain 1:                0.030189 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '07311d06eaed335b1371c9568f20ecc2' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 4e-06 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.04 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.015302 seconds (Warm-up)
## Chain 2:                0.015328 seconds (Sampling)
## Chain 2:                0.03063 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '07311d06eaed335b1371c9568f20ecc2' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 5e-06 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.05 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.015096 seconds (Warm-up)
## Chain 3:                0.015424 seconds (Sampling)
## Chain 3:                0.03052 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '07311d06eaed335b1371c9568f20ecc2' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 6e-06 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.06 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.015164 seconds (Warm-up)
## Chain 4:                0.014215 seconds (Sampling)
## Chain 4:                0.029379 seconds (Total)
## Chain 4:
```

``` r
summary(m1_brms)
```

``` language-r
##  Family: binomial 
##   Links: mu = logit 
## Formula: awards | trials(applications) ~ gender 
##    Data: d (Number of observations: 18) 
##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
##          total post-warmup draws = 4000
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
## Intercept    -1.74      0.08    -1.90    -1.59 1.00     2252     2457
## genderm       0.21      0.10     0.00     0.42 1.00     2779     2377
## 
## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
## and Tail_ESS are effective sample size measures, and Rhat is the potential
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

``` r
library(tidybayes)
library(magrittr) # for %>%
library(dplyr)

posterior_draws <- m1_brms %>%
  spread_draws(b_genderm,b_Intercept) %>% # sample/draw posteriors
  mutate(b_M = b_Intercept + b_genderm) %>% # due to dummy variables
  mutate(Contrast = inv_logit(b_Intercept) - inv_logit(b_M))

ggplot(posterior_draws, aes(x = Contrast)) +
  geom_density() +
  labs(x = "F-M contrast (total)")
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-10-1.png" width="672" />

We can check that the posterior contrasts from both approaches are similar.

``` r
mean(post1$G_contrast)
```

``` language-r
## [1] 0.02789571
```

``` r
mean(posterior_draws$Contrast)
```

``` language-r
## [1] -0.0284291
```

One nice thing about using `brms` is there are many other helpful packages. For example, we can easily calculate posterior predictive checks:

``` r
bayesplot::pp_check(m1_brms)
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-13-1.png" width="672" />

## Question 2

Now estimate the **DIRECT** causal effect of gender on grant awards. Compare the average direct causal effect of gender, weighting each discipline in proportion to the number of applications in the sample. Refer to the marginal effect example in Lecture 9 for help.

``` r
m2 <- ulam(
  alist(
    A ~ dbinom( N, p ) ,
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0, 1.5)
  ) , data = dat, chains = 4
)
```

``` language-r
## Running MCMC with 4 sequential chains, with 1 thread(s) per chain...
## 
## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 1 finished in 0.0 seconds.
## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 2 finished in 0.0 seconds.
## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 3 finished in 0.0 seconds.
## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 4 finished in 0.0 seconds.
## 
## All 4 chains finished successfully.
## Mean chain execution time: 0.0 seconds.
## Total execution time: 0.7 seconds.
```

``` r
# fyi: an identical way to specify priors in ulam
# we use above as easier for post-modeling calculations
# logit(p) <- a[G] + delta[D] ,
# a[G] ~ dnorm(0, 1.5) ,
# delta[D] ~ dnorm(0, 1.5)
```

``` r
precis(m2, depth = 2)
```

``` language-r
## [1] mean  sd    5.5%  94.5% n_eff Rhat4
## <0 rows> (or 0-length row.names)
```

Let’s calculate the contrast for gender.

``` r
total_apps <- sum(dat$N)
apps_per_disc <- sapply( 1:9 , function(i) sum(dat$N[dat$D==i]) )

pG1 <- link(m2,data=list(
    D=rep(1:9,times=apps_per_disc),
    N=rep(1,total_apps),
    G=rep(1,total_apps)))

pG2 <- link(m2,data=list(
    D=rep(1:9,times=apps_per_disc),
    N=rep(1,total_apps),
    G=rep(2,total_apps)))

dens( pG1 - pG2 , lwd=4 , col=2 , xlab="F-M contrast (marginal)" , xlim=c(-0.3,0.3) )
abline( v=0 , lty=3 )
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-16-1.png" width="672" />

``` r
plot( NULL , xlim=c(-0.4,0.4) , ylim=c(0,18) , xlab="F-M contrast for each discipline" , ylab="Density" )
abline( v=0 , lty=3 )
dat$disc <- as.character(d$discipline)
disc <- dat$disc[order(dat$D)]
for ( i in 1:9 ) {
    pG1Di <- link(m2,data=list(D=i,N=1,G=1))
    pG2Di <- link(m2,data=list(D=i,N=1,G=2))
    Gcont <- pG1Di - pG2Di
    dens( Gcont , add=TRUE , lwd=3 , col=i )
    xloc <- ifelse( mean(Gcont) < 0 , -0.35 , 0.35 )
    xpos <- ifelse( mean(Gcont) < 0 , 4 , 2 )
    text( xloc + 0.5*mean(Gcont) , 18-i , disc[2*i] , col=i , pos=xpos , font=2 )
}
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-17-1.png" width="672" />

## Question 3

Considering the total effect (problem 1) and direct effect (problem 2) of gender, what causes contribute to the average difference between women and men in award rate in this sample? It is not necessary to say whether or not there is evidence of discrimination. Simply explain how the direct effects you have estimated make sense (or not) of the total effect.

``` r
total_f <- sum(d$applications[d$gender=="f"])
pDf <- d$applications[d$gender=="f"] / total_f

total_m <- sum(d$applications[d$gender=="m"])
pDm <- d$applications[d$gender=="m"] / total_m

# overall award rate in each discipline
n_apps <- xtabs( dat$N ~ dat$D )
n_awards <- xtabs( dat$A ~ dat$D )
p_award <- n_awards / n_apps

# f/m award rate in each discipline
post2 <- extract.samples(m2)
pF <- apply( inv_logit(post2$a[,1,]) , 2 , mean )
pM <- apply( inv_logit(post2$a[,2,]) , 2 , mean )

plot( pDf , pDm , lwd=3 , col=ifelse(pDf>pDm,2,4) , pch=ifelse(pF>pM,"F","M") )
abline(a=0,b=1, lty=3)

identify( pDf , pDm , round(p_award,2) )
```

<img src="/assignment/06-problem-set-solutions_files/figure-html/unnamed-chunk-18-1.png" width="672" />

``` language-r
## integer(0)
```
