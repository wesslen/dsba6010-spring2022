---
title: Problem Set 7 Solutions
date: "2022-04-05"
menu:
  assignment:
    parent: Problem sets
    weight: 7
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on April 4, 2022 at 11:59am.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA3IFNvbHV0aW9ucwpkYXRlOiAiYHIgU3lzLkRhdGUoKWAiCi0tLQoKVGhpcyBwcm9ibGVtIHNldCBpcyBkdWUgb24gQXByaWwgNCwgMjAyMiBhdCAxMTo1OWFtLgoKIyMgUXVlc3Rpb24gMQoKQ29uZHVjdCBhIHByaW9yIHByZWRpY3RpdmUgc2ltdWxhdGlvbiBmb3IgdGhlIFJlZWRmcm9nIG1vZGVsLiBCeSB0aGlzIEkgbWVhbiB0byBzaW11bGF0ZSB0aGUgcHJpb3IgZGlzdHJpYnV0aW9uIG9mIHRhbmsgc3Vydml2YWwgcHJvYmFiaWxpdGllcyAkXGFscGhhX3tqfSQuIAoKU3RhcnQgYnkgdXNpbmcgdGhlc2UgcHJpb3JzOgoKJFxhbHBoYV97an0gXHNpbSBOb3JtYWwoXGJhcntcYWxwaGF9LFxzaWdtYSkkCgokXGJhcntcYWxwaGF9IFxzaW0gTm9ybWFsKDAsIDEpJAoKJFxzaWdtYSBcc2ltIEV4cG9uZW50aWFsKDEpJAoKQmUgc3VyZSB0byB0cmFuc2Zvcm0gdGhlICRcYWxwaGFfe2p9JCB2YWx1ZXMgdG8gdGhlIHByb2JhYmlsaXR5IHNjYWxlIGZvciBwbG90dGluZyBhbmQgc3VtbWFyeS4KCkhvdyBkb2VzIGluY3JlYXNpbmcgdGhlIHdpZHRoIG9mIHRoZSBwcmlvciBvbiDPgyBjaGFuZ2UgdGhlIHByaW9yIGRpc3RyaWJ1dGlvbiBvZiAkXGFscGhhX3tqfSQ/CgpZb3UgbWlnaHQgdHJ5IGBFeHBvbmVudGlhbCgxMClgIGFuZCBgRXhwb25lbnRpYWwoMC4xKWAgZm9yIGV4YW1wbGUuCgpTaW11bGF0aW5nIHZhcnlpbmcgZWZmZWN0IHByaW9ycyBpcyBpbiBwcmluY2lwbGUgbGlrZSBzaW11bGF0aW5nIGFueSBvdGhlciBwcmlvcnMuIFRoZSBvbmx5IGRpZmZlcmVuY2UgaXMgdGhhdCB0aGUgcGFyYW1ldGVycyBoYXZlIGFuIGltcGxpZWQgb3JkZXIgbm93LCBiZWNhdXNlIHNvbWUgcGFyYW1ldGVycyBkZXBlbmQgdXBvbiBvdGhlcnMuIFNvIGluIHRoaXMgcHJvYmxlbSB3ZSBtdXN0IHNpbXVsYXRlICRcc2lnbWEkIGFuZCAkXGJhcntcYWxwaGF9JCBmaXJzdCwgYW5kIHRoZW4gd2UgY2FuIHNpbXVsYXRlIHRoZSBpbmRpdmlkdWFsIHRhbmsgJFxhbHBoYV97VH0kIHZhcmlhYmxlcwoKYGBge3J9CmxpYnJhcnkocmV0aGlua2luZykKbiA8LSAxZTQKc2lnbWEgPC0gcmV4cChuLDEpCmFiYXIgPC0gcm5vcm0obiwwLDEpCmFUIDwtIHJub3JtKG4sYWJhcixzaWdtYSkKZGVucyhpbnZfbG9naXQoYVQpLHhsaW09YygwLDEpLGFkaj0wLjEsbHdkPTQsY29sPTIsIG1haW49InNpZ21hfmV4cG9uZW50aWFsKDAsMSkiKQpgYGAKCkxldCdzIGFsc28gcnVuIHR3byBtb3JlICgwLjEgYW5kIDEwKTogCgpgYGB7cn0KbiA8LSAxZTQKc2lnbWEgPC0gcmV4cChuLDAuMSkKYWJhciA8LSBybm9ybShuLDAsMSkKYVQgPC0gcm5vcm0obixhYmFyLHNpZ21hKQpkZW5zKGludl9sb2dpdChhVCkseGxpbT1jKDAsMSksYWRqPTAuMSxsd2Q9NCxjb2w9MiwgbWFpbj0ic2lnbWF+ZXhwb25lbnRpYWwoMCwwLjEpIikKYGBgCgpgYGB7cn0KbiA8LSAxZTQKc2lnbWEgPC0gcmV4cChuLDEwKQphYmFyIDwtIHJub3JtKG4sMCwxKQphVCA8LSBybm9ybShuLGFiYXIsc2lnbWEpCmRlbnMoaW52X2xvZ2l0KGFUKSx4bGltPWMoMCwxKSxhZGo9MC4xLGx3ZD00LGNvbD0yLCBtYWluPSJzaWdtYX5leHBvbmVudGlhbCgwLDEwKSIpCmBgYAoKSW5jcmVhc2luZyB0aGUgdmFyaWF0aW9uIGFjcm9zcyB0YW5rcywgYnkgbWFraW5nIHRoZSAkXHNpZ21hJCBkaXN0cmlidXRpb24gd2lkZXIsIHB1c2hlcyBwcmlvciBzdXJ2aXZhbCB1cCBhZ2FpbnN0IHRoZSBmbG9vciBhbmQgY2VpbGluZyBvZiB0aGUgb3V0Y29tZSBzcGFjZS4gVGhpcyBpcyB0aGUgc2FtZSBwaGVub21lbm9uIHlvdSBzYXcgYmVmb3JlIGZvciBvcmRpbmFyeSBsb2dpdCBtb2RlbHMuIFRoZSBrZXkgbGVzc29uIGFnYWluIGlzIHRoYXQgZmxhdCBwcmlvcnMgb24gb25lIHNjYWxlIGFyZSBub3QgbmVjZXNzYXJpbHkgZmxhdCBvbiBhbm90aGVyLgoKIyMgUXVlc3Rpb24gMgoKUmV2aXNpdCB0aGUgUmVlZGZyb2cgc3Vydml2YWwgZGF0YSwgYGRhdGEocmVlZGZyb2dzKWAuIFN0YXJ0IHdpdGggdGhlIHZhcnlpbmcgZWZmZWN0cyBtb2RlbCBmcm9tIHRoZSBib29rIGFuZCBsZWN0dXJlLiBUaGVuIG1vZGlmeSBpdCB0byBlc3RpbWF0ZSB0aGUgY2F1c2FsIGVmZmVjdHMgb2YgdGhlIHRyZWF0bWVudCB2YXJpYWJsZXMgcHJlZCBhbmQgc2l6ZSwgaW5jbHVkaW5nIGhvdyBzaXplIG1pZ2h0IG1vZGlmeSB0aGUgZWZmZWN0IG9mIHByZWRhdGlvbi4gQW4gZWFzeSBhcHByb2FjaCBpcyB0byBlc3RpbWF0ZSBhbiBlZmZlY3QgZm9yIGVhY2ggY29tYmluYXRpb24gb2YgcHJlZCBhbmQgc2l6ZS4gSnVzdGlmeSB5b3VyIG1vZGVsIHdpdGggYSBEQUcgb2YgdGhpcyBleHBlcmltZW50LgoKYGBge3J9CmxpYnJhcnkocmV0aGlua2luZykKZGF0YShyZWVkZnJvZ3MpCmQgPC0gcmVlZGZyb2dzCgpkYXQgPC0gbGlzdCgKUyA9IGQkc3VydiwKRCA9IGQkZGVuc2l0eSwKVCA9IDE6bnJvdyhkKSwKUCA9IGlmZWxzZSggZCRwcmVkPT0ibm8iICwgMUwgLCAyTCApLApHID0gaWZlbHNlKCBkJHNpemU9PSJzbWFsbCIgLCAxTCAsIDJMICkgKQoKbTIgPC0gdWxhbSgKICBhbGlzdCgKICAgIFMgfiBiaW5vbWlhbCggRCAsIHAgKSwKICAgIGxvZ2l0KHApIDwtIGFbVF0gKyBiW1AsR10sCiAgICBhW1RdIH4gbm9ybWFsKCAwICwgc2lnbWEgKSwKICAgIG1hdHJpeFtQLEddOmIgfiBub3JtYWwoIDAgLCAxICksCiAgICBzaWdtYSB+IGV4cG9uZW50aWFsKCAxICkKICApLCBkYXRhPWRhdCAsIGNoYWlucz00ICwgY29yZXM9NCAsIGxvZ19saWs9VFJVRSApCnByZWNpcyhtMiwzLHBhcnM9YygiYiIsInNpZ21hIikpCgpgYGAKClRoZSBwYXJhbWV0ZXJzIGFyZSBpbiBvcmRlciBmcm9tIHRvcCB0byBib3R0b206IG5vLXByZWQvc21hbGwsIG5vLXByZWQvbGFyZ2UsIHByZWQvc21hbGwsIHByZWQvbGFyZ2UuIFRoZSBjdXJpb3VzIHRoaW5nIGlzIG5vdCB0aGF0IHN1cnZpdmFsIGlzIGxvd2VyIHdpdGggcHJlZGF0aW9uLCBidXQgcmF0aGVyIHRoYXQgaXQgaXMgbG93ZXN0IGZvciBsYXJnZSB0YWRwb2xlcywgYGJbMiwyXWAuIFRoaXMgaXMgYSBzdHJvbmcgaW50ZXJhY3Rpb24gdGhhdCB3b3VsZCBiZSBtaXNzZWQgaWYgd2UgaGFkIG1hZGUgdGhlIGVmZmVjdHMgcHVyZWx5IGFkZGl0aXZlIHdpdGggb25lIGFub3RoZXIgKG9uIHRoZSBsb2ctb2RkcyBzY2FsZSkuIFRoZSBWb25lc2ggJiBCb2xrZXIgcGFwZXIgdGhhdCB0aGVzZSBkYXRhIGNvbWUgZnJvbSBnb2VzIGludG8gdGhpcyBpbnRlcmFjdGlvbiBpbiBncmVhdCBkZXB0aC4KClRoZSBwcm9ibGVtIGFza2VkIGZvciBhIGp1c3RpZmljYXRpb24gb2YgdGhlIG1vZGVsIGluIHRlcm1zIG9mIHRoZSBEQUcuIAoKCmBgYHtyIGZpZy5oZWlnaHQ9MiwgZmlnLndpZHRoPTIsIGVjaG89RkFMU0V9CmxpYnJhcnkoZGFnaXR0eSkKCmcgPC0gZGFnaXR0eSgnZGFnIHsKYmI9IjAsMCwxLDEiCkQgW3Bvcz0iMC42NzAsMC4yMTgiXQpHIFtwb3M9IjAuNzg3LDAuNDUzIl0KUCBbcG9zPSIwLjIwMywwLjQ1NyJdClMgW291dGNvbWUscG9zPSIwLjUxNCwwLjQ0OCJdClQgW3Bvcz0iMC4zNTQsMC4yMDAiXQpEIC0+IFMKRyAtPiBTClAgLT4gUwpUIC0+IFMKfQonKQpwbG90KGcpCmBgYAoKVGhpcyBpcyBhbiBleHBlcmltZW50LCBzbyB3ZSBrbm93IHRoZSB0cmVhdG1lbnRzIFAsIEcsIGFuZCBEIGFyZSBub3QgY29uZm91bmRlZC4gQXQgbGVhc3Qgbm90IGluIGFueSBvYnZpb3VzIHdheS4gQW5kIHRoZW4gdW5vYnNlcnZlZCB0YW5rIGVmZmVjdHMgVCBhbHNvIG1vZGVyYXRlIHRoZSBpbmZsdWVuY2Ugb2YgdGhlIHRyZWF0bWVudHMuIFRoZSBtb2RlbCBJIHVzZWQgdHJpZXMgdG8gZXN0aW1hdGUgaG93IFAgYW5kIEcgbW9kZXJhdGUgb25lIGFub3RoZXIuIEl0IGlnbm9yZXMgRCwgd2hpY2ggd2UgYXJlIGFsbG93ZWQgdG8gZG8sIGJlY2F1c2UgaXQgaXMgbm90IGEgY29uZm91bmQsIGp1c3QgYSBjb21wZXRpbmcgY2F1c2UuIEJ1dCBJIGluY2x1ZGUgdGFua3MsIHdoaWNoIGlzIGFsc28ganVzdCBhIGNvbXBldGluZyBjYXVzZS4gSW5jbHVkaW5nIGNvbXBldGluZyBjYXVzZXMgaGVscHMgd2l0aCBwcmVjaXNpb24sIGlmIG5vdGhpbmcgZWxzZS4KClRoZXkganVzdCBzaG93IGlucHV0cyBhbmQgb3V0cHV0cy4gVG8ganVzdGlmeSBhbnkgcGFydGljdWxhciBzdGF0aXN0aWNhbCBtb2RlbCwgeW91IG5lZWQgbW9yZSB0aGFuIHRoZSBEQUcuCgojIyBRdWVzdGlvbiAzCgpOb3cgZXN0aW1hdGUgdGhlIGNhdXNhbCBlZmZlY3Qgb2YgZGVuc2l0eSBvbiBzdXJ2aXZhbC4gQ29uc2lkZXIgd2hldGhlciBwcmVkIG1vZGlmaWVzIHRoZSBlZmZlY3Qgb2YgZGVuc2l0eS4gVGhlcmUgYXJlIHNldmVyYWwgZ29vZCB3YXlzIHRvIGluY2x1ZGUgZGVuc2l0eSBpbiB5b3VyIEJpbm9taWFsIEdMTS4gWW91IGNvdWxkIHRyZWF0IGl0IGFzIGEgY29udGludW91cyByZWdyZXNzaW9uIHZhcmlhYmxlIChwb3NzaWJseSBzdGFuZGFyZGl6ZWQpLiBPciB5b3UgY291bGQgY29udmVydCBpdCB0byBhbiBvcmRlcmVkIGNhdGVnb3J5ICh3aXRoIHRocmVlIGxldmVscykuIAoKQ29tcGFyZSB0aGUgJFxzaWdtYSQgKHRhbmsgc3RhbmRhcmQgZGV2aWF0aW9uKSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uIHRvICRcc2lnbWEkIGZyb20geW91ciBtb2RlbCBpbiBQcm9ibGVtIDIuIEhvdyBhcmUgdGhleSBkaWZmZXJlbnQ/IFdoeT8KCkRlbnNpdHkgaXMgYW4gaW1wb3J0YW50IGZhY3RvciBpbiB0aGVzZSBleHBlcmltZW50cy4gU28gbGV04oCZcyBpbmNsdWRlIGl0IGZpbmFsbHkuIEkgd2lsbCBkbyBzb21ldGhpbmcgc2ltcGxlLCBqdXN0IGluY2x1ZGUgaXQgYXMgYW4gYWRkaXRpdmUgZWZmZWN0IHRoYXQgaW50ZXJhY3RzIHdpdGggcHJlZGF0b3JzLiBCdXQgSSB3aWxsIHVzZSB0aGUgbG9nYXJpdGhtIG9mIGRlbnNpdHksIHNvIHRoYXQgaXQgaGFzIGltcGxpZWQgZGltaW5pc2hpbmcgcmV0dXJucyBvbiB0aGUgbG9nLW9kZHMgc2NhbGUuCgpgYGB7cn0KZGF0JERvIDwtIHN0YW5kYXJkaXplKGxvZyhkJGRlbnNpdHkpKQoKbTMgPC0gdWxhbSgKICBhbGlzdCgKICAgIFMgfiBiaW5vbWlhbCggRCAsIHAgKSwKICAgIGxvZ2l0KHApIDwtIGFbVF0gKyBiW1AsR10gKyBiRFtQXSpEbywKICAgIGFbVF0gfiBub3JtYWwoIDAgLCBzaWdtYSApLAogICAgbWF0cml4W1AsR106YiB+IG5vcm1hbCggMCAsIDEgKSwKICAgIGJEW1BdIH4gbm9ybWFsKDAsMC41KSwKICAgIHNpZ21hIH4gZXhwb25lbnRpYWwoIDEgKQogICksIGRhdGE9ZGF0ICwgY2hhaW5zPTQgLCBjb3Jlcz00ICwgbG9nX2xpaz1UUlVFICkKCnByZWNpcyhtMywzLHBhcnM9YygiYiIsImJEIiwic2lnbWEiKSkKYGBgCgpBZ2FpbiBhbiBpbnRlcmFjdGlvbi4gSGlnaGVyIGRlbnNpdGllcyBhcmUgd29yc2UgZm9yIHN1cnZpdmFsLCBidXQgb25seSBpbiB0aGUgcHJlc2VuY2Ugb2YgcHJlZGF0b3JzLiBUaGUgb3RoZXIgZXN0aW1hdGVzIGFyZSBub3QgY2hhbmdlZCBtdWNoLiBUaGUgJFxzaWdtYSQgZXN0aW1hdGUgaGVyZSBpcyBhIGxpdHRsZSBzbWFsbGVyIHRoYW4gaW4gdGhlIHByZXZpb3VzIHByb2JsZW0uIFRoaXMgaXMganVzdCBiZWNhdXNlIGRlbnNpdHkgaXMgYW4gcmVhbCBjYXVzZSBvZiBzdXJ2aXZhbCwgc28gaXQgZXhwbGFpbnMgc29tZSBvZiB0aGUgdmFyaWF0aW9uIHRoYXQgd2FzIHByZXZpb3VzbHkgc29ha2VkIHVwIGJ5IHRhbmtzIHdpdGggZGlmZmVyZW50IGRlbnNpdGllcy4=" download="07-problem-set-solutions.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

## Question 1

Conduct a prior predictive simulation for the Reedfrog model. By this I mean to simulate the prior distribution of tank survival probabilities `\(\alpha_{j}\)`.

Start by using these priors:

`\(\alpha_{j} \sim Normal(\bar{\alpha},\sigma)\)`

`\(\bar{\alpha} \sim Normal(0, 1)\)`

`\(\sigma \sim Exponential(1)\)`

Be sure to transform the `\(\alpha_{j}\)` values to the probability scale for plotting and summary.

How does increasing the width of the prior on σ change the prior distribution of `\(\alpha_{j}\)`?

You might try `Exponential(10)` and `Exponential(0.1)` for example.

Simulating varying effect priors is in principle like simulating any other priors. The only difference is that the parameters have an implied order now, because some parameters depend upon others. So in this problem we must simulate `\(\sigma\)` and `\(\bar{\alpha}\)` first, and then we can simulate the individual tank `\(\alpha_{T}\)` variables

``` r
library(rethinking)
n <- 1e4
sigma <- rexp(n,1)
abar <- rnorm(n,0,1)
aT <- rnorm(n,abar,sigma)
dens(inv_logit(aT),xlim=c(0,1),adj=0.1,lwd=4,col=2, main="sigma~exponential(0,1)")
```

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Let’s also run two more (0.1 and 10):

``` r
n <- 1e4
sigma <- rexp(n,0.1)
abar <- rnorm(n,0,1)
aT <- rnorm(n,abar,sigma)
dens(inv_logit(aT),xlim=c(0,1),adj=0.1,lwd=4,col=2, main="sigma~exponential(0,0.1)")
```

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-3-1.png" width="672" />

``` r
n <- 1e4
sigma <- rexp(n,10)
abar <- rnorm(n,0,1)
aT <- rnorm(n,abar,sigma)
dens(inv_logit(aT),xlim=c(0,1),adj=0.1,lwd=4,col=2, main="sigma~exponential(0,10)")
```

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Increasing the variation across tanks, by making the `\(\sigma\)` distribution wider, pushes prior survival up against the floor and ceiling of the outcome space. This is the same phenomenon you saw before for ordinary logit models. The key lesson again is that flat priors on one scale are not necessarily flat on another.

## Question 2

Revisit the Reedfrog survival data, `data(reedfrogs)`. Start with the varying effects model from the book and lecture. Then modify it to estimate the causal effects of the treatment variables pred and size, including how size might modify the effect of predation. An easy approach is to estimate an effect for each combination of pred and size. Justify your model with a DAG of this experiment.

``` r
library(rethinking)
data(reedfrogs)
d <- reedfrogs

dat <- list(
S = d$surv,
D = d$density,
T = 1:nrow(d),
P = ifelse( d$pred=="no" , 1L , 2L ),
G = ifelse( d$size=="small" , 1L , 2L ) )

m2 <- ulam(
  alist(
    S ~ binomial( D , p ),
    logit(p) <- a[T] + b[P,G],
    a[T] ~ normal( 0 , sigma ),
    matrix[P,G]:b ~ normal( 0 , 1 ),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )
```

``` language-r
## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
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
## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 1 finished in 0.1 seconds.
## Chain 2 finished in 0.1 seconds.
## Chain 3 finished in 0.1 seconds.
## Chain 4 finished in 0.1 seconds.
## 
## All 4 chains finished successfully.
## Mean chain execution time: 0.1 seconds.
## Total execution time: 0.2 seconds.
```

``` r
precis(m2,3,pars=c("b","sigma"))
```

``` language-r
##              mean        sd        5.5%       94.5%     n_eff     Rhat4
## b[1,1]  2.3810618 0.3172046  1.87589300  2.86919795 1200.3961 1.0008448
## b[1,2]  2.5056545 0.3177198  2.02876910  3.00680655 1083.9492 0.9998908
## b[2,1]  0.4425123 0.2459235  0.06262386  0.84003217  676.8399 1.0068731
## b[2,2] -0.4254197 0.2555635 -0.84205317 -0.01756207  710.5806 0.9998809
## sigma   0.7407306 0.1508802  0.51863864  0.99502158  441.7377 1.0075067
```

The parameters are in order from top to bottom: no-pred/small, no-pred/large, pred/small, pred/large. The curious thing is not that survival is lower with predation, but rather that it is lowest for large tadpoles, `b[2,2]`. This is a strong interaction that would be missed if we had made the effects purely additive with one another (on the log-odds scale). The Vonesh & Bolker paper that these data come from goes into this interaction in great depth.

The problem asked for a justification of the model in terms of the DAG.

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-6-1.png" width="192" />

This is an experiment, so we know the treatments P, G, and D are not confounded. At least not in any obvious way. And then unobserved tank effects T also moderate the influence of the treatments. The model I used tries to estimate how P and G moderate one another. It ignores D, which we are allowed to do, because it is not a confound, just a competing cause. But I include tanks, which is also just a competing cause. Including competing causes helps with precision, if nothing else.

They just show inputs and outputs. To justify any particular statistical model, you need more than the DAG.

## Question 3

Now estimate the causal effect of density on survival. Consider whether pred modifies the effect of density. There are several good ways to include density in your Binomial GLM. You could treat it as a continuous regression variable (possibly standardized). Or you could convert it to an ordered category (with three levels).

Compare the `\(\sigma\)` (tank standard deviation) posterior distribution to `\(\sigma\)` from your model in Problem 2. How are they different? Why?

Density is an important factor in these experiments. So let’s include it finally. I will do something simple, just include it as an additive effect that interacts with predators. But I will use the logarithm of density, so that it has implied diminishing returns on the log-odds scale.

``` r
dat$Do <- standardize(log(d$density))

m3 <- ulam(
  alist(
    S ~ binomial( D , p ),
    logit(p) <- a[T] + b[P,G] + bD[P]*Do,
    a[T] ~ normal( 0 , sigma ),
    matrix[P,G]:b ~ normal( 0 , 1 ),
    bD[P] ~ normal(0,0.5),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )
```

``` language-r
## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
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
## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 1 finished in 0.2 seconds.
## Chain 2 finished in 0.2 seconds.
## Chain 3 finished in 0.1 seconds.
## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 4 finished in 0.1 seconds.
## 
## All 4 chains finished successfully.
## Mean chain execution time: 0.2 seconds.
## Total execution time: 0.4 seconds.
```

``` r
precis(m3,3,pars=c("b","bD","sigma"))
```

``` language-r
##              mean        sd       5.5%      94.5%     n_eff     Rhat4
## b[1,1]  2.3473727 0.2911915  1.9004836  2.8429358 1324.4498 1.0033966
## b[1,2]  2.4927055 0.2976433  2.0269645  2.9834016 1707.5013 1.0001058
## b[2,1]  0.5257347 0.2236003  0.1769636  0.8770541  867.3344 1.0001885
## b[2,2] -0.3549865 0.2323851 -0.7201052  0.0147492 1013.5865 1.0027122
## bD[1]   0.1393125 0.2144765 -0.2121929  0.4681566 1602.1237 0.9997855
## bD[2]  -0.4715676 0.1742109 -0.7566293 -0.1871588 1129.7075 0.9993428
## sigma   0.6343169 0.1316046  0.4395601  0.8521297  381.6673 1.0113039
```

Again an interaction. Higher densities are worse for survival, but only in the presence of predators. The other estimates are not changed much. The `\(\sigma\)` estimate here is a little smaller than in the previous problem. This is just because density is an real cause of survival, so it explains some of the variation that was previously soaked up by tanks with different densities.
