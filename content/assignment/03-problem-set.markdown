---
title: Problem Set 3
date: "2022-02-06"
menu:
  assignment:
    parent: Problem sets
    weight: 3
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on February 21, 2022 at 11:59am.

Step 1: Download this file locally.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCAzCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBGZWJydWFyeSAyMSwgMjAyMiBhdCAxMTo1OWFtLgoKLSAqKk5hbWUqKjoKLSAqKlVOQ0MgSUQqKjogCi0gKipPdGhlciBzdHVkZW50IHdvcmtlZCB3aXRoIChvcHRpb25hbCkqKjoKCiMjIFF1ZXN0aW9uIDEKCkZyb20gdGhlIEhvd2VsbDEgZGF0YXNldCwgY29uc2lkZXIgb25seSB0aGUgcGVvcGxlIHlvdW5nZXIgdGhhbiAxMyB5ZWFycyBvbGQuIEVzdGltYXRlIHRoZSBjYXVzYWwgYXNzb2NpYXRpb24gYmV0d2VlbiBhZ2UgYW5kIHdlaWdodC4gQXNzdW1lIHRoYXQgYWdlIGluZmx1ZW5jZXMgd2VpZ2h0IHRocm91Z2ggdHdvIHBhdGhzLiBGaXJzdCwgYWdlIGluZmx1ZW5jZXMgaGVpZ2h0LCBhbmQgaGVpZ2h0IGluZmx1ZW5jZXMgd2VpZ2h0LiBTZWNvbmQsIGFnZSBkaXJlY3RseSBpbmZsdWVuY2VzIHdlaWdodCB0aHJvdWdoIGFnZSByZWxhdGVkIGNoYW5nZXMgaW4gbXVzY2xlIGdyb3d0aCBhbmQgYm9keSBwcm9wb3J0aW9ucy4gQWxsIG9mIHRoaXMgaW1wbGllcyB0aGlzIGNhdXNhbCBtb2RlbCAoREFHKToKCmBgYHtyIGZpZy5oZWlnaHQ9MiwgZmlnLndpZHRoPTIsaW5jbHVkZT1GQUxTRX0KbGlicmFyeShkYWdpdHR5KQoKZyA8LSBkYWdpdHR5KCdkYWcgewpiYj0iMCwwLDEsMSIKQSBbcG9zPSIwLjI1MSwwLjQ4MSJdCkggW3Bvcz0iMC4zNTAsMC4zMTIiXQpXIFtwb3M9IjAuNDUyLDAuNDg0Il0KQSAtPiBICkEgLT4gVwpIIC0+IFcKfQonKQpwbG90KGcpCmBgYAoKVXNlIGEgbGluZWFyIHJlZ3Jlc3Npb24gdG8gZXN0aW1hdGUgdGhlIHRvdGFsIChub3QganVzdCBkaXJlY3QpIGNhdXNhbCBlZmZlY3Qgb2YKZWFjaCB5ZWFyIG9mIGdyb3d0aCBvbiB3ZWlnaHQuIEJlIHN1cmUgdG8gY2FyZWZ1bGx5IGNvbnNpZGVyIHRoZSBwcmlvcnMuIFRyeQp1c2luZyBwcmlvciBwcmVkaWN0aXZlIHNpbXVsYXRpb24gdG8gYXNzZXNzIHdoYXQgdGhleSBpbXBseQoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCiMjIFF1ZXN0aW9uIDIKCk5vdyBzdXBwb3NlIHRoZSBjYXVzYWwgYXNzb2NpYXRpb24gYmV0d2VlbiBhZ2UgYW5kIHdlaWdodCBtaWdodCBiZSBkaWZmZXJlbnQgZm9yIGJveXMgYW5kIGdpcmxzLiBVc2UgYSBzaW5nbGUgbGluZWFyIHJlZ3Jlc3Npb24sIHdpdGggYSBjYXRlZ29yaWNhbCB2YXJpYWJsZSBmb3Igc2V4LCB0byBlc3RpbWF0ZSB0aGUgdG90YWwgY2F1c2FsIGVmZmVjdCBvZiBhZ2Ugb24gd2VpZ2h0IHNlcGFyYXRlbHkgZm9yIGJveXMgYW5kIGdpcmxzLiBIb3cgZG8gZ2lybHMgYW5kIGJveXMgZGlmZmVyPyBQcm92aWRlIG9uZSBvciBtb3JlIHBvc3RlcmlvciBjb250cmFzdHMgYXMgYSBzdW1tYXJ5LgoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCiMjIFF1ZXN0aW9uIDMKCkZvciB0aGlzIHByb2JsZW0sIHdlIHdhbnQgdG8gY29tcGFyZSB0aGUgZGlmZmVyZW5jZSBiZXR3ZWVuIEZyZXF1ZW50aXN0IGFuZCBCYXllc2lhbiBsaW5lYXIgcmVncmVzc2lvbnMuIFdlJ3JlIGdvaW5nIHRvIHVzZSB0aGUgc2ltaWxhciBmdW5jdGlvbnMgZnJvbSBzZWN0aW9uIDQuNS4KClRvIGJlZ2luLCBJIGhhdmUgcHJvdmlkZWQgdGhlIHNhbWUgY29kZSB0byBnZXQgdGhlIG1vZGVsIGBtNC41YCAoaS5lLiwgcnVuIFIgY29kZSA0LjY1KS4gUGxlYXNlIHJ1biBpdCBhbmQgcmVmcmVzaCB5b3Vyc2VsZi4KCmBgYHtyfQojIFIgY29kZSA0LjY1ICsgNC42NiAocHJlY2lzKQpsaWJyYXJ5KHJldGhpbmtpbmcpCmRhdGEoSG93ZWxsMSkKZCA8LSBIb3dlbGwxCmQkd2VpZ2h0X3MgPC0gKCBkJHdlaWdodCAtIG1lYW4oZCR3ZWlnaHQpICkvc2QoZCR3ZWlnaHQpCmQkd2VpZ2h0X3MyIDwtIGQkd2VpZ2h0X3NeMgoKbTQuNSA8LSBxdWFwKAogIGFsaXN0KAogICAgaGVpZ2h0IH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICBtdSA8LSBhICsgYjEqKHdlaWdodF9zKSArIGIyICogd2VpZ2h0X3MyLAogICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgIGIxIH4gZGxub3JtKCAwICwgMSApICwKICAgIGIyIH4gZG5vcm0oIDAsIDEgKSAsCiAgICBzaWdtYSB+IGR1bmlmKCAwICwgNTAgKQopICwgZGF0YT1kICkKCnByZWNpcyggbTQuNSApCmBgYAoKTm93IG1vZGlmeSBgbTQuNWAgbW9kZWwgYnkgcmVsYXhpbmcgb3VyICJwb3NpdGl2ZSByZWxhdGlvbnNoaXAiIChha2EgbG9nbm9ybWFsKSBhc3N1bXB0aW9uIGZvciB0aGUgYGIxYCB2YXJpYWJsZSBieSBtb2RpZnlpbmcgaXQncyBwcmlvciBhcyBgZG5vcm0oIDAgLCAxIClgIGFuZCBjcmVhdGUgYSBuZXcgbW9kZWwgY2FsbGVkIGBtNC41YmAuIFJ1biBgcHJlY2lzKG00LjViKWAuCgpgYGB7ciBldmFsPUZBTFNFLCBpbmNsdWRlPUZBTFNFfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCmBgYAoKTm93LCBydW4gYSBmcmVxdWVudGlzdCByZWdyZXNzaW9uIG9mIG00LjViIGJ5IHVzaW5nIHRoZSBgbG1gIGZ1bmN0aW9uLiBJIGhhdmUgcHJvdmlkZWQgdGhpcyBjb2RlLgoKYGBge3IgZXZhbD1GQUxTRX0KIyBoaW50OiB5b3UgbmVlZCB0byBvbmx5IHJlbW92ZSB0aGUgZXZhbD1GQUxTRSBzbyB0aGlzIGNvZGUgcnVucwpmbSA8LSBsbShoZWlnaHQgfiB3ZWlnaHRfcyArIHdlaWdodF9zMiwgZGF0YSA9IGQpCm5hbWVzKGZtJGNvZWZmaWNpZW50cykgPC0gYygnYScsJ2IxJywnYjInKSAjIHJlbmFtZSBjb2VmIGZvciBjb25zaXN0ZW5jeQpmbQpgYGAKCk5vdyBjb21wYXJlIGFsbCB0aHJlZSBtb2RlbHMgYnkgdXNpbmcgdGhlIGBjb2VmdGFiKClgIGFuZCBwdXR0aW5nIGFsbCB0aHJlZSBvZiB0aGUgbW9kZWxzIGFzIHBhcmFtZXRlcnMuIFlvdSBjYW4gYWxzbyBydW4gYSBgcGxvdCgpYCBvbiB0aGUgYGNvZWZ0YWIoKWAgZnVuY3Rpb24gdG8gcnVuIGEgcGxvdCBvZiB0aGUgZWZmZWN0cy4KCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgpIb3cgZGlmZmVyZW50IGFyZSB0aGUgbW9kZWxzPwoKIyMgUXVlc3Rpb24gNAoKRm9yIHRoaXMgcHJvYmxlbSwgd2UncmUgZ29pbmcgdG8gcmV1c2UgdGhlIHNhbWUgbW9kZWwgKGBtNC41YCkgZnJvbSBRdWVzdGlvbiAzIGFuZCBydW4gcHJpb3IgcHJlZGljdGl2ZSBzaW11bGF0aW9ucyB0byB1bmRlcnN0YW5kIHRoZSByb2xlIG9mIGRpZmZlcmVudCBwcmlvcnMuIEZvciBoZWxwLCBzZWUgNS40LTUuNSBjb2RlIGluIHRoZSBib29rLgoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCkNoYW5nZSB0aGUgcHJpb3JzIG9uIHRoZSBgYjJgIGNvZWZmaWNpZW50IHRvIGBiMiB+IGRub3JtKDAsIDEwKWAgYW5kIHJlcnVuIHRoZSBwcmlvciBwcmVkaWN0aXZlIHNpbXVsYXRpb24uIAoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCk5vdywgY2hhbmdlIHRoZSBwcmlvcnMgb24gdGhlIGJldGEgY29lZmZpY2llbnRzIHRvIG1vcmUgImZsYXQsIHZlcnkgdW5pbmZvcm1hdGl2ZSIgcHJpb3JzLCBgZG5vcm0oMCwgMTAwKWAgZm9yIGBiMWAgYW5kIGBiMmAuIFJlcnVuIGEgc2ltaWxhciBwcmlvciBwcmVkaWN0aXZlIHNpbXVsYXRpb24uCgpgYGB7ciBldmFsPUZBTFNFLCBpbmNsdWRlPUZBTFNFfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCmBgYAoKIyMgT3B0aW9uYWwgQ2hhbGxlbmdlIChOb3QgZ3JhZGVkKQoKUmV0dXJuIHRvIGBkYXRhKGNoZXJyeV9ibG9zc29tcylgIGFuZCBtb2RlbCB0aGUgYXNzb2NpYXRpb24gYmV0d2VlbiBibG9zc29tIGRhdGUgKGBkYXlgKSBhbmQgTWFyY2ggdGVtcGVyYXR1cmUgKGB0ZW1wYCkuIE5vdGUgdGhhdCB0aGVyZSBhcmUgbWFueSBtaXNzaW5nIHZhbHVlcyBpbiBib3RoIHZhcmlhYmxlcy4gWW91IG1heSBjb25zaWRlciBhIGxpbmVhciBtb2RlbCwgYSBwb2x5bm9taWFsLCBvciBhIHNwbGluZSBvbiB0ZW1wZXJhdHVyZS4gSG93IHdlbGwgZG9lcyB0ZW1wZXJhdHVyZSB0cmVuZCBwcmVkaWN0IHRoZSBibG9zc29tIHRyZW5kPw==" download="03-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1415434).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

## Question 1

From the Howell1 dataset, consider only the people younger than 13 years old. Estimate the causal association between age and weight. Assume that age influences weight through two paths. First, age influences height, and height influences weight. Second, age directly influences weight through age related changes in muscle growth and body proportions. All of this implies this causal model (DAG):

<img src="/assignment/03-problem-set_files/figure-html/unnamed-chunk-2-1.png" width="192" />

Use a linear regression to estimate the total (not just direct) causal effect of
each year of growth on weight. Be sure to carefully consider the priors. Try
using prior predictive simulation to assess what they imply

## Question 2

Now suppose the causal association between age and weight might be different for boys and girls. Use a single linear regression, with a categorical variable for sex, to estimate the total causal effect of age on weight separately for boys and girls. How do girls and boys differ? Provide one or more posterior contrasts as a summary.

## Question 3

For this problem, we want to compare the difference between Frequentist and Bayesian linear regressions. We’re going to use the similar functions from section 4.5.

To begin, I have provided the same code to get the model `m4.5` (i.e., run R code 4.65). Please run it and refresh yourself.

``` r
# R code 4.65 + 4.66 (precis)
library(rethinking)
data(Howell1)
d <- Howell1
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2

m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*(weight_s) + b2 * weight_s2,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0, 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d )

precis( m4.5 )
```

``` language-r
##             mean        sd       5.5%      94.5%
## a     146.057412 0.3689756 145.467718 146.647107
## b1     21.733065 0.2888890  21.271365  22.194766
## b2     -7.803266 0.2741839  -8.241465  -7.365067
## sigma   5.774474 0.1764652   5.492449   6.056500
```

Now modify `m4.5` model by relaxing our “positive relationship” (aka lognormal) assumption for the `b1` variable by modifying it’s prior as `dnorm( 0 , 1 )` and create a new model called `m4.5b`. Run `precis(m4.5b)`.

Now, run a frequentist regression of m4.5b by using the `lm` function. I have provided this code.

``` r
# hint: you need to only remove the eval=FALSE so this code runs
fm <- lm(height ~ weight_s + weight_s2, data = d)
names(fm$coefficients) <- c('a','b1','b2') # rename coef for consistency
fm
```

Now compare all three models by using the `coeftab()` and putting all three of the models as parameters. You can also run a `plot()` on the `coeftab()` function to run a plot of the effects.

How different are the models?

## Question 4

For this problem, we’re going to reuse the same model (`m4.5`) from Question 3 and run prior predictive simulations to understand the role of different priors. For help, see 5.4-5.5 code in the book.

Change the priors on the `b2` coefficient to `b2 ~ dnorm(0, 10)` and rerun the prior predictive simulation.

Now, change the priors on the beta coefficients to more “flat, very uninformative” priors, `dnorm(0, 100)` for `b1` and `b2`. Rerun a similar prior predictive simulation.

## Optional Challenge (Not graded)

Return to `data(cherry_blossoms)` and model the association between blossom date (`day`) and March temperature (`temp`). Note that there are many missing values in both variables. You may consider a linear model, a polynomial, or a spline on temperature. How well does temperature trend predict the blossom trend?
