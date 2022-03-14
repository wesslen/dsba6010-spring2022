---
title: Problem Set 7
date: "2022-03-14"
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

Step 1: Download this file locally.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA3CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBBcHJpbCA0LCAyMDIyIGF0IDExOjU5YW0uCgotICoqTmFtZSoqOgotICoqVU5DQyBJRCoqOiAKLSAqKk90aGVyIHN0dWRlbnQgd29ya2VkIHdpdGggKG9wdGlvbmFsKSoqOgoKIyMgUXVlc3Rpb24gMQoKQ29uZHVjdCBhIHByaW9yIHByZWRpY3RpdmUgc2ltdWxhdGlvbiBmb3IgdGhlIFJlZWRmcm9nIG1vZGVsLiBCeSB0aGlzIEkgbWVhbiB0byBzaW11bGF0ZSB0aGUgcHJpb3IgZGlzdHJpYnV0aW9uIG9mIHRhbmsgc3Vydml2YWwgcHJvYmFiaWxpdGllcyAkXGFscGhhX3tqfSRdLiAKClN0YXJ0IGJ5IHVzaW5nIHRoZXNlIHByaW9yczoKCiRcYWxwaGFfe2p9IFxzaW0gTm9ybWFsKFxiYXJ7XGFscGhhfSxcc2lnbWEpJAoKJFxiYXJ7XGFscGhhfSBcc2ltIE5vcm1hbCgwLCAxKSQKCiRcc2lnbWEgXHNpbSBFeHBvbmVudGlhbCgxKSQKCkJlIHN1cmUgdG8gdHJhbnNmb3JtIHRoZSAkXGFscGhhX3tqfSQgdmFsdWVzIHRvIHRoZSBwcm9iYWJpbGl0eSBzY2FsZSBmb3IgcGxvdHRpbmcgYW5kIHN1bW1hcnkuCgpIb3cgZG9lcyBpbmNyZWFzaW5nIHRoZSB3aWR0aCBvZiB0aGUgcHJpb3Igb24gz4MgY2hhbmdlIHRoZSBwcmlvciBkaXN0cmlidXRpb24gb2YgJFxhbHBoYV97an0kPwoKWW91IG1pZ2h0IHRyeSBFeHBvbmVudGlhbCgxMCkgYW5kIEV4cG9uZW50aWFsKDAuMSkgZm9yIGV4YW1wbGUuCgpgYGB7cn0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCiMjIFF1ZXN0aW9uIDIKClJldmlzaXQgdGhlIFJlZWRmcm9nIHN1cnZpdmFsIGRhdGEsIGBkYXRhKHJlZWRmcm9ncylgLiBTdGFydCB3aXRoIHRoZSB2YXJ5aW5nIGVmZmVjdHMgbW9kZWwgZnJvbSB0aGUgYm9vayBhbmQgbGVjdHVyZS4gVGhlbiBtb2RpZnkgaXQgdG8gZXN0aW1hdGUgdGhlIGNhdXNhbCBlZmZlY3RzIG9mIHRoZSB0cmVhdG1lbnQgdmFyaWFibGVzIHByZWQgYW5kIHNpemUsIGluY2x1ZGluZyBob3cgc2l6ZSBtaWdodCBtb2RpZnkgdGhlIGVmZmVjdCBvZiBwcmVkYXRpb24uIEFuIGVhc3kgYXBwcm9hY2ggaXMgdG8gZXN0aW1hdGUgYW4gZWZmZWN0IGZvciBlYWNoIGNvbWJpbmF0aW9uIG9mIHByZWQgYW5kIHNpemUuIEp1c3RpZnkgeW91ciBtb2RlbCB3aXRoIGEgREFHIG9mIHRoaXMgZXhwZXJpbWVudC4KCmBgYHtyfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCmBgYAoKIyMgUXVlc3Rpb24gMwoKTm93IGVzdGltYXRlIHRoZSBjYXVzYWwgZWZmZWN0IG9mIGRlbnNpdHkgb24gc3Vydml2YWwuIENvbnNpZGVyIHdoZXRoZXIgcHJlZCBtb2RpZmllcyB0aGUgZWZmZWN0IG9mIGRlbnNpdHkuIFRoZXJlIGFyZSBzZXZlcmFsIGdvb2Qgd2F5cyB0byBpbmNsdWRlIGRlbnNpdHkgaW4geW91ciBCaW5vbWlhbCBHTE0uIFlvdSBjb3VsZCB0cmVhdCBpdCBhcyBhIGNvbnRpbnVvdXMgcmVncmVzc2lvbiB2YXJpYWJsZSAocG9zc2libHkgc3RhbmRhcmRpemVkKS4gT3IgeW91IGNvdWxkIGNvbnZlcnQgaXQgdG8gYW4gb3JkZXJlZCBjYXRlZ29yeSAod2l0aCB0aHJlZSBsZXZlbHMpLiAKCkNvbXBhcmUgdGhlICRcc2lnbWEkICh0YW5rIHN0YW5kYXJkIGRldmlhdGlvbikgcG9zdGVyaW9yIGRpc3RyaWJ1dGlvbiB0byAkXHNpZ21hJCBmcm9tIHlvdXIgbW9kZWwgaW4gUHJvYmxlbSAyLiBIb3cgYXJlIHRoZXkgZGlmZmVyZW50PyBXaHk/CgpgYGB7cn0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAK" download="07-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1467898).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

## Question 1

Conduct a prior predictive simulation for the Reedfrog model. By this I mean to simulate the prior distribution of tank survival probabilities `\(\alpha_{j}\)`\].

Start by using these priors:

`\(\alpha_{j} \sim Normal(\bar{\alpha},\sigma)\)`

`\(\bar{\alpha} \sim Normal(0, 1)\)`

`\(\sigma \sim Exponential(1)\)`

Be sure to transform the `\(\alpha_{j}\)` values to the probability scale for plotting and summary.

How does increasing the width of the prior on σ change the prior distribution of `\(\alpha_{j}\)`?

You might try Exponential(10) and Exponential(0.1) for example.

``` r
# type in your code here
```

## Question 2

Revisit the Reedfrog survival data, `data(reedfrogs)`. Start with the varying effects model from the book and lecture. Then modify it to estimate the causal effects of the treatment variables pred and size, including how size might modify the effect of predation. An easy approach is to estimate an effect for each combination of pred and size. Justify your model with a DAG of this experiment.

``` r
# type in your code here
```

## Question 3

Now estimate the causal effect of density on survival. Consider whether pred modifies the effect of density. There are several good ways to include density in your Binomial GLM. You could treat it as a continuous regression variable (possibly standardized). Or you could convert it to an ordered category (with three levels).

Compare the `\(\sigma\)` (tank standard deviation) posterior distribution to `\(\sigma\)` from your model in Problem 2. How are they different? Why?

``` r
# type in your code here
```
