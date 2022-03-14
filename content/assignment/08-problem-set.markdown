---
title: Problem Set 8
date: "2022-03-14"
menu:
  assignment:
    parent: Problem sets
    weight: 8
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is **optional** and not necessary to be submitted.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA4CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzICoqb3B0aW9uYWwqKiBhbmQgbm90IG5lY2Vzc2FyeSB0byBiZSBzdWJtaXR0ZWQuCgojIyBRdWVzdGlvbiAxCgpUaGUgZGF0YSBpbiBgZGF0YShiYW5nbGFkZXNoKWAgYXJlIDE5MzQgd29tZW4gZnJvbSB0aGUgMTk4OSBCYW5nbGFkZXNoIEZlcnRpbGl0eSBTdXJ2ZXkuIEZvciBlYWNoIHdvbWFuLCB3ZSBrbm93IHdoaWNoIGRpc3RyaWN0IHNoZSBsaXZlZCBpbiwgaGVyIG51bWJlciBvZiBsaXZpbmcuY2hpbGRyZW4sIGhlciBhZ2UuY2VudGVyZWQsIHdoZXRoZXIgc2hlIGxpdmVkIGluIGFuIHVyYmFuIGNlbnRlciwgYW5kIGZpbmFsbHkgd2hldGhlciBvciBub3Qgc2hlIHVzZWQgY29udHJhY2VwdGlvbiAoYHVzZS5jb250cmFjZXB0aW9uYCkuCgpJbiB0aGlzIGZpcnN0IHByb2JsZW0sIEkgb25seSB3YW50IHlvdSB0byBpbnZlc3RpZ2F0ZSB0aGUgcHJvcG9ydGlvbiBvZiB3b21lbiB1c2luZyBjb250cmFjZXB0aW9uIGluIGVhY2ggZGlzdHJpY3QuIFVzZSBwYXJ0aWFsIHBvb2xpbmcgKHZhcnlpbmcgZWZmZWN0cykuIFRoZW4gY29tcGFyZSB0aGUgdmFyeWluZyBlZmZlY3QgZXN0aW1hdGVzIHRvIHRoZSByYXcgZW1waXJpY2FsIHByb3BvcnRpb24gaW4gZWFjaCBkaXN0cmljdC4gRXhwbGFpbiB0aGUgZGlmZmVyZW5jZXMgYmV0d2VlbiB0aGUgZXN0aW1hdGVzIGFuZCB0aGUgZGF0YS4KCk5vdGUgdGhhdCBkaXN0cmljdCBudW1iZXIgNTQgaXMgYWJzZW50IGluIHRoZSBkYXRhLiBUaGlzIGNhdXNlcyBzb21lIHByb2JsZW1zIGluIGluZGV4aW5nIHRoZSBwYXJhbWV0ZXJzLiBUaGUgc2ltcGxlc3QgZml4IGlzIGp1c3QgdG8gdGVsbCB1bGFtIG1hbnVhbGx5IGhvdyBsb25nIHRoZSB2ZWN0b3Igc2hvdWxkIGJlLCBsaWtlIHRoaXM6IGB2ZWN0b3JbNjFdOmEgfiBub3JtYWwoYWJhcixzaWdtYSlgLiAKClBheSBzcGVjaWFsIGF0dGVudGlvbiB0byBkaXN0cmljdCBudW1iZXIgNTTigJlzIGVzdGltYXRlLgoKYGBge3J9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgojIyBRdWVzdGlvbiAyCgpGaXJzdCwgZHJhdyBhIERBRyB0aGF0IGluY2x1ZGVzIGFsbCBmaXZlIHZhcmlhYmxlczogKDEpIFVzZXMgY29udHJhY2VwdGlvbiBDICgyKSBBZ2UgQSAoMykgQ2hpbGRyZW4gSyAoNCkgVXJiYW4gVSAoNSkgRGlzdHJpY3QgRC4gCgpZb3UgZG9u4oCZdCBoYXZlIHRvIGJlIGFuIGV4cGVydCBvbiBmZXJ0aWxpdHkuIEJ1dCBkbyB0aGluayBhYm91dCB3aGljaCB2YXJpYWJsZXMgY2FuIGluZmx1ZW5jZSB3aGljaCBvdGhlciB2YXJpYWJsZXMuCgpTZWNvbmQsIGRlc2lnbiBhbiBlc3RpbWF0aW9uIHN0cmF0ZWd5IHRvIGlkZW50aWZ5IGJvdGggdGhlIHRvdGFsIGFuZCBkaXJlY3QgY2F1c2FsIGVmZmVjdHMgb2YgbGl2aW5nIGluIGFuIHVyYmFuIGNlbnRlciBvbiBjb250cmFjZXB0aXZlIHVzZS4gVGhvc2UgYXJlIHlvdXIgZXN0aW1hbmRzLgoKQ29uc2lkZXIgY2F1c2FsIHJlbGF0aW9uc2hpcHMgYW1vbmcgdGhlIHZhcmlhYmxlcy4gVGhlbiB1c2UgeW91ciBEQUcgdG8ganVzdGlmeSBhbiBhZGp1c3RtZW50IHNldCB0aGF0IHdpbGwgeWllbGQgdGhlIGVzdGltYXRlIG9mIHRoZSBjYXVzYWwgZWZmZWN0IG9mIHVyYmFuIGxpdmluZyBvbiBjb250cmFjZXB0aXZlIHVzZS4KCkRvIG5vdCBydW4gYSBzdGF0aXN0aWNhbCBtb2RlbCAoeWV0KS4gSSBqdXN0IHdhbnQgeW91IHRvIHRyeSB0byBkZXNpZ24gYW4gYW5hbHlzaXMuIFRoZXJlIGlzIG5vIGZpcm0gcmlnaHQgYW5zd2VyLiBKdXN0IGFwcGx5IHRoZSBiYWNrZG9vciBjcml0ZXJpb24gYW5kIHJ1bGVzIG9mIGQtc2VwYXJhdGlvbiAodGhlIGVsZW1lbnRhbCBjb25mb3VuZHMpIGNvcnJlY3RseSB0byB0aGUgREFHIHlvdSBkZXNpZ24KCmBgYHtyfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCmBgYAoKIyMgUXVlc3Rpb24gMwoKTm93IGJ1aWxkIG9uZSBvciBtb3JlIHN0YXRpc3RpY2FsIG1vZGVscyB0byBlc3RpbWF0ZSB0aGUgdG90YWwgYW5kIHRoZSBkaXJlY3QgY2F1c2FsIGVmZmVjdHMgb2YgdXJiYW4gbGl2aW5nIG9uIGNvbnRyYWNlcHRpdmUgdXNlLiBBZ2FpbiBpbmNsdWRlIGRpc3RyaWN0IGFzIGEgc2ltcGxlIHZhcnlpbmcgZWZmZWN0IChhcyBpbiBwcm9ibGVtIDEpIHNvIHRoYXQgZWFjaCBkaXN0cmljdCBoYXMgaXRzIG93biBhdmVyYWdlIGNvbnRyYWNlcHRpdmUgdXNlLiBZb3UgbWF5IGFsc28gd2FudCB0byBzdHJhdGlmeSB0aGUgZWZmZWN0IG9mIHVyYmFuIGxpdmluZyBieSBkaXN0cmljdC4gSWYgeW91IGRvLCB0aGluayBjYXJlZnVsbHkgYWJvdXQgaG93IHRvIGRvIHRoaXMgc3RhdGlzdGljYWxseS4KCmBgYHtyfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCmBgYAo=" download="08-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

## Question 1

The data in `data(bangladesh)` are 1934 women from the 1989 Bangladesh Fertility Survey. For each woman, we know which district she lived in, her number of living.children, her age.centered, whether she lived in an urban center, and finally whether or not she used contraception (`use.contraception`).

In this first problem, I only want you to investigate the proportion of women using contraception in each district. Use partial pooling (varying effects). Then compare the varying effect estimates to the raw empirical proportion in each district. Explain the differences between the estimates and the data.

Note that district number 54 is absent in the data. This causes some problems in indexing the parameters. The simplest fix is just to tell ulam manually how long the vector should be, like this: `vector[61]:a ~ normal(abar,sigma)`.

Pay special attention to district number 54’s estimate.

``` r
# type in your code here
```

## Question 2

First, draw a DAG that includes all five variables: (1) Uses contraception C (2) Age A (3) Children K (4) Urban U (5) District D.

You don’t have to be an expert on fertility. But do think about which variables can influence which other variables.

Second, design an estimation strategy to identify both the total and direct causal effects of living in an urban center on contraceptive use. Those are your estimands.

Consider causal relationships among the variables. Then use your DAG to justify an adjustment set that will yield the estimate of the causal effect of urban living on contraceptive use.

Do not run a statistical model (yet). I just want you to try to design an analysis. There is no firm right answer. Just apply the backdoor criterion and rules of d-separation (the elemental confounds) correctly to the DAG you design

``` r
# type in your code here
```

## Question 3

Now build one or more statistical models to estimate the total and the direct causal effects of urban living on contraceptive use. Again include district as a simple varying effect (as in problem 1) so that each district has its own average contraceptive use. You may also want to stratify the effect of urban living by district. If you do, think carefully about how to do this statistically.

``` r
# type in your code here
```
