---
title: Problem Set 6
date: "2022-03-13"
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

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA2CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKbWVudToKICBhc3NpZ25tZW50OgogICAgcGFyZW50OiBQcm9ibGVtIHNldHMKICAgIHdlaWdodDogNgp0eXBlOiBkb2NzCnRvYzogdHJ1ZQotLS0KClRoaXMgcHJvYmxlbSBzZXQgaXMgKipvcHRpb25hbCoqIGFuZCBub3QgbmVjZXNzYXJ5IHRvIGJlIHN1Ym1pdHRlZC4gV2Ugd2lsbCB0cnkgdG8gd29yayBvbiBpdCBpbiBjbGFzcyBvbiBNYXJjaCAxNC4KCiMjIFF1ZXN0aW9uIDEKClRoZSBkYXRhIGluIGBkYXRhKE5XT0dyYW50cylgIGFyZSBvdXRjb21lcyBmb3Igc2NpZW50aWZpYyBmdW5kaW5nIGFwcGxpY2F0aW9ucyBmb3IgdGhlIE5ldGhlcmxhbmRzIE9yZ2FuaXphdGlvbiBmb3IgU2NpZW50aWZpYyBSZXNlYXJjaCAoTldPKSBmcm9tIDIwMTAtMjAxMi4gVGhlc2UgZGF0YSBoYXZlIGEgdmVyeSBzaW1pbGFyIHN0cnVjdHVyZSB0byB0aGUgVUNCQWRtaXQgZGF0YSBkaXNjdXNzZWQgaW4gQ2hhcHRlciAxMS4gCgpEcmF3IGEgREFHIGFuZCBlc3RpbWF0ZSB0aGUgKipUT1RBTCoqIGNhdXNhbCBlZmZlY3Qgb2YgZ2VuZGVyIG9uIGdyYW50IGF3YXJkcy4gCgpCZSBzdXJlIHRvIGNoZWNrIGNvbnZlcmdlbmNlIHN0YXRzIChgdHJhY2VwbG90KClgLCBgdHJhbmtwbG90KClgIGFuZCBSaGF0KQoKYGBge3J9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgojIyBRdWVzdGlvbiAyCgpOb3cgZXN0aW1hdGUgdGhlICoqRElSRUNUKiogY2F1c2FsIGVmZmVjdCBvZiBnZW5kZXIgb24gZ3JhbnQgYXdhcmRzLiBDb21wYXJlIHRoZSBhdmVyYWdlIGRpcmVjdCBjYXVzYWwgZWZmZWN0IG9mIGdlbmRlciwgd2VpZ2h0aW5nIGVhY2ggZGlzY2lwbGluZSBpbiBwcm9wb3J0aW9uIHRvIHRoZSBudW1iZXIgb2YgYXBwbGljYXRpb25zIGluIHRoZSBzYW1wbGUuIFJlZmVyIHRvIHRoZSBtYXJnaW5hbCBlZmZlY3QgZXhhbXBsZSBpbiBMZWN0dXJlIDkgZm9yIGhlbHAuCgpgYGB7cn0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCiMjIFF1ZXN0aW9uIDMKCkNvbnNpZGVyaW5nIHRoZSB0b3RhbCBlZmZlY3QgKHByb2JsZW0gMSkgYW5kIGRpcmVjdCBlZmZlY3QgKHByb2JsZW0gMikgb2YgZ2VuZGVyLCB3aGF0IGNhdXNlcyBjb250cmlidXRlIHRvIHRoZSBhdmVyYWdlIGRpZmZlcmVuY2UgYmV0d2VlbiB3b21lbiBhbmQgbWVuIGluIGF3YXJkIHJhdGUgaW4gdGhpcyBzYW1wbGU/IEl0IGlzIG5vdCBuZWNlc3NhcnkgdG8gc2F5IHdoZXRoZXIgb3Igbm90IHRoZXJlIGlzIGV2aWRlbmNlIG9mIGRpc2NyaW1pbmF0aW9uLiBTaW1wbHkgZXhwbGFpbiBob3cgdGhlIGRpcmVjdCBlZmZlY3RzIHlvdSBoYXZlIGVzdGltYXRlZCBtYWtlIHNlbnNlIChvciBub3QpIG9mIHRoZSB0b3RhbCBlZmZlY3QuCgpgYGB7cn0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAK" download="06-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

## Question 1

The data in `data(NWOGrants)` are outcomes for scientific funding applications for the Netherlands Organization for Scientific Research (NWO) from 2010-2012. These data have a very similar structure to the UCBAdmit data discussed in Chapter 11.

Draw a DAG and estimate the **TOTAL** causal effect of gender on grant awards.

Be sure to check convergence stats (`traceplot()`, `trankplot()` and Rhat)

``` r
# type in your code here
```

## Question 2

Now estimate the **DIRECT** causal effect of gender on grant awards. Compare the average direct causal effect of gender, weighting each discipline in proportion to the number of applications in the sample. Refer to the marginal effect example in Lecture 9 for help.

``` r
# type in your code here
```

## Question 3

Considering the total effect (problem 1) and direct effect (problem 2) of gender, what causes contribute to the average difference between women and men in award rate in this sample? It is not necessary to say whether or not there is evidence of discrimination. Simply explain how the direct effects you have estimated make sense (or not) of the total effect.

``` r
# type in your code here
```
