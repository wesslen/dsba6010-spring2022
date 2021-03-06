---
title: Problem Set 4
date: "2022-02-14"
menu:
  assignment:
    parent: Problem sets
    weight: 4
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on February 28, 2022 at 11:59am.

Step 1: Download this file locally.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA0CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBGZWJydWFyeSAyOCwgMjAyMiBhdCAxMTo1OWFtLgoKLSAqKk5hbWUqKjoKLSAqKlVOQ0MgSUQqKjogCi0gKipPdGhlciBzdHVkZW50IHdvcmtlZCB3aXRoIChvcHRpb25hbCkqKjoKCiMjIFF1ZXN0aW9uIDEKClRoZSBmaXJzdCB0d28gcHJvYmxlbXMgYXJlIGJhc2VkIG9uIHRoZSBzYW1lIGRhdGEuIFRoZSBkYXRhIGluIGBkYXRhKGZveGVzKWAgYXJlIDExNiBmb3hlcyBmcm9tIDMwIGRpZmZlcmVudCB1cmJhbiBncm91cHMgaW4gRW5nbGFuZC4gCgpgYGB7ciB3YXJuaW5nPUZBTFNFLG1lc3NhZ2U9RkFMU0V9CmxpYnJhcnkocmV0aGlua2luZykKZGF0YShmb3hlcykKZDwtIGZveGVzCmhlYWQoZCkKYGBgCgpUaGVzZSBmb3ggZ3JvdXBzIGFyZSBsaWtlIHN0cmVldCBnYW5ncy4gR3JvdXAgc2l6ZSAoYGdyb3Vwc2l6ZWApIHZhcmllcyBmcm9tIDIgdG8gOCBpbmRpdmlkdWFscy4gRWFjaCBncm91cCBtYWludGFpbnMgaXRzIG93biAoYWxtb3N0IGV4Y2x1c2l2ZSkgdXJiYW4gdGVycml0b3J5LiBTb21lIHRlcnJpdG9yaWVzIGFyZSBsYXJnZXIgdGhhbiBvdGhlcnMuIFRoZSBgYXJlYWAgdmFyaWFibGUgZW5jb2RlcyB0aGlzIGluZm9ybWF0aW9uLiBTb21lIHRlcnJpdG9yaWVzIGFsc28gaGF2ZSBtb3JlIGBhdmdmb29kYCB0aGFuIG90aGVycy4gQW5kIGZvb2QgaW5mbHVlbmNlcyB0aGUgYHdlaWdodGAgb2YgZWFjaCBmb3guIEFzc3VtZSB0aGlzIERBRzoKCmBgYHtyIGZpZy5oZWlnaHQ9NCwgZmlnLndpZHRoPTR9CmxpYnJhcnkoZGFnaXR0eSkKCmcgPC0gZGFnaXR0eSgnZGFnIHsKYmI9IjAsMCwxLDEiCkEgW3Bvcz0iMC40NTAsMC4yOTAiXQpGIFtleHBvc3VyZSxwb3M9IjAuMzMzLDAuNDkwIl0KRyBbcG9zPSIwLjUzOSwwLjQ5NSJdClcgW291dGNvbWUscG9zPSIwLjQ0NSwwLjY4NiJdCkEgLT4gRgpGIC0+IEcKRiAtPiBXCkcgLT4gVwp9CgonKQpwbG90KGcpCmBgYAoKd2hlcmUgRiBpcyBgYXZnZm9vZGAsIEcgaXMgYGdyb3Vwc2l6ZWAsIEEgaXMgYGFyZWFgLCBhbmQgVyBpcyBgd2VpZ2h0YC4KCioqUGFydCAxKio6IFVzZSB0aGUgYmFja2Rvb3IgY3JpdGVyaW9uIGFuZCBlc3RpbWF0ZSB0aGUgdG90YWwgY2F1c2FsIGluZmx1ZW5jZSBvZiBBIG9uIEYuIAoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCioqUGFydCAyKio6IFdoYXQgZWZmZWN0IHdvdWxkIGluY3JlYXNpbmcgdGhlIGFyZWEgb2YgYSB0ZXJyaXRvcnkgaGF2ZSBvbiB0aGUgYW1vdW50IG9mIGZvb2QgaW5zaWRlIGl0PwoKW1dyaXRlIGFuc3dlciBoZXJlIGluIHNlbnRlbmNlc10KCiMjIFF1ZXN0aW9uIDIKCk5vdyBpbmZlciBib3RoIHRoZSAqKnRvdGFsKiogYW5kICoqZGlyZWN0KiogY2F1c2FsIGVmZmVjdHMgb2YgYWRkaW5nIGZvb2QgRiB0byBhIHRlcnJpdG9yeSBvbiB0aGUgd2VpZ2h0IFcgb2YgZm94ZXMuIFdoaWNoIGNvdmFyaWF0ZXMgZG8geW91IG5lZWQgdG8gYWRqdXN0IGZvciBpbiBlYWNoIGNhc2U/IEluIGxpZ2h0IG9mIHlvdXIgZXN0aW1hdGVzIGZyb20gdGhpcyBwcm9ibGVtIGFuZCB0aGUgcHJldmlvdXMgb25lLCB3aGF0IGRvIHlvdSB0aGluayBpcyBnb2luZyBvbiB3aXRoIHRoZXNlIGZveGVzPyBGZWVsIGZyZWUgdG8gc3BlY3VsYXRl4oCUYWxsIHRoYXQgbWF0dGVycyBpcyB0aGF0IHlvdSBqdXN0aWZ5IHlvdXIgc3BlY3VsYXRpb24uCgoKCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgVG90YWwgY2F1c2FsIGVmZmVjdDogdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgoKCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgRm9yIHRoZSBkaXJlY3QgY2F1c2FsIGVmZmVjdDogdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgojIyBRdWVzdGlvbiAzCgpSZWNvbnNpZGVyIHRoZSBUYWJsZSAyIEZhbGxhY3kgZXhhbXBsZSAoZnJvbSBMZWN0dXJlIDYpLCB0aGlzIHRpbWUgd2l0aCBhbiB1bm9ic2VydmVkIGNvbmZvdW5kIFUgdGhhdCBpbmZsdWVuY2VzIGJvdGggc21va2luZyBTIGFuZCBzdHJva2UgWS4gSGVyZeKAmXMgdGhlIG1vZGlmaWVkIERBRzoKCmBgYHtyIGVjaG89RkFMU0UsIG91dC53aWR0aCA9ICc1MCUnfQojIHJ1biB0aGlzIGNodW5rIHRvIHZpZXcgdGhlIGltYWdlCmtuaXRyOjppbmNsdWRlX2dyYXBoaWNzKCJodHRwczovL3Jhdy5naXRodWJ1c2VyY29udGVudC5jb20vd2Vzc2xlbi9kc2JhNjAxMC1zcHJpbmcyMDIyL21hc3Rlci9zdGF0aWMvaW1nL2Fzc2lnbm1lbnRzLzA0LXByb2JsZW0tc2V0LzA0LXByb2JsZW0tc2V0LTAucG5nIikKYGBgCgpQYXJ0IDE6IHVzZSB0aGUgYmFja2Rvb3IgY3JpdGVyaW9uIHRvIGRldGVybWluZSBhbiBhZGp1c3RtZW50IHNldCB0aGF0IGFsbG93cyB5b3UgdG8gZXN0aW1hdGUgdGhlIGNhdXNhbCBlZmZlY3Qgb2YgWCBvbiBZLCBpLmUuIFAoWXxkbyhYKSkuIAoKRm9yIHRoaXMgZXhlcmNpc2UsIHlvdSBjYW4gdXNlIFtkYWdpdHR5Lm5ldF0oaHR0cDovL3d3dy5kYWdpdHR5Lm5ldC9kYWdzLmh0bWwpLgoKU3RlcCAxOiBJbnB1dCB5b3VyIERBRyBpbnRvIERhZ2l0dHkubmV0IGFuZCBjb3B5L3Bhc3RlIHlvdXIgcmVzdWx0cyBoZXJlOgoKYGBge3IgZXZhbD1GQUxTRX0KIyBpbnNlcnQgY29kZSBoZXJlCgpnIDwtIGRhZ2l0dHkoJwogICAgICAgICAgIyBjb3B5L3Bhc3RlIGRhZ2l0dHkubmV0IGNvZGUgZm9yIERBRyBoZXJlCiAgICAgICAgICAgICAnKQpgYGAKClN0ZXAgMjogV2hhdCBpcyB0aGUgYWRqdXN0bWVudCBzZXQgdG8gZXN0aW1hdGUgdGhlIGNhdXNhbCBlZmZlY3Qgb2YgWCBvbiBZPwoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyBmaW5kIGFkanVzdG1lbnQgc2V0OiB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKClBhcnQgMjogRXhwbGFpbiB0aGUgcHJvcGVyIGludGVycHJldGF0aW9uIG9mIGVhY2ggY29lZmZpY2llbnQgaW1wbGllZCBieSB0aGUgcmVncmVzc2lvbiBtb2RlbCB0aGF0IGNvcnJlc3BvbmRzIHRvIHRoZSBhZGp1c3RtZW50IHNldC4gV2hpY2ggY29lZmZpY2llbnRzIChzbG9wZXMpIGFyZSBjYXVzYWwgYW5kIHdoaWNoIGFyZSBub3Q/IFRoZXJlIGlzIG5vIG5lZWQgdG8gZml0IGFueSBtb2RlbHMuIEp1c3QgdGhpbmsgdGhyb3VnaCB0aGUgaW1wbGljYXRpb25zLgoKW1dyaXRlIGFuc3dlciBoZXJlIGluIHNlbnRlbmNlc10=" download="04-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1415435).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

## Question 1

The first two problems are based on the same data. The data in `data(foxes)` are 116 foxes from 30 different urban groups in England.

``` r
library(rethinking)
data(foxes)
d<- foxes
head(d)
```

``` language-r
##   group avgfood groupsize area weight
## 1     1    0.37         2 1.09   5.02
## 2     1    0.37         2 1.09   2.84
## 3     2    0.53         2 2.05   5.33
## 4     2    0.53         2 2.05   6.07
## 5     3    0.49         2 2.12   5.85
## 6     3    0.49         2 2.12   3.25
```

These fox groups are like street gangs. Group size (`groupsize`) varies from 2 to 8 individuals. Each group maintains its own (almost exclusive) urban territory. Some territories are larger than others. The `area` variable encodes this information. Some territories also have more `avgfood` than others. And food influences the `weight` of each fox. Assume this DAG:

<img src="/assignment/04-problem-set_files/figure-html/unnamed-chunk-3-1.png" width="192" />

where F is `avgfood`, G is `groupsize`, A is `area`, and W is `weight`.

**Part 1**: Use the backdoor criterion and estimate the total causal influence of A on F.

**Part 2**: What effect would increasing the area of a territory have on the amount of food inside it?

\[Write answer here in sentences\]

## Question 2

Now infer both the **total** and **direct** causal effects of adding food F to a territory on the weight W of foxes. Which covariates do you need to adjust for in each case? In light of your estimates from this problem and the previous one, what do you think is going on with these foxes? Feel free to speculate???all that matters is that you justify your speculation.

## Question 3

Reconsider the Table 2 Fallacy example (from Lecture 6), this time with an unobserved confound U that influences both smoking S and stroke Y. Here???s the modified DAG:

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/assignments/04-problem-set/04-problem-set-0.png" width="50%" />

Part 1: use the backdoor criterion to determine an adjustment set that allows you to estimate the causal effect of X on Y, i.e.??P(Y\|do(X)).

For this exercise, you can use [dagitty.net](http://www.dagitty.net/dags.html).

Step 1: Input your DAG into Dagitty.net and copy/paste your results here:

``` r
# insert code here

g <- dagitty('
          # copy/paste dagitty.net code for DAG here
             ')
```

Step 2: What is the adjustment set to estimate the causal effect of X on Y?

Part 2: Explain the proper interpretation of each coefficient implied by the regression model that corresponds to the adjustment set. Which coefficients (slopes) are causal and which are not? There is no need to fit any models. Just think through the implications.

\[Write answer here in sentences\]
