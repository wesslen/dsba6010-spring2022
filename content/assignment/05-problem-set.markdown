---
title: Problem Set 5
date: "2022-03-02"
menu:
  assignment:
    parent: Problem sets
    weight: 5
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on March 14, 2022 at 11:59am.

Step 1: Download this file locally.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA1CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBNYXJjaCAxNCwgMjAyMiBhdCAxMTo1OWFtLgoKLSAqKk5hbWUqKjoKLSAqKlVOQ0MgSUQqKjogCi0gKipPdGhlciBzdHVkZW50IHdvcmtlZCB3aXRoIChvcHRpb25hbCkqKjoKCllvdSBjYW4gdXNlIE1DTUMgdG8gc29sdmUgdGhlc2UgcHJvYmxlbXMsIGlmIHlvdSBsaWtlLiBCdXQgaXTigJlzIG5vdCByZXF1aXJlZC4KCiMjIFF1ZXN0aW9uIDEKClJldmlzaXQgdGhlIG1hcnJpYWdlLCBhZ2UsIGFuZCBoYXBwaW5lc3MgY29sbGlkZXIgYmlhcyBleGFtcGxlIGZyb20gQ2hhcHRlciA2LiBSdW4gbW9kZWxzIGBtNi45YCBhbmQgYG02LjEwYCBhZ2FpbiAocGFnZXMgMTc44oCTMTc5KS4gQ29tcGFyZSB0aGVzZSB0d28gbW9kZWxzIHVzaW5nIGJvdGggUFNJUyBhbmQgV0FJQy4gV2hpY2ggbW9kZWwgaXMgZXhwZWN0ZWQgdG8gbWFrZSBiZXR0ZXIgcHJlZGljdGlvbnMsIGFjY29yZGluZyB0byB0aGVzZSBjcml0ZXJpYT8gT24gdGhlIGJhc2lzIG9mIHRoZSBjYXVzYWwgbW9kZWwsIGhvdyBzaG91bGQgeW91IGludGVycHJldCB0aGUgcGFyYW1ldGVyIGVzdGltYXRlcyBmcm9tIHRoZSBtb2RlbCBwcmVmZXJyZWQgYnkgUFNJUyBhbmQgV0FJQz8KCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgojIyBRdWVzdGlvbiAyCgpSZWNvbnNpZGVyIHRoZSB1cmJhbiBmb3ggYW5hbHlzaXMgZnJvbSBsYXN0IHdlZWvigJlzIGhvbWV3b3JrLiBPbiB0aGUgYmFzaXMgb2YgUFNJUyBhbmQgV0FJQyBzY29yZXMsIHdoaWNoIGNvbWJpbmF0aW9uIG9mIHZhcmlhYmxlcyBiZXN0IHByZWRpY3RzIGJvZHkgd2VpZ2h0IChXLCB3ZWlnaHQpPyBIb3cgd291bGQgeW91IGludGVycHJldCB0aGUgZXN0aW1hdGVzIGZyb20gdGhlIGJlc3Qgc2NvcmluZyBtb2RlbD8KCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgojIyBPcHRpb25hbCBRdWVzdGlvbiAoTm90IEdyYWRlZCkKCkJ1aWxkIGEgcHJlZGljdGl2ZSBtb2RlbCBvZiB0aGUgcmVsYXRpb25zaGlwIHNob3cgb24gdGhlIGNvdmVyIG9mIHRoZSBib29rLCB0aGUgcmVsYXRpb25zaGlwIGJldHdlZW4gdGhlIHRpbWluZyBvZiBjaGVycnkgYmxvc3NvbXMgYW5kIE1hcmNoIHRlbXBlcmF0dXJlIGluIHRoZSBzYW1lIHllYXIuIFRoZSBkYXRhIGFyZSBmb3VuZCBpbiBgZGF0YShjaGVycnlfYmxvc3NvbXMpYC4gQ29uc2lkZXIgYXQgbGVhc3QgdHdvIGZ1bmN0aW9ucyB0byBwcmVkaWN0IGRveSB3aXRoIHRlbXAuIENvbXBhcmUgdGhlbSB3aXRoIFBTSVMgb3IgV0FJQy4KClN1cHBvc2UgTWFyY2ggdGVtcGVyYXR1cmVzIHJlYWNoIDkgZGVncmVlcyBieSB0aGUgeWVhciAyMDUwLiBXaGF0IGRvZXMgeW91ciBiZXN0IG1vZGVsIHByZWRpY3QgZm9yIHRoZSBwcmVkaWN0aXZlIGRpc3RyaWJ1dGlvbiBvZiB0aGUgZGF5LWluLXllYXIgdGhhdCB0aGUgY2hlcnJ5IHRyZWVzIHdpbGwgYmxvc3NvbT8KCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCg==" download="05-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1415462).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

You can use MCMC to solve these problems, if you like. But it’s not required.

## Question 1

Revisit the marriage, age, and happiness collider bias example from Chapter 6. <a href="http://xcelab.net/rmpubs/sr2/code.txt#:~:text=%23%23%20R%20code%206.21,d2%20)%0Aprecis(m6.10)">Run models `m6.9` and `m6.10` again</a> (pages 178–179). Compare these two models using both PSIS and WAIC. Which model is expected to make better predictions, according to these criteria? On the basis of the causal model, how should you interpret the parameter estimates from the model preferred by PSIS and WAIC?

## Question 2

Reconsider the urban fox analysis from last week’s homework. On the basis of PSIS and WAIC scores, which combination of variables best predicts body weight (W, weight)? How would you interpret the estimates from the best scoring model?

## Optional Question (Not Graded)

Build a predictive model of the relationship show on the cover of the book, the relationship between the timing of cherry blossoms and March temperature in the same year. The data are found in `data(cherry_blossoms)`. Consider at least two functions to predict doy with temp. Compare them with PSIS or WAIC.

Suppose March temperatures reach 9 degrees by the year 2050. What does your best model predict for the predictive distribution of the day-in-year that the cherry trees will blossom?
