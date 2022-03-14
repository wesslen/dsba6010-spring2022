---
title: Problem Set 9
date: "2022-03-14"
menu:
  assignment:
    parent: Problem sets
    weight: 9
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on May 2, 2022 at 11:59am.

Step 1: Download this file locally.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA5CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBNYXkgMiwgMjAyMiBhdCAxMTo1OWFtLgoKLSAqKk5hbWUqKjoKLSAqKlVOQ0MgSUQqKjogCi0gKipPdGhlciBzdHVkZW50IHdvcmtlZCB3aXRoIChvcHRpb25hbCkqKjoKClRoZSBkYXRhIGluIGBkYXRhKEFjaGVodW50aW5nKWAgYXJlIDE0LDM2NCBpbmRpdmlkdWFsIGh1bnRpbmcgdHJpcHMgYnkgMTQ3IG1lbiBhbW9uZyB0aGUgaW5kaWdlbm91cyBBY2jDqSBvZiBQYXJhZ3VheS4gRWFjaCB0cmlwIGhhcyByZWNvcmRlZCB0aGUgaHVudGVy4oCZcyBpZGVudGl0eSAoYW5vbnltaXplZCBpZCksIGh1bnRlcuKAmXMgYWdlIGF0IHRoZSB0aW1lIG9mIHRoZSB0cmlwLCB0aGUgZHVyYXRpb24gaW4gaG91cnMgb2YgdGhlIHRyaXAsIGFuZCB0aGUga2lsb2dyYW1zIG9mIG1lYXQgcmV0dXJuZWQuIE1hbnkgb2YgdGhlIHRyaXBzIGhhdmUgbWlzc2luZyB2YWx1ZXMgZm9yIGR1cmF0aW9uLgoKIyMgUXVlc3Rpb24gMQoKSW4gdGhpcyBmaXJzdCBwcm9ibGVtLCBlc3RpbWF0ZSB0aGUgaW5mbHVlbmNlIG9mIGFnZSBvbiB0aGUgcHJvYmFiaWxpdHkgb2YgdHJpcCBzdWNjZXNzLiBEZWZpbmUg4oCcc3VjY2Vzc+KAnSBhcyBhIHRyaXAgdGhhdCByZXR1cm5zIGFueSBub24temVybyBhbW91bnQgb2YgbWVhdC4gSWdub3JlIGluZGl2aWR1YWwgaHVudGVyIGlkZW50aXRpZXMgZm9yIG5vdy4gWW91IGNhbiB1c2UgYW55IGZ1bmN0aW9uYWwgcmVsYXRpb25zaGlwIGZvciBhZ2UgYW5kIHN1Y2Nlc3MgdGhhdCB5b3UgdGhpbmsgaXMgc2Vuc2libGUsIGJ1dCBiZSBzdXJlIHRvIGp1c3RpZnkgaXQgYW5kIGNoZWNrIGl0IGFnYWluc3QgcG9zdGVyaW9yIHByZWRpY3Rpb25zLgoKYGBge3J9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgojIyBRdWVzdGlvbiAyCgpOb3cgaW5jb3Jwb3JhdGUgaW5kaXZpZHVhbCBodW50ZXIgdmFyeWluZyBlZmZlY3RzIGludG8geW91ciBhbmFseXNpcyBmcm9tIHRoZSBwcmV2aW91cyBwcm9ibGVtLiBBbGxvdyB0aGUgaW5mbHVlbmNlIG9mIGFnZSBvbiBzdWNjZXNzIHRvIHZhcnkgYnkgZWFjaCBpbmRpdmlkdWFsLgoKSG93IG11Y2ggdmFyaWF0aW9uIGluIHN1Y2Nlc3MgaXMgZXhwbGFpbmVkIGJ5IGluZGl2aWR1YWxzIGFuZCBob3cgbXVjaCBieSBhZ2U/CgpgYGB7cn0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCiMjIFF1ZXN0aW9uIDMKCk5vdyBpbmNsdWRlIHRyaXAgZHVyYXRpb24gaW4gdGhlIG1vZGVsIGZyb20gdGhlIHByZXZpb3VzIHByb2JsZW0uIFRoZSBmb2N1cyBpcyBzdGlsbCB0aGUgY2F1c2FsIHJlbGF0aW9uc2hpcCBiZXR3ZWVuIGFnZSBhbmQgc3VjY2VzcywgYnV0IGR1cmF0aW9uIGlzIGEgcG90ZW50aWFsIGNvbXBldGluZyBjYXVzZSBvciBhIG1lZGlhdG9yLCBzbyBpdCBpcyB1c2VmdWwgdG8gaW5jbHVkZSBpdC4gU2luY2UgdGhlcmUgYXJlIG1hbnkgbWlzc2luZyB2YWx1ZXMgZm9yIHRyaXAgZHVyYXRpb24sIGNvbXBhcmUgKDEpIGEgY29tcGxldGUgY2FzZSBhbmFseXNpcywgd2hpY2ggZHJvcHMgYWxsIHRoZSB0cmlwcyB3aXRoIG1pc3NpbmcgZHVyYXRpb25zLCB0byAoMikgYW4gYW5hbHlzaXMgd2hpY2ggdXNlcyBhbGwgMTQsMzY0IHRyaXBzIGFuZCBpbXB1dGVzIG1pc3NpbmcgZHVyYXRpb24gdmFsdWVzIHdoZXJlIG5lY2Vzc2FyeS4gVXNlIGFueSBmdW5jdGlvbmFsIHJlbGF0aW9uc2hpcApiZXR3ZWVuIGR1cmF0aW9uIGFuZCBzdWNjZXNzIHRoYXQgeW91IHRoaW5rIGlzIHNlbnNpYmxlLCBidXQgYmUgc3VyZSB0byBqdXN0aWZ5IGl0IGFuZCBjaGVjayBpdCBhZ2FpbnN0IHBvc3RlcmlvciBwcmVkaWN0aW9ucwoKYGBge3J9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCg==" download="09-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1467904).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

The data in `data(Achehunting)` are 14,364 individual hunting trips by 147 men among the indigenous Aché of Paraguay. Each trip has recorded the hunter’s identity (anonymized id), hunter’s age at the time of the trip, the duration in hours of the trip, and the kilograms of meat returned. Many of the trips have missing values for duration.

## Question 1

In this first problem, estimate the influence of age on the probability of trip success. Define “success” as a trip that returns any non-zero amount of meat. Ignore individual hunter identities for now. You can use any functional relationship for age and success that you think is sensible, but be sure to justify it and check it against posterior predictions.

``` r
# type in your code here
```

## Question 2

Now incorporate individual hunter varying effects into your analysis from the previous problem. Allow the influence of age on success to vary by each individual.

How much variation in success is explained by individuals and how much by age?

``` r
# type in your code here
```

## Question 3

Now include trip duration in the model from the previous problem. The focus is still the causal relationship between age and success, but duration is a potential competing cause or a mediator, so it is useful to include it. Since there are many missing values for trip duration, compare (1) a complete case analysis, which drops all the trips with missing durations, to (2) an analysis which uses all 14,364 trips and imputes missing duration values where necessary. Use any functional relationship
between duration and success that you think is sensible, but be sure to justify it and check it against posterior predictions

``` r
# type in your code here
```
