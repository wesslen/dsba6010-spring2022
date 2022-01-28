---
title: Problem Set 2
date: "2022-01-27"
menu:
  assignment:
    parent: Problem sets
    weight: 1
type: docs
toc: true
---

<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on February 7, 2022 at 11:59am.

Step 1: Download this file locally.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCAyCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBGZWJydWFyeSA3LCAyMDIyIGF0IDExOjU5YW0uCgotICoqTmFtZSoqOgotICoqVU5DQyBJRCoqOiAKLSAqKk90aGVyIHN0dWRlbnQgd29ya2VkIHdpdGggKG9wdGlvbmFsKSoqOgoKIyMgUXVlc3Rpb24gMQoKQ29uc3RydWN0IGEgbGluZWFyIHJlZ3Jlc3Npb24gb2Ygd2VpZ2h0IGFzIHByZWRpY3RlZCBieSBoZWlnaHQsIHVzaW5nIHRoZSBhZHVsdHMgKGFnZSAxOCBvciBncmVhdGVyKSBmcm9tIHRoZSBIb3dlbGwxIGRhdGFzZXQuIFRoZSBoZWlnaHRzIGxpc3RlZCBiZWxvdyB3ZXJlIHJlY29yZGVkIGluIHRoZSAhS3VuZyBjZW5zdXMsIGJ1dCB3ZWlnaHRzIHdlcmUgbm90IHJlY29yZGVkIGZvciB0aGVzZSBpbmRpdmlkdWFscy4gCgpQcm92aWRlIHByZWRpY3RlZCB3ZWlnaHRzIGFuZCA4OSUgY29tcGF0aWJpbGl0eSBpbnRlcnZhbHMgZm9yIGVhY2ggb2YgdGhlc2UgaW5kaXZpZHVhbHMuIEZpbGwgaW4gdGhlIHRhYmxlIGJlbG93LCB1c2luZyBtb2RlbC1iYXNlZCBwcmVkaWN0aW9ucy4KCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgp8IEluZGl2aWR1YWwgfCBoZWlnaHQgfCBleHBlY3RlZCB3ZWlnaHQgfCA4OSUgaW50ZXJ2YWwgfAp8LS0tLS0tLS0tLS0tfC0tLS0tLS0tfC0tLS0tLS0tLS0tLS0tLS0tfC0tLS0tLS0tLS0tLS0tfAp8IDEgICAgICAgICAgfCAxNDAgICAgfCAgICAgICAgICAgICAgICAgfCAgICAgICAgICAgICAgfAp8IDIgICAgICAgICAgfCAxNTAgICAgfCAgICAgICAgICAgICAgICAgfCAgICAgICAgICAgICAgfAp8IDMgICAgICAgICAgfCAxNjAgICAgfCAgICAgICAgICAgICAgICAgfCAgICAgICAgICAgICAgfAp8IDQgICAgICAgICAgfCAxNzUgICAgfCAgICAgICAgICAgICAgICAgfCAgICAgICAgICAgICAgfAoKIyMgUXVlc3Rpb25zIDItNAoKQSBzYW1wbGUgb2Ygc3R1ZGVudHMgaXMgbWVhc3VyZWQgZm9yIGhlaWdodCBlYWNoIHllYXIgZm9yIDMgeWVhcnMuIEFmdGVyIHRoZSB0aGlyZCB5ZWFyLCB5b3Ugd2FudCB0byBmaXQgYSBsaW5lYXIgcmVncmVzc2lvbiBwcmVkaWN0aW5nIGhlaWdodCAoaW4gY2VudGltZXRlcnMpIHVzaW5nIHllYXIgYXMgYSBwcmVkaWN0b3IuIAoKIyMjIFF1ZXN0aW9uIDI6CgotIFdyaXRlIGRvd24gYSBtYXRoZW1hdGljYWwgbW9kZWwgZGVmaW5pdGlvbiBmb3IgdGhpcyByZWdyZXNzaW9uLCB1c2luZyBhbnkgdmFyaWFibGUgbmFtZXMgYW5kIHByaW9ycyB5b3UgY2hvb3NlLiBZb3UgZG9uJ3QgbmVlZCB0byBydW4gc2luY2UgeW91IHdvbid0IGhhdmUgdGhlIGRhdGEuIFlvdSBtYXkgYWxzbyB3cml0ZSBkb3duIHlvdXIgZXF1YXRpb24gYW5kIHRoZW4gdXBsb2FkIGFuIGltYWdlLgoKVG8gaGVscCB5b3UsIHRoaXMgaXMgd2hhdCB3ZSdsbCBkZWZpbmUgYXMgdGhlIG91dGNvbWUgdmFyaWFibGUgaGVpZ2h0ICRoX3tpan0kLCB3aGVyZSBcaXR7aX0gaXMgdGhlIHN0dWRlbnQgXGl0e2l9IGFuZCBcaXR7an0gaXMgdGhlIHllYXIgXGl0e2p9LiBGb3IgaGVscCB3aXRoIExhVGVYIGVxdWF0aW9ucywgc2VlIHRoaXMKCiRoX3tpan0gXHNpbSBOb3JtYWwodV97aWp9LFxzaWdtYSkkCgojIyMgUXVlc3Rpb24gMwoKLSBSdW4gcHJpb3IgcHJlZGljdGl2ZSBzaW11bGF0aW9uIGFuZCBkZWZlbmQgeW91ciBjaG9pY2Ugb2YgcHJpb3JzLgoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KICAjIHR5cGUgaW4geW91ciBzaW11bGF0aW9uIAogICMgc2VlIGNvZGUgNC4zOCBhbmQgNC4zOSBleGFtcGxlLCB1c2UgeWVhciBhcyB4IGF4aXMgKHByZWRpY3RvcikKYGBgCgojIyMgUXVlc3Rpb24gNAoKTm93IHN1cHBvc2UgSSB0ZWxsIHlvdSB0aGF0IHRoZSBzdHVkZW50cyB3ZXJlIDggdG8gMTAgeWVhcnMgb2xkIChzbyA4IHllYXIgMSwgOSB5ZWFyIDIsIGV0Yy4pLiBXaGF0IGRvIHlvdSBleHBlY3Qgb2YgdGhlIHRyZW5kIG9mIHN0dWRlbnRzJyBoZWlnaHRzIG92ZXIgdGltZT8KCi0gRG9lcyB0aGlzIGluZm9ybWF0aW9uIGxlYWQgeW91IHRvIGNoYW5nZSB5b3VyIGNob2ljZSBvZiBwcmlvcnM/IEhvdz8gUmVzaW11bGF0ZSB5b3VyIHByaW9ycyBmcm9tIFF1ZXN0aW9uIDMuCgpgYGB7ciBldmFsPUZBTFNFLCBpbmNsdWRlPUZBTFNFfQogICMgdHlwZSBpbiB5b3VyIHNpbXVsYXRpb24gCiAgIyBzZWUgY29kZSA0LjM4IGFuZCA0LjM5IGV4YW1wbGUsIHVzZSB5ZWFyIGFzIHggYXhpcyAocHJlZGljdG9yKQpgYGAKCiMjIFF1ZXN0aW9uIDUKCjQuIFJlZml0IG1vZGVsIG00LjMgZnJvbSB0aGUgY2hhcHRlciwgYnV0IG9taXQgdGhlIG1lYW4gd2VpZ2h0IHhiYXIgdGhpcyB0aW1lLiBDb21wYXJlIHRoZSBuZXcgbW9kZWzigJlzIHBvc3RlcmlvciB0byB0aGF0IG9mIHRoZSBvcmlnaW5hbCBtb2RlbC4gSW4gcGFydGljdWxhciwgbG9vayBhdCB0aGUgY292YXJpYW5jZSBhbW9uZyB0aGUgcGFyYW1ldGVycy4gV2hhdCBpcyBkaWZmZXJlbnQ/IFRoZW4gY29tcGFyZSB0aGUgcG9zdGVyaW9yIHByZWRpY3Rpb25zIG9mIGJvdGggbW9kZWxzLgoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGAKCiMjIE9wdGlvbmFsIChub3QgZ3JhZGVkKQoKSW4gdGhlIGNoYXB0ZXIsIHdlIHVzZWQgMTUga25vdHMgd2l0aCB0aGUgY2hlcnJ5IGJsb3Nzb20gc3BsaW5lLiBJbmNyZWFzZSB0aGUgbnVtYmVyIG9mIGtub3RzIGFuZCBvYnNlcnZlIHdoYXQgaGFwcGVucyB0byB0aGUgcmVzdWx0aW5nIHNwbGluZS4gVGhlbiBhZGp1c3QgYWxzbyB0aGUgd2lkdGggb2YgdGhlIHByaW9yIG9uIHRoZSB3ZWlnaHRz4oCUY2hhbmdlIHRoZSBzdGFuZGFyZCBkZXZpYXRpb24gb2YgdGhlIHByaW9yIGFuZCB3YXRjaCB3aGF0IGhhcHBlbnMuIFdoYXQgZG8geW91IHRoaW5rIHRoZSBjb21iaW5hdGlvbiBvZiBrbm90IG51bWJlciBhbmQgdGhlIHByaW9yIG9uIHRoZSB3ZWlnaHRzIGNvbnRyb2xzPwoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgpgYGA=" download="02-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1415432).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

## Question 1

Construct a linear regression of weight as predicted by height, using the adults (age 18 or greater) from the Howell1 dataset. The heights listed below were recorded in the !Kung census, but weights were not recorded for these individuals.

Provide predicted weights and 89% compatibility intervals for each of these individuals. Fill in the table below, using model-based predictions.

| Individual | height | expected weight | 89% interval |
|------------|--------|-----------------|--------------|
| 1          | 140    |                 |              |
| 2          | 150    |                 |              |
| 3          | 160    |                 |              |
| 4          | 175    |                 |              |

## Questions 2-4

A sample of students is measured for height each year for 3 years. After the third year, you want to fit a linear regression predicting height (in centimeters) using year as a predictor.

### Question 2:

-   Write down a mathematical model definition for this regression, using any variable names and priors you choose. You don’t need to run since you won’t have the data. You may also write down your equation and then upload an image.

To help you, this is what we’ll define as the outcome variable height `\(h_{ij}\)`, where

`\(h_{ij} \sim Normal(u_{ij},\sigma)\)`

### Question 3

-   Run prior predictive simulation and defend your choice of priors.

### Question 4

Now suppose I tell you that the students were 8 to 10 years old (so 8 year 1, 9 year 2, etc.). What do you expect of the trend of students’ heights over time?

-   Does this information lead you to change your choice of priors? How? Resimulate your priors from Question 3.

## Question 5

4.  Refit model m4.3 from the chapter, but omit the mean weight xbar this time. Compare the new model’s posterior to that of the original model. In particular, look at the covariance among the parameters. What is different? Then compare the posterior predictions of both models.

## Optional (not graded)

In the chapter, we used 15 knots with the cherry blossom spline. Increase the number of knots and observe what happens to the resulting spline. Then adjust also the width of the prior on the weights—change the standard deviation of the prior and watch what happens. What do you think the combination of knot number and the prior on the weights controls?
