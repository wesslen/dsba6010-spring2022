---
title: Problem Set 1
date: "2022-01-13"
menu:
  assignment:
    parent: Problem sets
    weight: 1
type: docs
toc: true
---

<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on January 31, 2022 at 11:59am.

Step 1: Download this file locally.

<a href="data:application/octet-stream;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCAxCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKbWVudToKICBhc3NpZ25tZW50OgogICAgcGFyZW50OiBQcm9ibGVtIHNldHMKICAgIHdlaWdodDogMQp0eXBlOiBkb2NzCnRvYzogdHJ1ZQotLS0KClRoaXMgcHJvYmxlbSBzZXQgaXMgZHVlIG9uIEphbnVhcnkgMzEsIDIwMjIgYXQgMTE6NTlhbS4KClN0ZXAgMTogRG93bmxvYWQgdGhpcyBmaWxlIGxvY2FsbHkuCmBgYHtyIGVjaG89RkFMU0V9CiMgcmVtb3ZlIHRoaXMgY2h1bmsgZm9yIHlvdXIgc3VibWlzc2lvbgpkb3dubG9hZHRoaXM6OmRvd25sb2FkX2ZpbGUoCiAgcGF0aCA9ICIwMS1wcm9ibGVtLXNldC5SbWFya2Rvd24iLAogIG91dHB1dF9uYW1lID0gIjAxLXByb2JsZW0tc2V0IiwKICBidXR0b25fbGFiZWwgPSAiRG93bmxvYWQgdGhpcyBmaWxlIiwKICBidXR0b25fdHlwZSA9ICJkYW5nZXIiLAogIGhhc19pY29uID0gVFJVRSwKICBpY29uID0gImZhIGZhLXNhdmUiLAogIHNlbGZfY29udGFpbmVkID0gRkFMU0UKKQpgYGAKClN0ZXAgMjogQ29tcGxldGUgdGhlIGFzc2lnbm1lbnQKClN0ZXAgMzogS25pdCB0aGUgYXNzaWdubWVudCBhcyBlaXRoZXIgYW4gaHRtbCBvciBwZGYgZmlsZS4KClN0ZXAgNDogU3VibWl0IHlvdXIgZmlsZSBoZXJlIFt0aHJvdWdoIHRoaXMgY2FudmFzIGxpbmtdKGh0dHBzOi8vdW5jYy5pbnN0cnVjdHVyZS5jb20vY291cnNlcy8xNzEwMDAvYXNzaWdubWVudHMvMTQxNTQzMikuCgotLS0tLS0tLS0tLS0tLS0tLQoKLSAqKk5hbWUqKjoKLSAqKlVOQ0MgSUQqKjogCi0gKipPdGhlciBzdHVkZW50IHdvcmtlZCB3aXRoIChvcHRpb25hbCkqKjoKCiMjIFF1ZXN0aW9uIDEKCjEuIFlvdXIgZnJpZW5kIGp1c3QgYmVjYW1lIGludGVyZXN0ZWQgaW4gQmF5ZXNpYW4gc3RhdGlzdGljcy4gSW4gb25lIHBhcmFncmFwaCBvciBsZXNzIChubyBjb2RlKSwgZXhwbGFpbiB0aGUgZm9sbG93aW5nIHRvIHRoZW06CiogV2h5L3doZW4gaXMgQmF5ZXNpYW4gc3RhdGlzdGljcyB1c2VmdWw/CiogV2hhdCBhcmUgdGhlIHNpbWlsYXJpdGllcyBpbiBCYXllc2lhbiBhbmQgZnJlcXVlbnRpc3Qgc3RhdGlzdGljcz8KCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpIaW50OiBnbyB0byBbUmVmZXJlbmNlcy9SZWFkaW5ncyBpbiBCYXllc2lhbiBTdGF0aXN0aWNzXShodHRwczovL2RzYmE2MDEwLXNwcmluZzIwMjIubmV0bGlmeS5hcHAvcmVzb3VyY2UvYmF5ZXNpYW4tcmVhZGluZ3MvKSBhbmQgY2hvb3NlIDEgcGFwZXIuIEZvY3VzIG9uIHNob3J0ZXIgcmVhZGluZ3MgYW5kIG9uZXMgd2l0aCBlaXRoZXIgIDxpIGNsYXNzPSJmYXMgZmEtc3RhciI+PC9pPiA8aSBjbGFzcz0iZmFzIGZhLXN0YXIiPjwvaT4gb3IgIDxpIGNsYXNzPSJmYXMgZmEtc3RhciI+PC9pPjxpIGNsYXNzPSJmYXMgZmEtc3RhciI+PC9pPjxpIGNsYXNzPSJmYXMgZmEtc3RhciI+PC9pPgoKe3slIC8gY2FsbG91dCAlfX0KCiMjIFF1ZXN0aW9uIDIKCjIuIFN1cHBvc2UgdGhlIGdsb2JlIHRvc3NpbmcgZGF0YSAoQ2hhcHRlciAyKSBoYWQgdHVybmVkIG91dCB0byBiZSA0IHdhdGVyIGFuZCAxMSBsYW5kLiBDb25zdHJ1Y3QgdGhlIHBvc3RlcmlvciBkaXN0cmlidXRpb24sIHVzaW5nIGdyaWQgYXBwcm94aW1hdGlvbi4gVXNlIHRoZSBzYW1lIGZsYXQgcHJpb3IgYXMgaW4gdGhlIGJvb2suCgpgYGB7ciBldmFsPUZBTFNFLCBpbmNsdWRlPUZBTFNFfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCiMgaW5jbHVkZSBhdCBsZWFzdCAxIHBhcmFncmFwaCBiZWxvdyB0byBleHBsYWluIHlvdXIgcmVzdWx0cwpgYGAKCiMjIFF1ZXN0aW9uIDMKCjMuIE5vdyBzdXBwb3NlIHRoZSBkYXRhIGFyZSA0IHdhdGVyIGFuZCAyIGxhbmQuIENvbXB1dGUgdGhlIHBvc3RlcmlvciBhZ2FpbiwgYnV0IHRoaXMgdGltZSB1c2UgYSBwcmlvciB0aGF0IGlzIHplcm8gYmVsb3cgcCA9IDAuNSBhbmQgYSBjb25zdGFudCBhYm92ZSBwID0gMC41LiBUaGlzIGNvcnJlc3BvbmRzIHRvIHByaW9yIGluZm9ybWF0aW9uIHRoYXQgYSBtYWpvcml0eSBvZiB0aGUgRWFydGjigJlzIHN1cmZhY2UgaXMgd2F0ZXIuCgpgYGB7ciBldmFsPUZBTFNFLCBpbmNsdWRlPUZBTFNFfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCiMgaW5jbHVkZSBhdCBsZWFzdCAxIHBhcmFncmFwaCBiZWxvdyB0byBleHBsYWluIHlvdXIgcmVzdWx0cwpgYGAKCiMjIFF1ZXN0aW9uIDQKCjQuIEZvciB0aGUgcG9zdGVyaW9yIGRpc3RyaWJ1dGlvbiBmcm9tIDIsIGNvbXB1dGUgODklIHBlcmNlbnRpbGUgYW5kIEhQREkgaW50ZXJ2YWxzLiBDb21wYXJlIHRoZSB3aWR0aHMgb2YgdGhlc2UgaW50ZXJ2YWxzLiBXaGljaCBpcyB3aWRlcj8gV2h5PyBJZiB5b3UgaGFkIG9ubHkgdGhlIGluZm9ybWF0aW9uIGluIHRoZSBpbnRlcnZhbCwgd2hhdCBtaWdodCB5b3UgbWlzdW5kZXJzdGFuZCBhYm91dCB0aGUgc2hhcGUgb2YgdGhlIHBvc3RlcmlvciBkaXN0cmlidXRpb24/CgpgYGB7ciBldmFsPUZBTFNFLCBpbmNsdWRlPUZBTFNFfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCiMgaW5jbHVkZSBhdCBsZWFzdCAxIHBhcmFncmFwaCBiZWxvdyB0byBleHBsYWluIHlvdXIgcmVzdWx0cwpgYGAKCiMjIE9wdGlvbmFsIChub3QgZ3JhZGVkKQoKU3VwcG9zZSB0aGVyZSBpcyBiaWFzIGluIHNhbXBsaW5nIHNvIHRoYXQgTGFuZCBpcyBtb3JlIGxpa2VseSB0aGFuIFdhdGVyIHRvIGJlIHJlY29yZGVkLiBTcGVjaWZpY2FsbHksIGFzc3VtZSB0aGF0IDEtaW4tNSAoMjAlKSBvZiBXYXRlciBzYW1wbGVzIGFyZSBhY2NpZGVudGFsbHkgcmVjb3JkZWQgaW5zdGVhZCBhcyAiTGFuZCIuIEZpcnN0LCB3cml0ZSBhIGdlbmVyYXRpdmUgc2ltdWxhdGlvbiBvZiB0aGlzIHNhbXBsaW5nIHByb2Nlc3MuIEFzc3VtaW5nIHRoZSB0cnVlIHByb3BvcnRpb24gb2YgV2F0ZXIgaXMgMC43MCwgd2hhdCBwcm9wb3J0aW9uIGRvZXMgeW91ciBzaW11bGF0aW9uIHRlbmQgdG8gcHJvZHVjZSBpbnN0ZWFkPyBTZWNvbmQsIHVzaW5nIGEgc2ltdWxhdGVkIHNhbXBsZSBvZiAyMCB0b3NzZXMsIGNvbXB1dGUgdGhlIHVuYmlhc2VkIHBvc3RlcmlvciBkaXN0cmlidXRpb24gb2YgdGhlIHRydWUgcHJvcG9ydGlvbiBvZiB3YXRlci4KCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKIyBpbmNsdWRlIGF0IGxlYXN0IDEgcGFyYWdyYXBoIGJlbG93IHRvIGV4cGxhaW4geW91ciByZXN1bHRzCmBgYA==" download="01-problem-set.Rmarkdown">
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

1.  Your friend just became interested in Bayesian statistics. In one paragraph or less (no code), explain the following to them:

-   Why/when is Bayesian statistics useful?
-   What are the similarities in Bayesian and frequentist statistics?

{{% callout note %}}

Hint: go to [References/Readings in Bayesian Statistics](https://dsba6010-spring2022.netlify.app/resource/bayesian-readings/) and choose 1 paper. Focus on shorter readings and ones with either <i class="fas fa-star"></i> <i class="fas fa-star"></i> or <i class="fas fa-star"></i><i class="fas fa-star"></i><i class="fas fa-star"></i>

{{% / callout %}}

## Question 2

2.  Suppose the globe tossing data (Chapter 2) had turned out to be 4 water and 11 land. Construct the posterior distribution, using grid approximation. Use the same flat prior as in the book.

## Question 3

3.  Now suppose the data are 4 water and 2 land. Compute the posterior again, but this time use a prior that is zero below p = 0.5 and a constant above p = 0.5. This corresponds to prior information that a majority of the Earth’s surface is water.

## Question 4

4.  For the posterior distribution from 2, compute 89% percentile and HPDI intervals. Compare the widths of these intervals. Which is wider? Why? If you had only the information in the interval, what might you misunderstand about the shape of the posterior distribution?

## Optional (not graded)

Suppose there is bias in sampling so that Land is more likely than Water to be recorded. Specifically, assume that 1-in-5 (20%) of Water samples are accidentally recorded instead as “Land.” First, write a generative simulation of this sampling process. Assuming the true proportion of Water is 0.70, what proportion does your simulation tend to produce instead? Second, using a simulated sample of 20 tosses, compute the unbiased posterior distribution of the true proportion of water.
