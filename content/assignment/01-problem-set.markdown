---
title: Problem Set 1
date: "2022-01-26"
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

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCAxCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKbWVudToKICBhc3NpZ25tZW50OgogICAgcGFyZW50OiBQcm9ibGVtIHNldHMKICAgIHdlaWdodDogMQp0eXBlOiBkb2NzCnRvYzogdHJ1ZQotLS0KClRoaXMgcHJvYmxlbSBzZXQgaXMgZHVlIG9uIEphbnVhcnkgMzEsIDIwMjIgYXQgMTE6NTlhbS4KClN0ZXAgMTogRG93bmxvYWQgdGhpcyBmaWxlIGxvY2FsbHkuCmBgYHtyIGVjaG89RkFMU0V9CiMgcmVtb3ZlIHRoaXMgY2h1bmsgZm9yIHlvdXIgc3VibWlzc2lvbgpkb3dubG9hZHRoaXM6OmRvd25sb2FkX2ZpbGUoCiAgcGF0aCA9ICIwMS1wcm9ibGVtLXNldC5SbWFya2Rvd24iLAogIG91dHB1dF9uYW1lID0gIjAxLXByb2JsZW0tc2V0IiwKICBidXR0b25fbGFiZWwgPSAiRG93bmxvYWQgdGhpcyBmaWxlIiwKICBidXR0b25fdHlwZSA9ICJkYW5nZXIiLAogIGhhc19pY29uID0gVFJVRSwKICBpY29uID0gImZhIGZhLXNhdmUiLAogIHNlbGZfY29udGFpbmVkID0gRkFMU0UKKQpgYGAKClN0ZXAgMjogQ29tcGxldGUgdGhlIGFzc2lnbm1lbnQKClN0ZXAgMzogS25pdCB0aGUgYXNzaWdubWVudCBhcyBlaXRoZXIgYW4gaHRtbCBvciBwZGYgZmlsZS4KClN0ZXAgNDogU3VibWl0IHlvdXIgZmlsZSBoZXJlIFt0aHJvdWdoIHRoaXMgY2FudmFzIGxpbmtdKGh0dHBzOi8vdW5jYy5pbnN0cnVjdHVyZS5jb20vY291cnNlcy8xNzEwMDAvYXNzaWdubWVudHMvMTQxNTQzMikuCgotLS0tLS0tLS0tLS0tLS0tLQoKLSAqKk5hbWUqKjoKLSAqKlVOQ0MgSUQqKjogCi0gKipPdGhlciBzdHVkZW50IHdvcmtlZCB3aXRoIChvcHRpb25hbCkqKjoKCiMjIFF1ZXN0aW9uIDEKCjEuIFlvdXIgZnJpZW5kIGp1c3QgYmVjYW1lIGludGVyZXN0ZWQgaW4gQmF5ZXNpYW4gc3RhdGlzdGljcy4gSW4gb25lIHBhcmFncmFwaCBvciBsZXNzIChubyBjb2RlKSwgZXhwbGFpbiB0aGUgZm9sbG93aW5nIHRvIHRoZW06CiogV2h5L3doZW4gaXMgQmF5ZXNpYW4gc3RhdGlzdGljcyB1c2VmdWw/CiogV2hhdCBhcmUgdGhlIHNpbWlsYXJpdGllcyBpbiBCYXllc2lhbiBhbmQgZnJlcXVlbnRpc3Qgc3RhdGlzdGljcz8KCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpIaW50OiBJZiB5b3UgbmVlZCBpZGVhcywgZ28gdG8gW1JlZmVyZW5jZXMvUmVhZGluZ3MgaW4gQmF5ZXNpYW4gU3RhdGlzdGljc10oaHR0cHM6Ly9kc2JhNjAxMC1zcHJpbmcyMDIyLm5ldGxpZnkuYXBwL3Jlc291cmNlL2JheWVzaWFuLXJlYWRpbmdzLykgYW5kIGNob29zZSAxIHBhcGVyLiBObyBuZWVkIHRvIHJlYWQgZW50aXJlIHBhcGVyIC0gZm9jdXMgb24gaW50cm9kdWN0aW9uIG9yIG9wZW5pbmcgcGFyYWdyYXBocyB0byBqdXN0aWZ5LiBGb2N1cyBvbiBvbmVzIHdpdGggZWl0aGVyICA8aSBjbGFzcz0iZmFzIGZhLXN0YXIiPjwvaT4gPGkgY2xhc3M9ImZhcyBmYS1zdGFyIj48L2k+IG9yICA8aSBjbGFzcz0iZmFzIGZhLXN0YXIiPjwvaT48aSBjbGFzcz0iZmFzIGZhLXN0YXIiPjwvaT48aSBjbGFzcz0iZmFzIGZhLXN0YXIiPjwvaT4KCnt7JSAvIGNhbGxvdXQgJX19CgojIyBRdWVzdGlvbiAyCgoyLiBTdXBwb3NlIHRoZSBnbG9iZSB0b3NzaW5nIGRhdGEgKENoYXB0ZXIgMikgaGFkIHR1cm5lZCBvdXQgdG8gYmUgNCB3YXRlciBhbmQgMTEgbGFuZC4gQ29uc3RydWN0IHRoZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uLCB1c2luZyBncmlkIGFwcHJveGltYXRpb24uIFVzZSB0aGUgc2FtZSBmbGF0IHByaW9yIGFzIGluIHRoZSBib29rLiBQbG90IHRoZSBwb3N0ZXJpb3IuIFVzZSAxMDAwIGdyaWQgYXBwcm94aW1hdGlvbnMgYW5kIHNldCB0aGUgc2V0IGFzIGBzZXQuc2VlZCgxMDApYC4KCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKIyBpbmNsdWRlIGF0IGxlYXN0IDEgcGFyYWdyYXBoIGJlbG93IHRvIGV4cGxhaW4geW91ciByZXN1bHRzCmBgYAoKIyMgUXVlc3Rpb24gMwoKMy4gTm93IHN1cHBvc2UgdGhlIGRhdGEgYXJlIDQgd2F0ZXIgYW5kIDIgbGFuZC4gQ29tcHV0ZSB0aGUgcG9zdGVyaW9yIGFnYWluLCBidXQgdGhpcyB0aW1lIHVzZSBhIHByaW9yIHRoYXQgaXMgemVybyBiZWxvdyBwID0gMC41IGFuZCBhIGNvbnN0YW50IGFib3ZlIHAgPSAwLjUuIFRoaXMgY29ycmVzcG9uZHMgdG8gcHJpb3IgaW5mb3JtYXRpb24gdGhhdCBhIG1ham9yaXR5IG9mIHRoZSBFYXJ0aOKAmXMgc3VyZmFjZSBpcyB3YXRlci4gUGxvdCB0aGUgbmV3IHBvc3Rlcmlvci4gVXNlIDEwMDAgZ3JpZCBhcHByb3hpbWF0aW9ucyBhbmQgc2V0IHRoZSBzZXQgYXMgYHNldC5zZWVkKDEwMClgLgoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgojIGluY2x1ZGUgYXQgbGVhc3QgMSBwYXJhZ3JhcGggYmVsb3cgdG8gZXhwbGFpbiB5b3VyIHJlc3VsdHMKYGBgCgojIyBRdWVzdGlvbiA0Cgo0LiBGb3IgdGhlIHBvc3RlcmlvciBkaXN0cmlidXRpb24gZnJvbSAzLCBjb21wdXRlIDg5JSBwZXJjZW50aWxlIGFuZCBIUERJIGludGVydmFscy4gQ29tcGFyZSB0aGUgd2lkdGhzIG9mIHRoZXNlIGludGVydmFscy4gV2hpY2ggaXMgd2lkZXI/IFdoeT8gSWYgeW91IGhhZCBvbmx5IHRoZSBpbmZvcm1hdGlvbiBpbiB0aGUgaW50ZXJ2YWwsIHdoYXQgbWlnaHQgeW91IG1pc3VuZGVyc3RhbmQgYWJvdXQgdGhlIHNoYXBlIG9mIHRoZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uPwoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyB0eXBlIGluIHlvdXIgY29kZSBoZXJlCgojIGluY2x1ZGUgYXQgbGVhc3QgMSBwYXJhZ3JhcGggYmVsb3cgdG8gZXhwbGFpbiB5b3VyIHJlc3VsdHMKYGBgCgojIyBPcHRpb25hbCAobm90IGdyYWRlZCkKClN1cHBvc2UgdGhlcmUgaXMgYmlhcyBpbiBzYW1wbGluZyBzbyB0aGF0IExhbmQgaXMgbW9yZSBsaWtlbHkgdGhhbiBXYXRlciB0byBiZSByZWNvcmRlZC4gU3BlY2lmaWNhbGx5LCBhc3N1bWUgdGhhdCAxLWluLTUgKDIwJSkgb2YgV2F0ZXIgc2FtcGxlcyBhcmUgYWNjaWRlbnRhbGx5IHJlY29yZGVkIGluc3RlYWQgYXMgIkxhbmQiLiBGaXJzdCwgd3JpdGUgYSBnZW5lcmF0aXZlIHNpbXVsYXRpb24gb2YgdGhpcyBzYW1wbGluZyBwcm9jZXNzLiBBc3N1bWluZyB0aGUgdHJ1ZSBwcm9wb3J0aW9uIG9mIFdhdGVyIGlzIDAuNzAsIHdoYXQgcHJvcG9ydGlvbiBkb2VzIHlvdXIgc2ltdWxhdGlvbiB0ZW5kIHRvIHByb2R1Y2UgaW5zdGVhZD8gU2Vjb25kLCB1c2luZyBhIHNpbXVsYXRlZCBzYW1wbGUgb2YgMjAgdG9zc2VzLCBjb21wdXRlIHRoZSB1bmJpYXNlZCBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uIG9mIHRoZSB0cnVlIHByb3BvcnRpb24gb2Ygd2F0ZXIuCgpgYGB7ciBldmFsPUZBTFNFLCBpbmNsdWRlPUZBTFNFfQojIHR5cGUgaW4geW91ciBjb2RlIGhlcmUKCiMgaW5jbHVkZSBhdCBsZWFzdCAxIHBhcmFncmFwaCBiZWxvdyB0byBleHBsYWluIHlvdXIgcmVzdWx0cwpgYGA=" download="01-problem-set.Rmd">
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

Hint: If you need ideas, go to [References/Readings in Bayesian Statistics](https://dsba6010-spring2022.netlify.app/resource/bayesian-readings/) and choose 1 paper. No need to read entire paper - focus on introduction or opening paragraphs to justify. Focus on ones with either <i class="fas fa-star"></i> <i class="fas fa-star"></i> or <i class="fas fa-star"></i><i class="fas fa-star"></i><i class="fas fa-star"></i>

{{% / callout %}}

## Question 2

2.  Suppose the globe tossing data (Chapter 2) had turned out to be 4 water and 11 land. Construct the posterior distribution, using grid approximation. Use the same flat prior as in the book. Plot the posterior. Use 1000 grid approximations and set the set as `set.seed(100)`.

## Question 3

3.  Now suppose the data are 4 water and 2 land. Compute the posterior again, but this time use a prior that is zero below p = 0.5 and a constant above p = 0.5. This corresponds to prior information that a majority of the Earth’s surface is water. Plot the new posterior. Use 1000 grid approximations and set the set as `set.seed(100)`.

## Question 4

4.  For the posterior distribution from 3, compute 89% percentile and HPDI intervals. Compare the widths of these intervals. Which is wider? Why? If you had only the information in the interval, what might you misunderstand about the shape of the posterior distribution?

## Optional (not graded)

Suppose there is bias in sampling so that Land is more likely than Water to be recorded. Specifically, assume that 1-in-5 (20%) of Water samples are accidentally recorded instead as “Land.” First, write a generative simulation of this sampling process. Assuming the true proportion of Water is 0.70, what proportion does your simulation tend to produce instead? Second, using a simulated sample of 20 tosses, compute the unbiased posterior distribution of the true proportion of water.
