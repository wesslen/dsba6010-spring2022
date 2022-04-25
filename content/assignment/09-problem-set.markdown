---
title: Problem Set 9
date: "2022-04-25"
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

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA5CmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBNYXkgMiwgMjAyMiBhdCAxMTo1OWFtLgoKLSAqKk5hbWUqKjoKLSAqKlVOQ0MgSUQqKjogCi0gKipPdGhlciBzdHVkZW50IHdvcmtlZCB3aXRoIChvcHRpb25hbCkqKjoKCkZvciB0aGlzIHByb2JsZW0gc2V0LCB5b3UncmUgZ29pbmcgdG8gcnVuIHRoZSBtZWFzdXJlbWVudCBlcnJvciBleGFtcGxlcyBjb25zaWRlcmVkIGluIDE1LjEgaW4gdGhlIGJvb2sgYW5kIFtMZWN0dXJlIDE3XShodHRwczovL3lvdXR1LmJlL2xURkFCNlFtd0hNKS4gSG93ZXZlciwgeW91J3JlIGdvaW5nIHRvIHJ1biB0aGVtIGluIHRoZSBbZm9ybWF0IG9mIHRoZSBwcm9qZWN0XShodHRwczovL2RzYmE2MDEwLXNwcmluZzIwMjIubmV0bGlmeS5hcHAvYXNzaWdubWVudC9maW5hbC1wcm9qZWN0LyMyLW51bWJlci1vZi1hbmFseXNpcy10YXNrcy01MC1wb2ludHMpLCBuYW1lbHkgdGhlIEJheWVzaWFuIHdvcmtmbG93LgoKWW91IG1heSBmaW5kIFt0aGlzIGNvZGUgZnJvbSB0aGUgbGVjdHVyZSB0byBiZSBoZWxwZnVsXShodHRwczovL2dpdGh1Yi5jb20vcm1jZWxyZWF0aC9zdGF0X3JldGhpbmtpbmdfMjAyMi9ibG9iL2I3NWM0NWIzNWQzODM4NjZhYWI1MjYyM2M0ZTVkYzUyYmU3OGJlZWUvc2NyaXB0c19hbmltYXRpb24vMTdfbWVhc3VyZW1lbnRfZXJyb3IuciNMMzUpLiBJbnN0ZWFkIG9mIHVzaW5nIGByZXRoaW5raW5nYCwgeW91IG1heSBhbHNvIGFsdGVybmF0aXZlbHkgdXNlIGBicm1zYCB1c2luZyBbU29sb21vbiBLdXJ6J3MgcmV0aGlua2luZyB2ZXJzaW9uIG9mIENoYXB0ZXIgMTVdKGh0dHBzOi8vYm9va2Rvd24ub3JnL2NvbnRlbnQvNDg1Ny9taXNzaW5nLWRhdGEtYW5kLW90aGVyLW9wcG9ydHVuaXRpZXMuaHRtbCNtZWFzdXJlbWVudC1lcnJvcikuCgpFaXRoZXIgdXNlIHRoaXMgUm1hcmtkb3duIGZpbGUgb3IgY3JlYXRlIGEgbmV3IG9uZSBmb3Igc2NyYXRjaCBmb3IgdGhpcyBwcm9qZWN0LiAKCiMjIDEuIEluaXRpYWwgTW9kZWwKCiogUGFydCAxOiBQcm92aWRlIGluIERBRyBmb3JtIHRoZSBpbml0aWFsIG1vZGVsIHNob3duIG9uIHBhZ2UgNDkyLiBCZSBzdXJlIHRvIGRpZmZlcmVudGlhdGUgdGhlIHVub2JzZXJ2ZWQgbm9kZXMuIFdoYXQgaXMgYSB3YXkgdGhhdCB3ZSBjYW4gcmV3cml0ZSB0aGUgcmVncmVzc2lvbiBpbnRvIG11bHRpcGxlIHBhcnRzIChzZWUgW0xlY3R1cmUgMTcgc2xpZGVzIDM3LTM4XShodHRwczovL2ZpbGVzLnNwZWFrZXJkZWNrLmNvbS9wcmVzZW50YXRpb25zL2I2NTc2MzdlNDg4MTRjNzg5NmFkNzkzOTVhMjlmMGEwL0xlY3R1cmVfMTcucGRmKSk/IEV4cGxhaW4gaW4gMi0zIHNlbnRlbmNlcyB0aGUgREFHLgoKKiBQYXJ0IDI6IFdyaXRlIG91dCB5b3VyIHN0YXRpc3RpY2FsIG1vZGVsIGluIHVsYW0gZm9ybS4gVGhpcyBpcyBzbGlkZXMgNDEtNDIgb24gW0xlY3R1cmUgMTddKGh0dHBzOi8vZmlsZXMuc3BlYWtlcmRlY2suY29tL3ByZXNlbnRhdGlvbnMvYjY1NzYzN2U0ODgxNGM3ODk2YWQ3OTM5NWEyOWYwYTAvTGVjdHVyZV8xNy5wZGYpLiBXaGVuIGRvZXMgdGhlIGRpc3RpbmN0aW9uIGJldHdlZW4gZGF0YSBhbmQgcGFyYW1ldGVycyBpbXBvcnRhbnQgaW4gdGhpcyBtb2RlbCBhbmQgd2hlbiBpcyBpdCBub3QgKHNlZSBbYXJvdW5kIDMwOjAwIGluIExlY3R1cmUgMTcgdmlkZW9dKGh0dHBzOi8veW91dHUuYmUvbFRGQUI2UW13SE0pKQoKRm9yIHRoaXMgcGFydCwgaXQncyBva2F5IGlmIHlvdSB0ZWNobmljYWxseSBydW4gdGhlIG1vZGVsLCB3aGljaCB3b3VsZCBiZSBwYXJ0IDMuIFdlJ2xsIG5lZWQgdGhlIG1vZGVsIHNwZWNpZmljYXRpb24gZm9yIHRoZSBwcmlvciBwcmVkaWN0aXZlIGNoZWNrIG5leHQuCgojIyAyLiBQcmlvciBQcmVkaWN0aXZlIENoZWNrCgpDb25kdWN0IGEgcHJpb3IgcHJlZGljdGl2ZSBzaW11bGF0aW9uIGZvciB5b3VyIG1vZGVsLiBZb3UgbWF5IGZpbmQgdGhlIGVhcmxpZXIgZXhhbXBsZSBvZiBSIGNvZGUgNS40IHRvIGJlIGhlbHBmdWwgZnJvbSBDaGFwdGVyIDUuIFlvdSB3aWxsIG5lZWQgdG8gdXNlIHRoZSBgZXh0cmFjdC5wcmlvcigpYCBmdW5jdGlvbi4gU2V0IHlvdXIgc2VlZCB0byBgc2V0LnNlZWQoMTApYC4gVG8gZG8gdGhlIHByaW9yIHByZWRpY3RpdmUgY2hlY2ssIHlvdSB3aWxsIG5lZWQgdG8gcHV0IGBjKC0yLDIpYCBmb3IgdGhlIGBBYCBhbmQgYE1gIGlucHV0cy4gUGxvdCB5b3VyIG1vZGVsLgoKIyMgMy4gRml0IHRoZSBNb2RlbAoKUnVuIHRoZSBgbTE1LjFgIG1vZGVsIGlmIHlvdSBoYXZlbid0IGFscmVhZHkgaW4gcGFydCAxLiBSdW4gYSBgcHJlY2lzKGRlcHRoPTIpYCBmdW5jdGlvbiB0byBkaXNwbGF5IHRoZSBzdW1tYXJpemVkIG1vZGVsIHJlc3VsdHMuIENvbXBhcmUgeW91ciByZXN1bHRzIHRvIHdoYXQgd2FzIGluIExlY3R1cmUgMTcgLyBwYWdlIDQ5NC4KCiMjIDQuIFZhbGlkYXRlIENvbXB1dGF0aW9uCgpSdW4gY29udmVyZ2VuY2UgZGlhZ25vc3RpY3MgbGlrZSB0cmFjZSBwbG90cywgdHJhbmsgcGxvdHMsIGFuZCBleGFtaW5pbmcgdGhlIGBSaGF0YCBhbmQgYG5fZWZmYC4gQXJlIHlvdSBzYXRpc2ZpZWQgd2l0aCB0aGUgY29udmVyZ2VuY2Ugb2YgeW91ciBtb2RlbD8gTWFrZSBhbiBhcmd1bWVudCB3aHkgb3Igd2h5IG5vdC4gCgojIyA1LiBQb3N0ZXJpb3IgUHJlZGljdGl2ZSBDaGVjawoKUnVuIGEgcG9zdGVyaW9yIHByZWRpY3RpdmUgY2hlY2sgYXMgYSBzY2F0dGVycGxvdC4gWW91IHdpbGwgd2FudCB0byBmaXJzdCBkaXNwbGF5IHRoZSBvcmlnaW5hbCBwb2ludHMgKGlnbm9yZXMgbW9kZWwgZXJyb3IpIGFuZCB0aGUgcG9zdGVyaW9yIG1lYW5zLiBZb3UgY2FuIGZpbmQgW3RoZSBjb2RlIGhlcmVdKGh0dHBzOi8vZ2l0aHViLmNvbS9ybWNlbHJlYXRoL3N0YXRfcmV0aGlua2luZ18yMDIyL2Jsb2IvYjc1YzQ1YjM1ZDM4Mzg2NmFhYjUyNjIzYzRlNWRjNTJiZTc4YmVlZS9zY3JpcHRzX2FuaW1hdGlvbi8xN19tZWFzdXJlbWVudF9lcnJvci5yI0w5MSkuIEluIG9yZGVyIHRvIHJ1biB0aGlzLCB5b3UnbGwgbmVlZCB0byByZXJ1biB0aGUgc2ltaWxhciBtb2RlbCBidXQgd2l0aG91dCBtb2RlbCBlcnJvci4KCkV4cGxhaW4gd2hhdCBpcyBnb2luZyBvbiBpbiB0aGlzIHBsb3QuCgpZb3UgYXJlIHdlbGNvbWUgdG8gcnVuIGNyb3NzLXZhbGlkYXRpb24gKGUuZy4sIFdBSUMvUFNJUykgb24gdGhlIHR3byBtb2RlbHMgdG8gYW5kIGNvbXBhcmUgdGhlaXIgbW9kZWwgZml0LiAKCiMjIFJldmlzZSBNb2RlbDogUmVwZWF0IGZvciBlcnJvciBpbiBib3RoIG91dGNvbWUgYW5kIHByZWRpY3RvcgoKRm9sbG93IHNpbWlsYXIgbW9kZWwgY29kZSBpbiAxNS41IHRvIGV4dGVuZCB5b3VyIG1vZGVsIHRvIGluY2x1ZGUgbWVhc3VyZW1lbnQgZXJyb3IgZm9yIG1hcnJpYWdlIHJhdGUgKE0pLiBZb3Ugd2lsbCBuZWVkIHRvIHJlcGVhdCB0aGUgc2FtZSBmaXZlIHN0ZXBzIGFib3ZlIGZvciB0aGlzIG5ldyB2ZXJzaW9uIG9mIHRoZSBtb2RlbDogc3BlY2lmeSB0aGUgbW9kZWwvREFHLCBydW4gcHJpb3IgcHJlZGljdGl2ZSBjaGVjaywgZml0IHRoZSBtb2RlbCwgdmFsaWRhdGUgY29tcHV0YXRpb24sIGFuZCBydW4gcG9zdGVyaW9yIHByZWRpY3RpdmUgY2hlY2sgKHNlZSBSIGNvZGUgMTUuNikuIAoKSW4geW91ciBwb3N0ZXJpb3IgcHJlZGljdGl2ZSBjaGVjaywgcmVydW4gdGhlIGNvZGUgdG8gcHJvZHVjZSBGaWd1cmUgMTUuMyBpbiB0aGUgYm9vayAoc2xpZGUgNTgtNTkgaW4gdGhlIExlY3R1cmUpLgoKQ29tcGFyZSB5b3VyIHJlc3VsdHMgdG8gcnVubmluZyB0aGUgbW9kZWwgd2l0aG91dCBtZWFzdXJlbWVudCBlcnJvci4gRGlkIGluY2x1ZGluZyBlcnJvciBvbiBNIGluY3JlYXNlIG9yIGRlY3JlYXNlIHRoZSBlZmZlY3Qgb2YgTSBvbiBEPwoK" download="09-problem-set.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1467904).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

For this problem set, you’re going to run the measurement error examples considered in 15.1 in the book and [Lecture 17](https://youtu.be/lTFAB6QmwHM). However, you’re going to run them in the [format of the project](https://dsba6010-spring2022.netlify.app/assignment/final-project/#2-number-of-analysis-tasks-50-points), namely the Bayesian workflow.

You may find [this code from the lecture to be helpful](https://github.com/rmcelreath/stat_rethinking_2022/blob/b75c45b35d383866aab52623c4e5dc52be78beee/scripts_animation/17_measurement_error.r#L35). Instead of using `rethinking`, you may also alternatively use `brms` using [Solomon Kurz’s rethinking version of Chapter 15](https://bookdown.org/content/4857/missing-data-and-other-opportunities.html#measurement-error).

Either use this Rmarkdown file or create a new one for scratch for this project.

## 1. Initial Model

-   Part 1: Provide in DAG form the initial model shown on page 492. Be sure to differentiate the unobserved nodes. What is a way that we can rewrite the regression into multiple parts (see [Lecture 17 slides 37-38](https://files.speakerdeck.com/presentations/b657637e48814c7896ad79395a29f0a0/Lecture_17.pdf))? Explain in 2-3 sentences the DAG.

-   Part 2: Write out your statistical model in ulam form. This is slides 41-42 on [Lecture 17](https://files.speakerdeck.com/presentations/b657637e48814c7896ad79395a29f0a0/Lecture_17.pdf). When does the distinction between data and parameters important in this model and when is it not (see [around 30:00 in Lecture 17 video](https://youtu.be/lTFAB6QmwHM))

For this part, it’s okay if you technically run the model, which would be part 3. We’ll need the model specification for the prior predictive check next.

## 2. Prior Predictive Check

Conduct a prior predictive simulation for your model. You may find the earlier example of R code 5.4 to be helpful from Chapter 5. You will need to use the `extract.prior()` function. Set your seed to `set.seed(10)`. To do the prior predictive check, you will need to put `c(-2,2)` for the `A` and `M` inputs. Plot your model.

## 3. Fit the Model

Run the `m15.1` model if you haven’t already in part 1. Run a `precis(depth=2)` function to display the summarized model results. Compare your results to what was in Lecture 17 / page 494.

## 4. Validate Computation

Run convergence diagnostics like trace plots, trank plots, and examining the `Rhat` and `n_eff`. Are you satisfied with the convergence of your model? Make an argument why or why not.

## 5. Posterior Predictive Check

Run a posterior predictive check as a scatterplot. You will want to first display the original points (ignores model error) and the posterior means. You can find [the code here](https://github.com/rmcelreath/stat_rethinking_2022/blob/b75c45b35d383866aab52623c4e5dc52be78beee/scripts_animation/17_measurement_error.r#L91). In order to run this, you’ll need to rerun the similar model but without model error.

Explain what is going on in this plot.

You are welcome to run cross-validation (e.g., WAIC/PSIS) on the two models to and compare their model fit.

## Revise Model: Repeat for error in both outcome and predictor

Follow similar model code in 15.5 to extend your model to include measurement error for marriage rate (M). You will need to repeat the same five steps above for this new version of the model: specify the model/DAG, run prior predictive check, fit the model, validate computation, and run posterior predictive check (see R code 15.6).

In your posterior predictive check, rerun the code to produce Figure 15.3 in the book (slide 58-59 in the Lecture).

Compare your results to running the model without measurement error. Did including error on M increase or decrease the effect of M on D?
