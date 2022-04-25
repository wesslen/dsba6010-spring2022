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

For this problem set, you’re going to run the measurement error examples considered in 15.1 in the book and [Lecture 17](https://youtu.be/lTFAB6QmwHM). However, you’re going to run them in the \[format of the project\](https://dsba6010-spring2022.netlify.app/assignment/final-project/\#2-number-of-analysis-tasks-50-points, namely the Bayesian workflow.

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
