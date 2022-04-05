---
title: Problem Set 7 Solutions
date: "2022-03-30"
menu:
  assignment:
    parent: Problem sets
    weight: 7
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on April 4, 2022 at 11:59am.

<a href="data:application/octet-stream;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCA3IFNvbHV0aW9ucwpkYXRlOiAiYHIgU3lzLkRhdGUoKWAiCm1lbnU6CiAgYXNzaWdubWVudDoKICAgIHBhcmVudDogUHJvYmxlbSBzZXRzCiAgICB3ZWlnaHQ6IDcKdHlwZTogZG9jcwp0b2M6IHRydWUKLS0tCgpgYGB7ciBzZXR1cCwgaW5jbHVkZT1GQUxTRSwgZmlnLndpZHRoPTUsIGZpZy5oZWlnaHQ9NH0Ka25pdHI6Om9wdHNfY2h1bmskc2V0KGVjaG8gPSBUUlVFLCBjbGFzcy5zb3VyY2U9Imxhbmd1YWdlLXIiLCBjbGFzcy5vdXRwdXQ9Imxhbmd1YWdlLXIiLCBtZXNzYWdlID0gRkFMU0UsIHdhcm5pbmcgPSBGQUxTRSkKeGFyaW5nYW5FeHRyYTo6dXNlX2NsaXBib2FyZCgpCmBgYAoKVGhpcyBwcm9ibGVtIHNldCBpcyBkdWUgb24gQXByaWwgNCwgMjAyMiBhdCAxMTo1OWFtLgoKYGBge3IgZWNobz1GQUxTRX0KIyByZW1vdmUgdGhpcyBjaHVuayBmb3IgeW91ciBzdWJtaXNzaW9uCmRvd25sb2FkdGhpczo6ZG93bmxvYWRfZmlsZSgKICBwYXRoID0gIjA3LXByb2JsZW0tc2V0LXNvbHV0aW9ucy5SbWFya2Rvd24iLAogIG91dHB1dF9uYW1lID0gIjA3LXByb2JsZW0tc2V0LXNvbHV0aW9ucyIsCiAgYnV0dG9uX2xhYmVsID0gIkRvd25sb2FkIHRoaXMgZmlsZSIsCiAgYnV0dG9uX3R5cGUgPSAiZGFuZ2VyIiwKICBoYXNfaWNvbiA9IFRSVUUsCiAgaWNvbiA9ICJmYSBmYS1zYXZlIiwKICBzZWxmX2NvbnRhaW5lZCA9IEZBTFNFCikKYGBgCgojIyBRdWVzdGlvbiAxCgpDb25kdWN0IGEgcHJpb3IgcHJlZGljdGl2ZSBzaW11bGF0aW9uIGZvciB0aGUgUmVlZGZyb2cgbW9kZWwuIEJ5IHRoaXMgSSBtZWFuIHRvIHNpbXVsYXRlIHRoZSBwcmlvciBkaXN0cmlidXRpb24gb2YgdGFuayBzdXJ2aXZhbCBwcm9iYWJpbGl0aWVzICRcYWxwaGFfe2p9JC4gCgpTdGFydCBieSB1c2luZyB0aGVzZSBwcmlvcnM6CgokXGFscGhhX3tqfSBcc2ltIE5vcm1hbChcYmFye1xhbHBoYX0sXHNpZ21hKSQKCiRcYmFye1xhbHBoYX0gXHNpbSBOb3JtYWwoMCwgMSkkCgokXHNpZ21hIFxzaW0gRXhwb25lbnRpYWwoMSkkCgpCZSBzdXJlIHRvIHRyYW5zZm9ybSB0aGUgJFxhbHBoYV97an0kIHZhbHVlcyB0byB0aGUgcHJvYmFiaWxpdHkgc2NhbGUgZm9yIHBsb3R0aW5nIGFuZCBzdW1tYXJ5LgoKSG93IGRvZXMgaW5jcmVhc2luZyB0aGUgd2lkdGggb2YgdGhlIHByaW9yIG9uIM+DIGNoYW5nZSB0aGUgcHJpb3IgZGlzdHJpYnV0aW9uIG9mICRcYWxwaGFfe2p9JD8KCllvdSBtaWdodCB0cnkgYEV4cG9uZW50aWFsKDEwKWAgYW5kIGBFeHBvbmVudGlhbCgwLjEpYCBmb3IgZXhhbXBsZS4KClNpbXVsYXRpbmcgdmFyeWluZyBlZmZlY3QgcHJpb3JzIGlzIGluIHByaW5jaXBsZSBsaWtlIHNpbXVsYXRpbmcgYW55IG90aGVyIHByaW9ycy4gVGhlIG9ubHkgZGlmZmVyZW5jZSBpcyB0aGF0IHRoZSBwYXJhbWV0ZXJzIGhhdmUgYW4gaW1wbGllZCBvcmRlciBub3csIGJlY2F1c2Ugc29tZSBwYXJhbWV0ZXJzIGRlcGVuZCB1cG9uIG90aGVycy4gU28gaW4gdGhpcyBwcm9ibGVtIHdlIG11c3Qgc2ltdWxhdGUgJFxzaWdtYSQgYW5kICRcYmFye1xhbHBoYX0kIGZpcnN0LCBhbmQgdGhlbiB3ZSBjYW4gc2ltdWxhdGUgdGhlIGluZGl2aWR1YWwgdGFuayAkXGFscGhhX3tUfSQgdmFyaWFibGVzCgpgYGB7cn0KbGlicmFyeShyZXRoaW5raW5nKQpuIDwtIDFlNApzaWdtYSA8LSByZXhwKG4sMSkKYWJhciA8LSBybm9ybShuLDAsMSkKYVQgPC0gcm5vcm0obixhYmFyLHNpZ21hKQpkZW5zKGludl9sb2dpdChhVCkseGxpbT1jKDAsMSksYWRqPTAuMSxsd2Q9NCxjb2w9MiwgbWFpbj0ic2lnbWF+ZXhwb25lbnRpYWwoMCwxKSIpCmBgYAoKTGV0J3MgYWxzbyBydW4gdHdvIG1vcmUgKDAuMSBhbmQgMTApOiAKCmBgYHtyfQpuIDwtIDFlNApzaWdtYSA8LSByZXhwKG4sMC4xKQphYmFyIDwtIHJub3JtKG4sMCwxKQphVCA8LSBybm9ybShuLGFiYXIsc2lnbWEpCmRlbnMoaW52X2xvZ2l0KGFUKSx4bGltPWMoMCwxKSxhZGo9MC4xLGx3ZD00LGNvbD0yLCBtYWluPSJzaWdtYX5leHBvbmVudGlhbCgwLDAuMSkiKQpgYGAKCmBgYHtyfQpuIDwtIDFlNApzaWdtYSA8LSByZXhwKG4sMTApCmFiYXIgPC0gcm5vcm0obiwwLDEpCmFUIDwtIHJub3JtKG4sYWJhcixzaWdtYSkKZGVucyhpbnZfbG9naXQoYVQpLHhsaW09YygwLDEpLGFkaj0wLjEsbHdkPTQsY29sPTIsIG1haW49InNpZ21hfmV4cG9uZW50aWFsKDAsMTApIikKYGBgCgpJbmNyZWFzaW5nIHRoZSB2YXJpYXRpb24gYWNyb3NzIHRhbmtzLCBieSBtYWtpbmcgdGhlICRcc2lnbWEkIGRpc3RyaWJ1dGlvbiB3aWRlciwgcHVzaGVzIHByaW9yIHN1cnZpdmFsIHVwIGFnYWluc3QgdGhlIGZsb29yIGFuZCBjZWlsaW5nIG9mIHRoZSBvdXRjb21lIHNwYWNlLiBUaGlzIGlzIHRoZSBzYW1lIHBoZW5vbWVub24geW91IHNhdyBiZWZvcmUgZm9yIG9yZGluYXJ5IGxvZ2l0IG1vZGVscy4gVGhlIGtleSBsZXNzb24gYWdhaW4gaXMgdGhhdCBmbGF0IHByaW9ycyBvbiBvbmUgc2NhbGUgYXJlIG5vdCBuZWNlc3NhcmlseSBmbGF0IG9uIGFub3RoZXIuCgojIyBRdWVzdGlvbiAyCgpSZXZpc2l0IHRoZSBSZWVkZnJvZyBzdXJ2aXZhbCBkYXRhLCBgZGF0YShyZWVkZnJvZ3MpYC4gU3RhcnQgd2l0aCB0aGUgdmFyeWluZyBlZmZlY3RzIG1vZGVsIGZyb20gdGhlIGJvb2sgYW5kIGxlY3R1cmUuIFRoZW4gbW9kaWZ5IGl0IHRvIGVzdGltYXRlIHRoZSBjYXVzYWwgZWZmZWN0cyBvZiB0aGUgdHJlYXRtZW50IHZhcmlhYmxlcyBwcmVkIGFuZCBzaXplLCBpbmNsdWRpbmcgaG93IHNpemUgbWlnaHQgbW9kaWZ5IHRoZSBlZmZlY3Qgb2YgcHJlZGF0aW9uLiBBbiBlYXN5IGFwcHJvYWNoIGlzIHRvIGVzdGltYXRlIGFuIGVmZmVjdCBmb3IgZWFjaCBjb21iaW5hdGlvbiBvZiBwcmVkIGFuZCBzaXplLiBKdXN0aWZ5IHlvdXIgbW9kZWwgd2l0aCBhIERBRyBvZiB0aGlzIGV4cGVyaW1lbnQuCgpgYGB7cn0KbGlicmFyeShyZXRoaW5raW5nKQpkYXRhKHJlZWRmcm9ncykKZCA8LSByZWVkZnJvZ3MKCmRhdCA8LSBsaXN0KApTID0gZCRzdXJ2LApEID0gZCRkZW5zaXR5LApUID0gMTpucm93KGQpLApQID0gaWZlbHNlKCBkJHByZWQ9PSJubyIgLCAxTCAsIDJMICksCkcgPSBpZmVsc2UoIGQkc2l6ZT09InNtYWxsIiAsIDFMICwgMkwgKSApCgptMiA8LSB1bGFtKAogIGFsaXN0KAogICAgUyB+IGJpbm9taWFsKCBEICwgcCApLAogICAgbG9naXQocCkgPC0gYVtUXSArIGJbUCxHXSwKICAgIGFbVF0gfiBub3JtYWwoIDAgLCBzaWdtYSApLAogICAgbWF0cml4W1AsR106YiB+IG5vcm1hbCggMCAsIDEgKSwKICAgIHNpZ21hIH4gZXhwb25lbnRpYWwoIDEgKQogICksIGRhdGE9ZGF0ICwgY2hhaW5zPTQgLCBjb3Jlcz00ICwgbG9nX2xpaz1UUlVFICkKcHJlY2lzKG0yLDMscGFycz1jKCJiIiwic2lnbWEiKSkKCmBgYAoKVGhlIHBhcmFtZXRlcnMgYXJlIGluIG9yZGVyIGZyb20gdG9wIHRvIGJvdHRvbTogbm8tcHJlZC9zbWFsbCwgbm8tcHJlZC9sYXJnZSwgcHJlZC9zbWFsbCwgcHJlZC9sYXJnZS4gVGhlIGN1cmlvdXMgdGhpbmcgaXMgbm90IHRoYXQgc3Vydml2YWwgaXMgbG93ZXIgd2l0aCBwcmVkYXRpb24sIGJ1dCByYXRoZXIgdGhhdCBpdCBpcyBsb3dlc3QgZm9yIGxhcmdlIHRhZHBvbGVzLCBgYlsyLDJdYC4gVGhpcyBpcyBhIHN0cm9uZyBpbnRlcmFjdGlvbiB0aGF0IHdvdWxkIGJlIG1pc3NlZCBpZiB3ZSBoYWQgbWFkZSB0aGUgZWZmZWN0cyBwdXJlbHkgYWRkaXRpdmUgd2l0aCBvbmUgYW5vdGhlciAob24gdGhlIGxvZy1vZGRzIHNjYWxlKS4gVGhlIFZvbmVzaCAmIEJvbGtlciBwYXBlciB0aGF0IHRoZXNlIGRhdGEgY29tZSBmcm9tIGdvZXMgaW50byB0aGlzIGludGVyYWN0aW9uIGluIGdyZWF0IGRlcHRoLgoKVGhlIHByb2JsZW0gYXNrZWQgZm9yIGEganVzdGlmaWNhdGlvbiBvZiB0aGUgbW9kZWwgaW4gdGVybXMgb2YgdGhlIERBRy4gCgoKYGBge3IgZmlnLmhlaWdodD0yLCBmaWcud2lkdGg9MiwgZWNobz1GQUxTRX0KbGlicmFyeShkYWdpdHR5KQoKZyA8LSBkYWdpdHR5KCdkYWcgewpiYj0iMCwwLDEsMSIKRCBbcG9zPSIwLjY3MCwwLjIxOCJdCkcgW3Bvcz0iMC43ODcsMC40NTMiXQpQIFtwb3M9IjAuMjAzLDAuNDU3Il0KUyBbb3V0Y29tZSxwb3M9IjAuNTE0LDAuNDQ4Il0KVCBbcG9zPSIwLjM1NCwwLjIwMCJdCkQgLT4gUwpHIC0+IFMKUCAtPiBTClQgLT4gUwp9CicpCnBsb3QoZykKYGBgCgpUaGlzIGlzIGFuIGV4cGVyaW1lbnQsIHNvIHdlIGtub3cgdGhlIHRyZWF0bWVudHMgUCwgRywgYW5kIEQgYXJlIG5vdCBjb25mb3VuZGVkLiBBdCBsZWFzdCBub3QgaW4gYW55IG9idmlvdXMgd2F5LiBBbmQgdGhlbiB1bm9ic2VydmVkIHRhbmsgZWZmZWN0cyBUIGFsc28gbW9kZXJhdGUgdGhlIGluZmx1ZW5jZSBvZiB0aGUgdHJlYXRtZW50cy4gVGhlIG1vZGVsIEkgdXNlZCB0cmllcyB0byBlc3RpbWF0ZSBob3cgUCBhbmQgRyBtb2RlcmF0ZSBvbmUgYW5vdGhlci4gSXQgaWdub3JlcyBELCB3aGljaCB3ZSBhcmUgYWxsb3dlZCB0byBkbywgYmVjYXVzZSBpdCBpcyBub3QgYSBjb25mb3VuZCwganVzdCBhIGNvbXBldGluZyBjYXVzZS4gQnV0IEkgaW5jbHVkZSB0YW5rcywgd2hpY2ggaXMgYWxzbyBqdXN0IGEgY29tcGV0aW5nIGNhdXNlLiBJbmNsdWRpbmcgY29tcGV0aW5nIGNhdXNlcyBoZWxwcyB3aXRoIHByZWNpc2lvbiwgaWYgbm90aGluZyBlbHNlLgoKVGhleSBqdXN0IHNob3cgaW5wdXRzIGFuZCBvdXRwdXRzLiBUbyBqdXN0aWZ5IGFueSBwYXJ0aWN1bGFyIHN0YXRpc3RpY2FsIG1vZGVsLCB5b3UgbmVlZCBtb3JlIHRoYW4gdGhlIERBRy4KCiMjIFF1ZXN0aW9uIDMKCk5vdyBlc3RpbWF0ZSB0aGUgY2F1c2FsIGVmZmVjdCBvZiBkZW5zaXR5IG9uIHN1cnZpdmFsLiBDb25zaWRlciB3aGV0aGVyIHByZWQgbW9kaWZpZXMgdGhlIGVmZmVjdCBvZiBkZW5zaXR5LiBUaGVyZSBhcmUgc2V2ZXJhbCBnb29kIHdheXMgdG8gaW5jbHVkZSBkZW5zaXR5IGluIHlvdXIgQmlub21pYWwgR0xNLiBZb3UgY291bGQgdHJlYXQgaXQgYXMgYSBjb250aW51b3VzIHJlZ3Jlc3Npb24gdmFyaWFibGUgKHBvc3NpYmx5IHN0YW5kYXJkaXplZCkuIE9yIHlvdSBjb3VsZCBjb252ZXJ0IGl0IHRvIGFuIG9yZGVyZWQgY2F0ZWdvcnkgKHdpdGggdGhyZWUgbGV2ZWxzKS4gCgpDb21wYXJlIHRoZSAkXHNpZ21hJCAodGFuayBzdGFuZGFyZCBkZXZpYXRpb24pIHBvc3RlcmlvciBkaXN0cmlidXRpb24gdG8gJFxzaWdtYSQgZnJvbSB5b3VyIG1vZGVsIGluIFByb2JsZW0gMi4gSG93IGFyZSB0aGV5IGRpZmZlcmVudD8gV2h5PwoKRGVuc2l0eSBpcyBhbiBpbXBvcnRhbnQgZmFjdG9yIGluIHRoZXNlIGV4cGVyaW1lbnRzLiBTbyBsZXTigJlzIGluY2x1ZGUgaXQgZmluYWxseS4gSSB3aWxsIGRvIHNvbWV0aGluZyBzaW1wbGUsIGp1c3QgaW5jbHVkZSBpdCBhcyBhbiBhZGRpdGl2ZSBlZmZlY3QgdGhhdCBpbnRlcmFjdHMgd2l0aCBwcmVkYXRvcnMuIEJ1dCBJIHdpbGwgdXNlIHRoZSBsb2dhcml0aG0gb2YgZGVuc2l0eSwgc28gdGhhdCBpdCBoYXMgaW1wbGllZCBkaW1pbmlzaGluZyByZXR1cm5zIG9uIHRoZSBsb2ctb2RkcyBzY2FsZS4KCmBgYHtyfQpkYXQkRG8gPC0gc3RhbmRhcmRpemUobG9nKGQkZGVuc2l0eSkpCgptMyA8LSB1bGFtKAogIGFsaXN0KAogICAgUyB+IGJpbm9taWFsKCBEICwgcCApLAogICAgbG9naXQocCkgPC0gYVtUXSArIGJbUCxHXSArIGJEW1BdKkRvLAogICAgYVtUXSB+IG5vcm1hbCggMCAsIHNpZ21hICksCiAgICBtYXRyaXhbUCxHXTpiIH4gbm9ybWFsKCAwICwgMSApLAogICAgYkRbUF0gfiBub3JtYWwoMCwwLjUpLAogICAgc2lnbWEgfiBleHBvbmVudGlhbCggMSApCiAgKSwgZGF0YT1kYXQgLCBjaGFpbnM9NCAsIGNvcmVzPTQgLCBsb2dfbGlrPVRSVUUgKQoKcHJlY2lzKG0zLDMscGFycz1jKCJiIiwiYkQiLCJzaWdtYSIpKQpgYGAKCkFnYWluIGFuIGludGVyYWN0aW9uLiBIaWdoZXIgZGVuc2l0aWVzIGFyZSB3b3JzZSBmb3Igc3Vydml2YWwsIGJ1dCBvbmx5IGluIHRoZSBwcmVzZW5jZSBvZiBwcmVkYXRvcnMuIFRoZSBvdGhlciBlc3RpbWF0ZXMgYXJlIG5vdCBjaGFuZ2VkIG11Y2guIFRoZSAkXHNpZ21hJCBlc3RpbWF0ZSBoZXJlIGlzIGEgbGl0dGxlIHNtYWxsZXIgdGhhbiBpbiB0aGUgcHJldmlvdXMgcHJvYmxlbS4gVGhpcyBpcyBqdXN0IGJlY2F1c2UgZGVuc2l0eSBpcyBhbiByZWFsIGNhdXNlIG9mIHN1cnZpdmFsLCBzbyBpdCBleHBsYWlucyBzb21lIG9mIHRoZSB2YXJpYXRpb24gdGhhdCB3YXMgcHJldmlvdXNseSBzb2FrZWQgdXAgYnkgdGFua3Mgd2l0aCBkaWZmZXJlbnQgZGVuc2l0aWVzLg==" download="07-problem-set-solutions.Rmarkdown">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

## Question 1

Conduct a prior predictive simulation for the Reedfrog model. By this I mean to simulate the prior distribution of tank survival probabilities `\(\alpha_{j}\)`.

Start by using these priors:

`\(\alpha_{j} \sim Normal(\bar{\alpha},\sigma)\)`

`\(\bar{\alpha} \sim Normal(0, 1)\)`

`\(\sigma \sim Exponential(1)\)`

Be sure to transform the `\(\alpha_{j}\)` values to the probability scale for plotting and summary.

How does increasing the width of the prior on σ change the prior distribution of `\(\alpha_{j}\)`?

You might try `Exponential(10)` and `Exponential(0.1)` for example.

Simulating varying effect priors is in principle like simulating any other priors. The only difference is that the parameters have an implied order now, because some parameters depend upon others. So in this problem we must simulate `\(\sigma\)` and `\(\bar{\alpha}\)` first, and then we can simulate the individual tank `\(\alpha_{T}\)` variables

``` r
library(rethinking)
n <- 1e4
sigma <- rexp(n,1)
abar <- rnorm(n,0,1)
aT <- rnorm(n,abar,sigma)
dens(inv_logit(aT),xlim=c(0,1),adj=0.1,lwd=4,col=2, main="sigma~exponential(0,1)")
```

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Let’s also run two more (0.1 and 10):

``` r
n <- 1e4
sigma <- rexp(n,0.1)
abar <- rnorm(n,0,1)
aT <- rnorm(n,abar,sigma)
dens(inv_logit(aT),xlim=c(0,1),adj=0.1,lwd=4,col=2, main="sigma~exponential(0,0.1)")
```

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-3-1.png" width="672" />

``` r
n <- 1e4
sigma <- rexp(n,10)
abar <- rnorm(n,0,1)
aT <- rnorm(n,abar,sigma)
dens(inv_logit(aT),xlim=c(0,1),adj=0.1,lwd=4,col=2, main="sigma~exponential(0,10)")
```

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Increasing the variation across tanks, by making the `\(\sigma\)` distribution wider, pushes prior survival up against the floor and ceiling of the outcome space. This is the same phenomenon you saw before for ordinary logit models. The key lesson again is that flat priors on one scale are not necessarily flat on another.

## Question 2

Revisit the Reedfrog survival data, `data(reedfrogs)`. Start with the varying effects model from the book and lecture. Then modify it to estimate the causal effects of the treatment variables pred and size, including how size might modify the effect of predation. An easy approach is to estimate an effect for each combination of pred and size. Justify your model with a DAG of this experiment.

``` r
library(rethinking)
data(reedfrogs)
d <- reedfrogs

dat <- list(
S = d$surv,
D = d$density,
T = 1:nrow(d),
P = ifelse( d$pred=="no" , 1L , 2L ),
G = ifelse( d$size=="small" , 1L , 2L ) )

m2 <- ulam(
  alist(
    S ~ binomial( D , p ),
    logit(p) <- a[T] + b[P,G],
    a[T] ~ normal( 0 , sigma ),
    matrix[P,G]:b ~ normal( 0 , 1 ),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )
```

``` language-r
## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
## 
## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 1 finished in 0.1 seconds.
## Chain 2 finished in 0.1 seconds.
## Chain 3 finished in 0.1 seconds.
## Chain 4 finished in 0.1 seconds.
## 
## All 4 chains finished successfully.
## Mean chain execution time: 0.1 seconds.
## Total execution time: 0.2 seconds.
```

``` r
precis(m2,3,pars=c("b","sigma"))
```

``` language-r
##              mean        sd        5.5%       94.5%     n_eff     Rhat4
## b[1,1]  2.3614622 0.3014024  1.87941375  2.83508090 1335.2624 0.9983588
## b[1,2]  2.4918285 0.3102111  2.00800115  3.00209350 1410.8184 1.0008440
## b[2,1]  0.4464155 0.2492295  0.06604155  0.84732253  854.7499 1.0025240
## b[2,2] -0.4197933 0.2492729 -0.80027971 -0.01495176  666.0142 1.0048726
## sigma   0.7274569 0.1397025  0.52282539  0.96588100  483.5875 1.0020042
```

The parameters are in order from top to bottom: no-pred/small, no-pred/large, pred/small, pred/large. The curious thing is not that survival is lower with predation, but rather that it is lowest for large tadpoles, `b[2,2]`. This is a strong interaction that would be missed if we had made the effects purely additive with one another (on the log-odds scale). The Vonesh & Bolker paper that these data come from goes into this interaction in great depth.

The problem asked for a justification of the model in terms of the DAG.

<img src="/assignment/07-problem-set-solutions_files/figure-html/unnamed-chunk-6-1.png" width="192" />

This is an experiment, so we know the treatments P, G, and D are not confounded. At least not in any obvious way. And then unobserved tank effects T also moderate the influence of the treatments. The model I used tries to estimate how P and G moderate one another. It ignores D, which we are allowed to do, because it is not a confound, just a competing cause. But I include tanks, which is also just a competing cause. Including competing causes helps with precision, if nothing else.

They just show inputs and outputs. To justify any particular statistical model, you need more than the DAG.

## Question 3

Now estimate the causal effect of density on survival. Consider whether pred modifies the effect of density. There are several good ways to include density in your Binomial GLM. You could treat it as a continuous regression variable (possibly standardized). Or you could convert it to an ordered category (with three levels).

Compare the `\(\sigma\)` (tank standard deviation) posterior distribution to `\(\sigma\)` from your model in Problem 2. How are they different? Why?

Density is an important factor in these experiments. So let’s include it finally. I will do something simple, just include it as an additive effect that interacts with predators. But I will use the logarithm of density, so that it has implied diminishing returns on the log-odds scale.

``` r
dat$Do <- standardize(log(d$density))

m3 <- ulam(
  alist(
    S ~ binomial( D , p ),
    logit(p) <- a[T] + b[P,G] + bD[P]*Do,
    a[T] ~ normal( 0 , sigma ),
    matrix[P,G]:b ~ normal( 0 , 1 ),
    bD[P] ~ normal(0,0.5),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )
```

``` language-r
## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
## 
## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
## Chain 1 finished in 0.1 seconds.
## Chain 2 finished in 0.2 seconds.
## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
## Chain 3 finished in 0.2 seconds.
## Chain 4 finished in 0.1 seconds.
## 
## All 4 chains finished successfully.
## Mean chain execution time: 0.2 seconds.
## Total execution time: 0.4 seconds.
```

``` r
precis(m3,3,pars=c("b","bD","sigma"))
```

``` language-r
##              mean        sd       5.5%        94.5%     n_eff     Rhat4
## b[1,1]  2.3593300 0.2932911  1.9027953  2.863030700 1218.5607 1.0025928
## b[1,2]  2.4813305 0.2938420  2.0102535  2.943844750 1732.2746 0.9996898
## b[2,1]  0.5385628 0.2394553  0.1605141  0.917684120  841.9203 1.0015666
## b[2,2] -0.3608224 0.2329186 -0.7229110  0.004728589  906.0308 1.0021550
## bD[1]   0.1388237 0.2147237 -0.2024534  0.477723720 1814.4378 0.9989649
## bD[2]  -0.4743264 0.1656581 -0.7337175 -0.213861705 1353.4145 0.9996741
## sigma   0.6383159 0.1352702  0.4434156  0.870755710  406.2424 1.0035301
```

Again an interaction. Higher densities are worse for survival, but only in the presence of predators. The other estimates are not changed much. The `\(\sigma\)` estimate here is a little smaller than in the previous problem. This is just because density is an real cause of survival, so it explains some of the variation that was previously soaked up by tanks with different densities.
