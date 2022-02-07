---
title: Problem Set 2 Solutions
date: "2022-02-07"
menu:
  assignment:
    parent: Problem sets
    weight: 2
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on February 6, 2022 at 11:59am.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCAyCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBGZWJydWFyeSA3LCAyMDIyIGF0IDExOjU5YW0uCgotICoqTmFtZSoqOgotICoqVU5DQyBJRCoqOiAKLSAqKk90aGVyIHN0dWRlbnQgd29ya2VkIHdpdGggKG9wdGlvbmFsKSoqOgoKIyMgUXVlc3Rpb24gMQoKQ29uc3RydWN0IGEgbGluZWFyIHJlZ3Jlc3Npb24gb2Ygd2VpZ2h0IGFzIHByZWRpY3RlZCBieSBoZWlnaHQsIHVzaW5nIHRoZSBhZHVsdHMgKGFnZSAxOCBvciBncmVhdGVyKSBmcm9tIHRoZSBIb3dlbGwxIGRhdGFzZXQuIFRoZSBoZWlnaHRzIGxpc3RlZCBiZWxvdyB3ZXJlIHJlY29yZGVkIGluIHRoZSAhS3VuZyBjZW5zdXMsIGJ1dCB3ZWlnaHRzIHdlcmUgbm90IHJlY29yZGVkIGZvciB0aGVzZSBpbmRpdmlkdWFscy4gCgpQcm92aWRlIHByZWRpY3RlZCB3ZWlnaHRzIGFuZCA4OSUgY29tcGF0aWJpbGl0eSBpbnRlcnZhbHMgZm9yIGVhY2ggb2YgdGhlc2UgaW5kaXZpZHVhbHMuIEZpbGwgaW4gdGhlIHRhYmxlIGJlbG93LCB1c2luZyBtb2RlbC1iYXNlZCBwcmVkaWN0aW9ucy4KCmBgYHtyfQpsaWJyYXJ5KHJldGhpbmtpbmcpCmRhdGEoSG93ZWxsMSkKZCA8LSBIb3dlbGwxW0hvd2VsbDEkYWdlPj0xOCxdCgpkYXQgPC0gbGlzdChXID0gZCR3ZWlnaHQsIEggPSBkJGhlaWdodCwgSGJhciA9IG1lYW4oZCRoZWlnaHQpKQoKbV9hZHVsdHMgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgICAgICBtdSA8LSBhICsgYiooIEggLSBIYmFyICksCiAgICAgICAgYSB+IGRub3JtKCA2MCAsIDEwICkgLAogICAgICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgICAgIHNpZ21hIH4gZHVuaWYoIDAgLCAxMCApCiAgICApICwgZGF0YT1kYXQgKQoKc2V0LnNlZWQoMTAwKQpzYW1wbGVfaGVpZ2h0cyA9IGMoMTQwLDE1MCwxNjAsMTc1KQoKc2ltdWxfd2VpZ2h0cyA8LSBzaW0oIG1fYWR1bHRzICwgZGF0YT1saXN0KEg9c2FtcGxlX2hlaWdodHMsSGJhcj1tZWFuKGQkaGVpZ2h0KSkpIAoKIyBzaW11bGF0ZWQgbWVhbnMKbWVhbl93ZWlnaHRzID0gYXBwbHkoIHNpbXVsX3dlaWdodHMgLCAyICwgbWVhbiApCiMgc2ltdWxhdGVkIFBJJ3MKcGlfd2VpZ2h0cyA9IGFwcGx5KCBzaW11bF93ZWlnaHRzICwgMiAsIFBJICwgcHJvYj0wLjg5ICkKCmRmIDwtIGRhdGEuZnJhbWUoCiAgaW5kaXZpZHVhbCA9IDE6NCwKICBzYW1wbGVfaGVpZ2h0cyA9IHNhbXBsZV9oZWlnaHRzLAogIHNpbXVsYXRlZF9tZWFuID0gbWVhbl93ZWlnaHRzLAogIGxvd19waSA9IHBpX3dlaWdodHNbMSxdLAogIGhpZ2hfcGkgPSBwaV93ZWlnaHRzWzIsXQopCgpkZgpgYGAKCmBgYHtyfQpjb2xfbmFtZSA8LSBjKCdJbmRpdmlkdWFsJywnSGVpZ2h0JywgJ0V4cGVjdGVkIFdlaWdodCcsICdMb3cgSW50ZXJ2YWwnLCAnSGlnaCBJbnRlcnZhbCcpCiMgc2VlIGh0dHBzOi8vYm9va2Rvd24ub3JnL3lpaHVpL3JtYXJrZG93bi1jb29rYm9vay9rYWJsZS5odG1sCiMgZnlpIHRoaXMgaXMgYSB3YXkgdG8gImZhbmN5IiBwcmludCBkYXRhZnJhbWUgaW4gUk1hcmtkb3duCmtuaXRyOjprYWJsZShkZiwgY29sLm5hbWVzID0gY29sX25hbWUpCmBgYAoKIyMgUXVlc3Rpb25zIDItNAoKQSBzYW1wbGUgb2Ygc3R1ZGVudHMgaXMgbWVhc3VyZWQgZm9yIGhlaWdodCBlYWNoIHllYXIgZm9yIDMgeWVhcnMuIEFmdGVyIHRoZSB0aGlyZCB5ZWFyLCB5b3Ugd2FudCB0byBmaXQgYSBsaW5lYXIgcmVncmVzc2lvbiBwcmVkaWN0aW5nIGhlaWdodCAoaW4gY2VudGltZXRlcnMpIHVzaW5nIHllYXIgYXMgYSBwcmVkaWN0b3IuIAoKIyMjIFF1ZXN0aW9uIDI6CgotIFdyaXRlIGRvd24gYSBtYXRoZW1hdGljYWwgbW9kZWwgZGVmaW5pdGlvbiBmb3IgdGhpcyByZWdyZXNzaW9uLCB1c2luZyBhbnkgdmFyaWFibGUgbmFtZXMgYW5kIHByaW9ycyB5b3UgY2hvb3NlLiBZb3UgZG9uJ3QgbmVlZCB0byBydW4gc2luY2UgeW91IHdvbid0IGhhdmUgdGhlIGRhdGEuIFlvdSBtYXkgYWxzbyB3cml0ZSBkb3duIHlvdXIgZXF1YXRpb24gYW5kIHRoZW4gdXBsb2FkIGFuIGltYWdlLgoKV2UnbGwgZGVmaW5lIGFzIHRoZSBvdXRjb21lIHZhcmlhYmxlIGhlaWdodCAkaF97aWp9JCwgd2hlcmUgXGl0e2l9IGlzIHRoZSBzdHVkZW50IFxpdHtpfSBhbmQgXGl0e2p9IGlzIHRoZSB5ZWFyIFxpdHtqfS4gRm9yIGhlbHAgd2l0aCBMYVRlWCBlcXVhdGlvbnMsIHNlZSB0aGlzCgokaF97aWp9IFxzaW0gTm9ybWFsKHVfe2lqfSxcc2lnbWEpJAoKJFxtdV97aWp9ID0gXGFscGhhICsgXGJldGEgKiAoeF97an0gLSBcYmFye3h9KSQKCiRcYWxwaGEgXHNpbSBOb3JtYWwoMTAwLCAxMCkkCgokXGJldGEgXHNpbSBOb3JtYWwoMCwgMTApJAoKJFxzaWdtYSBcc2ltIEV4cG9uZW50aWFsKDEpJAoKd2hlcmUgKmgqIGlzIGhlaWdodCBhbmQgKnkqIGlzIHllYXIgYW5kICp5KiB0aGUgYXZlcmFnZSB5ZWFyIGluIHRoZSBzYW1wbGUuIFRoZSBpbmRleCAqaSogaW5kaWNhdGVzIHRoZSBzdHVkZW50IGFuZCB0aGUgaW5kZXggKmoqIGluZGljYXRlcyB0aGUgeWVhcgoKVGhlIHByb2JsZW0gZGlkbuKAmXQgc2F5IGhvdyBvbGQgdGhlIHN0dWRlbnRzIGFyZSwgc28geW914oCZbGwgaGF2ZSB0byBkZWNpZGUgZm9yIHlvdXJzZWxmLiBUaGUgcHJpb3JzIGFib3ZlIGFzc3VtZSB0aGUgc3R1ZGVudHMgYXJlIHN0aWxsIGdyb3dpbmcsIHNvIHRoZSBtZWFuIGhlaWdodCDOsSBpcyBzZXQgYXJvdW5kIDEwMCBjbS4gVGhlIHNsb3BlIHdpdGggeWVhciDOsiBpcyB2YWd1ZSBoZXJl4oCUd2XigJlsbCBkbyBiZXR0ZXIgaW4gdGhlIG5leHQgcHJvYmxlbS4gRm9yIM+DLCB0aGlzIG5lZWRzIHRvIGV4cHJlc3MgaG93IHZhcmlhYmxlIHN0dWRlbnRzIGFyZSBpbiB0aGUgc2FtZSB5ZWFyCgojIyMgUXVlc3Rpb24gMwoKLSBSdW4gcHJpb3IgcHJlZGljdGl2ZSBzaW11bGF0aW9uIGFuZCBkZWZlbmQgeW91ciBjaG9pY2Ugb2YgcHJpb3JzLgoKYGBge3J9Cm4gPC0gNTAKYSA8LSBybm9ybSggbiAsIDEwMCAsIDEwICkKYiA8LSBybm9ybSggbiAsIDAgLCAxMCApCnMgPC0gcmV4cCggbiAsIDEgKQoKeCA8LSAxOjMKeGJhciA8LSBtZWFuKHgpCiMgbWF0cml4IG9mIGhlaWdodHMsIHN0dWRlbnRzIGluIHJvd3MgYW5kIHllYXJzIGluIGNvbHVtbnMKaCA8LSBtYXRyaXgoIE5BICwgbnJvdz1uICwgbmNvbD0zICkKZm9yICggaSBpbiAxOm4gKSBmb3IgKCBqIGluIDE6MyApCiAgaFsgaSAsIGogXSA8LSBybm9ybSggMSAsIGFbaV0gKyBiW2ldKiggeFtqXSAtIHhiYXIgKSAsIHNbaV0gKQoKcGxvdCggTlVMTCAsIHhsaW09YygxLDMpICwgeWxpbT1yYW5nZShoKSAsIHhsYWI9InllYXIiICwgeWxhYj0iaGVpZ2h0IChjbSkiICkKZm9yICggaSBpbiAxOm4gKSBsaW5lcyggMTozICwgaFtpLF0gKQpgYGAKCiMjIyBRdWVzdGlvbiA0CgoqKk5vdyBzdXBwb3NlIEkgdGVsbCB5b3UgdGhhdCB0aGUgc3R1ZGVudHMgd2VyZSA4IHRvIDEwIHllYXJzIG9sZCAoc28gOCB5ZWFyIDEsIDkgeWVhciAyLCBldGMuKS4gV2hhdCBkbyB5b3UgZXhwZWN0IG9mIHRoZSB0cmVuZCBvZiBzdHVkZW50cycgaGVpZ2h0cyBvdmVyIHRpbWU/KioKCi0gKipEb2VzIHRoaXMgaW5mb3JtYXRpb24gbGVhZCB5b3UgdG8gY2hhbmdlIHlvdXIgY2hvaWNlIG9mIHByaW9ycz8gSG93PyBSZXNpbXVsYXRlIHlvdXIgcHJpb3JzIGZyb20gUXVlc3Rpb24gMy4qKgoKQSBzaW1wbGUgd2F5IHRvIGZvcmNlIGluZGl2aWR1YWxzIHRvIGdldCB0YWxsZXIgZnJvbSBvbmUgeWVhciB0byB0aGUgbmV4dCBpcyB0byBjb25zdHJhaW4gdGhlIHNsb3BlIM6yIHRvIGJlIHBvc2l0aXZlLiBBIGxvZy1ub3JtYWwgZGlzdHJpYnV0aW9uIG1ha2VzIHRoaXMgcmF0aGVyIGVhc3ksIGJ1dCB5b3UgZG8gaGF2ZSB0byBiZSBjYXJlZnVsIGluIGNob29zaW5nIGl0cyBwYXJhbWV0ZXIgdmFsdWVzLiBSZWNhbGwgdGhhdCB0aGUgbWVhbiBvZiBhIGxvZy1ub3JtYWwgaXMgJGV4cChcbXUgKyBcc2lnbWFeezJ9IC8gMikkLiBMZXTigJlzIHRyeSAkXG11ID0gMSQgYW5kICRcc2lnbWEgPSAwLjUkLiBUaGlzIHdpbGwgZ2l2ZSBhIG1lYW4gJGV4cCgxICsgKDAuMjUpXnsyfSAvIDIpIOKJiCAzJCwgb3IgMyBjbSBvZiBncm93dGggcGVyIHllYXIuCgpgYGB7cn0KbiA8LSA1MAphIDwtIHJub3JtKCBuICwgMTAwICwgMTAgKQpiIDwtIHJsbm9ybSggbiAsIDEgLCAwLjUgKQpzIDwtIHJleHAoIG4gLCAxICkKeCA8LSAxOjMKeGJhciA8LSBtZWFuKHgpCiMgbWF0cml4IG9mIGhlaWdodHMsIHN0dWRlbnRzIGluIHJvd3MgYW5kIHllYXJzIGluIGNvbHVtbnMKaCA8LSBtYXRyaXgoIE5BICwgbnJvdz1uICwgbmNvbD0zICkKZm9yICggaSBpbiAxOm4gKSBmb3IgKCBqIGluIDE6MyApCiAgaFsgaSAsIGogXSA8LSBybm9ybSggMSAsIGFbaV0gKyBiW2ldKiggeFtqXSAtIHhiYXIgKSAsIHNbaV0gKQpwbG90KCBOVUxMICwgeGxpbT1jKDEsMykgLCB5bGltPXJhbmdlKGgpICwgeGxhYj0ieWVhciIgLCB5bGFiPSJoZWlnaHQgKGNtKSIgKQogIGZvciAoIGkgaW4gMTpuICkgbGluZXMoIDE6MyAsIGhbaSxdICkKYGBgCgpBc2lkZSBmcm9tIGEgZmV3IHBlb3BsZSwgdGhlIGxpbmVzIGFyZSBzbG9wZSB1cHdhcmRzLiBXaHkgZG8gc29tZSBvZiB0aGVtIHppZy16YWc/IEJlY2F1c2UgdGhlIHZhcmlhdGlvbiBhcm91bmQgdGhlIGV4cGVjdGF0aW9uIGZyb20gz4MgYWxsb3dzIGl0LiBJZiB5b3UgdGhpbmsgb2YgdGhpcyBhcyBtZWFzdXJlbWVudCBlcnJvciwgaXTigJlzIG5vdCBuZWNlc3NhcmlseSBiYWQuIElmIG1lYXN1cmVtZW50IGVycm9yIGlzIHNtYWxsIGhvd2V2ZXIsIHlvdeKAmWQgaGF2ZSB0byB0aGluayBoYXJkZXIuIE11Y2ggbGF0ZXIgaW4gdGhlIGJvb2ssIHlvdeKAmWxsIGxlYXJuIHNvbWUgdG9vbHMgdG8gaGVscCB3aXRoIHRoaXMga2luZCBvZiBwcm9ibGVtLgoKIyMgUXVlc3Rpb24gNQoKKio0LiBSZWZpdCBtb2RlbCBtNC4zIGZyb20gdGhlIGNoYXB0ZXIsIGJ1dCBvbWl0IHRoZSBtZWFuIHdlaWdodCB4YmFyIHRoaXMgdGltZS4gQ29tcGFyZSB0aGUgbmV3IG1vZGVs4oCZcyBwb3N0ZXJpb3IgdG8gdGhhdCBvZiB0aGUgb3JpZ2luYWwgbW9kZWwuIEluIHBhcnRpY3VsYXIsIGxvb2sgYXQgdGhlIGNvdmFyaWFuY2UgYW1vbmcgdGhlIHBhcmFtZXRlcnMuIFdoYXQgaXMgZGlmZmVyZW50PyBUaGVuIGNvbXBhcmUgdGhlIHBvc3RlcmlvciBwcmVkaWN0aW9ucyBvZiBib3RoIG1vZGVscy4qKgoKYGBge3J9CmxpYnJhcnkocmV0aGlua2luZykKZGF0YShIb3dlbGwxKTsgCmQgPC0gSG93ZWxsMTsgCmQyIDwtIGRbIGQkYWdlID49IDE4ICwgXQoKeGJhciA8LSBtZWFuKGQyJHdlaWdodCkKIyBydW4gb3JpZ2luYWwgbW9kZWwgZnJvbSBjb2RlCm00LjMgPC0gcXVhcCgKICBhbGlzdCgKICAgIGhlaWdodCB+IGRub3JtKCBtdSAsIHNpZ21hICkgLAogICAgbXUgPC0gYSArIGIqKHdlaWdodCAtIHhiYXIpLAogICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgc2lnbWEgfiBkdW5pZiggMCAsIDUwICkKKSAsIGRhdGE9ZDIgKQpwcmVjaXMoIG00LjMgKQpgYGAKCmBgYHtyfQptNC4zYiA8LSBxdWFwKAogIGFsaXN0KAogICAgaGVpZ2h0IH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICBtdSA8LSBhICsgYip3ZWlnaHQgLAogICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgc2lnbWEgfiBkdW5pZiggMCAsIDUwICkKKSAsIGRhdGE9ZDIgKQpwcmVjaXMoIG00LjNiICkKYGBgCgpOb3RpY2UgdGhhdCB0aGUgbW9kZWxzIHlpZWxkIGlkZW50aWNhbCBvdXRwdXRzLgoKTGV0J3Mgbm93IGxvb2sgYXQgdGhlIGNvdmFyaWF0aW9uIHdoaWNoIGlzIGR1ZSB0byBydW5uaW5nIHF1YWRyYXRpYyBhcHByb3hpbWF0aW9uLgoKYGBge3J9CnJvdW5kKCB2Y292KCBtNC4zICkgLCAyICkKYGBgCgpGb2N1cyBvbiB0aGUgb2ZmLWRpYWdvbmFscyAuLi4gd2Ugc2VlIG5lYXJseSB6ZXJvIGNvdmFyaWF0aW9uLiBUaGlzIGlzIGdvb2QuCgpXZSBjYW4gYWxzbyB2aWV3IHRoaXMgdGhyb3VnaCB0aGUgYHBhaXJzKClgIGZ1bmN0aW9uLgoKYGBge3J9CnBhaXJzKG00LjMpCmBgYAoKTG9vayBhdCB0aGUgY292KEEsQikgcGxvdC4gTm90aWNlIHRoZXJlJ3Mgbm8gY292YXJpYXRpb24uCgpXaGlsZSBpZiB3ZSBkbyB0aGlzIGZvciB0aGUgbm9uLXNjYWxlZCwgd2UnbGwgc2VlIHNvbWV0aGluZyBkaWZmZXJlbnQuIEZpcnN0LCBsZXQncyBqdXN0IHJ1biBjb3ZhcmlhdGlvbi4KCmBgYHtyfQpyb3VuZCggdmNvdiggbTQuM2IgKSAsIDIgKQpgYGAKCk5vdGljZSBzb21lIGNvdmFyaWF0aW9uICgtMC4wOCkgZm9yIHRoZSBub24tc2NhbGVkLiBXaGlsZSAtMC4wOCBjb3ZhcmlhdGlvbiBpc24ndCBtdWNoLCBsZXQncyB0cmFuc2xhdGUgdGhpcyBpbnRvIGNvcnJlbGF0aW9uIChhcyBjb3ZhcmlhdGlvbiBkZXBlbmRzIG9uIHRoZSBzY2FsZSBvZiB2YXJpYWJsZXMpLgoKYGBge3J9CnJvdW5kKCBjb3YyY29yKHZjb3YoIG00LjNiICkpICwgMiApCmBgYAoKV293IC0tIHVuc2NhbGVkIG91ciBBLUIgYXJlIG5lYXJseSAtMSBuZWdhdGl2ZSBjb3JyZWxhdGlvbi4gV2UnbGwgc2VlIHRoaXMgaW4gdGhlIGBwYWlycygpYCBmdW5jdGlvbi4KCgpgYGB7cn0KcGFpcnMobTQuM2IpCmBgYAoKV2UgY2FuIGFsc28gcnVuIHBvc3RlcmlvciBwcmVkaWN0aW9uIHNpbXVsYXRpb25zLgoKRmlyc3QsIGxldCdzIHJ1biB0aGUgb3JpZ2luYWwgbW9kZWwgKHNjYWxlZCkuCgpgYGB7cn0KY29sMiA8LSBjb2wuYWxwaGEoMiwwLjgpCiMgZ2V0IHBvc3RlcmlvciBtZWFuIHZpYSBsaW5rIGZ1bmN0aW9uCnhzZXEgPC0gc2VxKGZyb209MCx0bz03NSxsZW49NzUpCm1lYW4gPSBtZWFuKGQyJHdlaWdodCkKbXUgPC0gbGluayhtNC4zLGRhdGE9bGlzdCh3ZWlnaHQgPSB4c2VxLCB4YmFyID0gbWVhbikpCm11Lm1lYW4gPC0gYXBwbHkoIG11ICwgMiAsIG1lYW4gKQptdS5QSSA8LSBhcHBseSggbXUgLCAyICwgUEkgLCBwcm9iPS44OSkKIyBnZXQgcG9zdGVyaW9yIHByZWRpY3Rpb25zIHZpYSBzaW0gZnVuY3Rpb24Kc2ltLndlaWdodCA8LSBzaW0oIG00LjMgLCBkYXRhPWxpc3Qod2VpZ2h0ID0geHNlcSwgV2JhciA9IG1lYW4oZDIkd2VpZ2h0KSkpCndlaWdodC5QSSA8LSBhcHBseSggc2ltLndlaWdodCAsIDIgLCBQSSAsIHByb2I9MC44OSApCgpwbG90KGQyJHdlaWdodCwgZDIkaGVpZ2h0LCBjb2w9Y29sMiwgbHdkPTMsCiAgICAgY2V4PTEuMiwgeGxhYj0id2VpZ2h0IChrZykiLCB5bGFiPSJoZWlnaHQgKGNtKSIpCmxpbmVzKHhzZXEsIG11Lm1lYW4sIGx3ZD00KQpzaGFkZSggbXUuUEkgLCB4c2VxICkKc2hhZGUoIHdlaWdodC5QSSAsIHhzZXEgKQpgYGAKCk5vdyBydW4gdGhlIHVuc2NhbGVkIG1vZGVsIHNpbXVsYXRpb25zLgoKYGBge3J9CmNvbDIgPC0gY29sLmFscGhhKDIsMC44KQojIGdldCBwb3N0ZXJpb3IgbWVhbiB2aWEgbGluayBmdW5jdGlvbgp4c2VxIDwtIHNlcShmcm9tPTAsdG89NzUsbGVuPTc1KQptZWFuID0gbWVhbihkMiR3ZWlnaHQpCm11IDwtIGxpbmsobTQuM2IsZGF0YT1saXN0KHdlaWdodCA9IHhzZXEpKQptdS5tZWFuIDwtIGFwcGx5KCBtdSAsIDIgLCBtZWFuICkKbXUuUEkgPC0gYXBwbHkoIG11ICwgMiAsIFBJICwgcHJvYj0uODkpCiMgZ2V0IHBvc3RlcmlvciBwcmVkaWN0aW9ucyB2aWEgc2ltIGZ1bmN0aW9uCnNpbS53ZWlnaHQgPC0gc2ltKCBtNC4zYiAsIGRhdGE9bGlzdCh3ZWlnaHQgPSB4c2VxLCBXYmFyID0gbWVhbihkMiR3ZWlnaHQpKSkKd2VpZ2h0LlBJIDwtIGFwcGx5KCBzaW0ud2VpZ2h0ICwgMiAsIFBJICwgcHJvYj0wLjg5ICkKCnBsb3QoZDIkd2VpZ2h0LCBkMiRoZWlnaHQsIGNvbD1jb2wyLCBsd2Q9MywKICAgICBjZXg9MS4yLCB4bGFiPSJ3ZWlnaHQgKGtnKSIsIHlsYWI9ImhlaWdodCAoY20pIikKbGluZXMoeHNlcSwgbXUubWVhbiwgbHdkPTQpCnNoYWRlKCBtdS5QSSAsIHhzZXEgKQpzaGFkZSggd2VpZ2h0LlBJICwgeHNlcSApCmBgYAoKCldlIGhhdmUgdGhlIGNvbmNsdXNpb24gdGhhdCB3aGlsZSB0aGVzZSBtb2RlbHMgbWFrZSB0aGUgc2FtZSBwb3N0ZXJpb3IgcHJlZGljdGlvbnMsIGJ1dCB0aGUgcGFyYW1ldGVycyBoYXZlIHF1aXRlIGRpZmZlcmVudCBtZWFuaW5ncyBhbmQgcmVsYXRpb25zaGlwcyB3aXRoIG9uZSBhbm90aGVyLiBUaGVyZSBpcyBub3RoaW5nIHdyb25nIHdpdGggdGhpcyBuZXcgdmVyc2lvbiBvZiB0aGUgbW9kZWwuIEJ1dCB1c3VhbGx5IGl0IGlzIG11Y2ggZWFzaWVyIHRvIHNldCBwcmlvcnMsIHdoZW4gd2UgY2VudGVyIHRoZSBwcmVkaWN0b3IgdmFyaWFibGVzLiBCdXQgeW91IGNhbiBhbHdheXMgdXNlIHByaW9yIHNpbXVsYXRpb25zIHRvIHNldCBzZW5zaWJsZSBwcmlvcnMsIHdoZW4gaW4gZG91YnQuCgojIyBQYWNrYWdlIHZlcnNpb25zCgpgYGB7cn0Kc2Vzc2lvbkluZm8oKQpgYGA=" download="02-problem-set-solutions.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

## Question 1

Construct a linear regression of weight as predicted by height, using the adults (age 18 or greater) from the Howell1 dataset. The heights listed below were recorded in the !Kung census, but weights were not recorded for these individuals.

Provide predicted weights and 89% compatibility intervals for each of these individuals. Fill in the table below, using model-based predictions.

``` r
library(rethinking)
data(Howell1)
d <- Howell1[Howell1$age>=18,]

dat <- list(W = d$weight, H = d$height, Hbar = mean(d$height))

m_adults <- quap(
    alist(
        W ~ dnorm( mu , sigma ) ,
        mu <- a + b*( H - Hbar ),
        a ~ dnorm( 60 , 10 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 10 )
    ) , data=dat )

set.seed(100)
sample_heights = c(140,150,160,175)

simul_weights <- sim( m_adults , data=list(H=sample_heights,Hbar=mean(d$height))) 

# simulated means
mean_weights = apply( simul_weights , 2 , mean )
# simulated PI's
pi_weights = apply( simul_weights , 2 , PI , prob=0.89 )

df <- data.frame(
  individual = 1:4,
  sample_heights = sample_heights,
  simulated_mean = mean_weights,
  low_pi = pi_weights[1,],
  high_pi = pi_weights[2,]
)

df
```

``` language-r
##   individual sample_heights simulated_mean   low_pi  high_pi
## 1          1            140       35.66492 28.94471 42.53722
## 2          2            150       42.15074 35.55812 48.65086
## 3          3            160       48.56264 41.84482 55.16735
## 4          4            175       57.88451 51.56018 64.33351
```

``` r
col_name <- c('Individual','Height', 'Expected Weight', 'Low Interval', 'High Interval')
# see https://bookdown.org/yihui/rmarkdown-cookbook/kable.html
# fyi this is a way to "fancy" print dataframe in RMarkdown
knitr::kable(df, col.names = col_name)
```

| Individual | Height | Expected Weight | Low Interval | High Interval |
|-----------:|-------:|----------------:|-------------:|--------------:|
|          1 |    140 |        35.66492 |     28.94471 |      42.53722 |
|          2 |    150 |        42.15074 |     35.55812 |      48.65086 |
|          3 |    160 |        48.56264 |     41.84482 |      55.16735 |
|          4 |    175 |        57.88451 |     51.56018 |      64.33351 |

## Questions 2-4

A sample of students is measured for height each year for 3 years. After the third year, you want to fit a linear regression predicting height (in centimeters) using year as a predictor.

### Question 2:

-   Write down a mathematical model definition for this regression, using any variable names and priors you choose. You don’t need to run since you won’t have the data. You may also write down your equation and then upload an image.

We’ll define as the outcome variable height `\(h_{ij}\)`, where

`\(h_{ij} \sim Normal(u_{ij},\sigma)\)`

`\(\mu_{ij} = \alpha + \beta * (x_{j} - \bar{x})\)`

`\(\alpha \sim Normal(100, 10)\)`

`\(\beta \sim Normal(0, 10)\)`

`\(\sigma \sim Exponential(1)\)`

where *h* is height and *y* is year and *y* the average year in the sample. The index *i* indicates the student and the index *j* indicates the year

The problem didn’t say how old the students are, so you’ll have to decide for yourself. The priors above assume the students are still growing, so the mean height α is set around 100 cm. The slope with year β is vague here—we’ll do better in the next problem. For σ, this needs to express how variable students are in the same year

### Question 3

-   Run prior predictive simulation and defend your choice of priors.

``` r
n <- 50
a <- rnorm( n , 100 , 10 )
b <- rnorm( n , 0 , 10 )
s <- rexp( n , 1 )

x <- 1:3
xbar <- mean(x)
# matrix of heights, students in rows and years in columns
h <- matrix( NA , nrow=n , ncol=3 )
for ( i in 1:n ) for ( j in 1:3 )
  h[ i , j ] <- rnorm( 1 , a[i] + b[i]*( x[j] - xbar ) , s[i] )

plot( NULL , xlim=c(1,3) , ylim=range(h) , xlab="year" , ylab="height (cm)" )
for ( i in 1:n ) lines( 1:3 , h[i,] )
```

<img src="/assignment/02-problem-set-solutions_files/figure-html/unnamed-chunk-4-1.png" width="672" />

### Question 4

**Now suppose I tell you that the students were 8 to 10 years old (so 8 year 1, 9 year 2, etc.). What do you expect of the trend of students’ heights over time?**

-   **Does this information lead you to change your choice of priors? How? Resimulate your priors from Question 3.**

A simple way to force individuals to get taller from one year to the next is to constrain the slope β to be positive. A log-normal distribution makes this rather easy, but you do have to be careful in choosing its parameter values. Recall that the mean of a log-normal is `\(exp(\mu + \sigma^{2} / 2)\)`. Let’s try `\(\mu = 1\)` and `\(\sigma = 0.5\)`. This will give a mean `\(exp(1 + (0.25)^{2} / 2) ≈ 3\)`, or 3 cm of growth per year.

``` r
n <- 50
a <- rnorm( n , 100 , 10 )
b <- rlnorm( n , 1 , 0.5 )
s <- rexp( n , 1 )
x <- 1:3
xbar <- mean(x)
# matrix of heights, students in rows and years in columns
h <- matrix( NA , nrow=n , ncol=3 )
for ( i in 1:n ) for ( j in 1:3 )
  h[ i , j ] <- rnorm( 1 , a[i] + b[i]*( x[j] - xbar ) , s[i] )
plot( NULL , xlim=c(1,3) , ylim=range(h) , xlab="year" , ylab="height (cm)" )
  for ( i in 1:n ) lines( 1:3 , h[i,] )
```

<img src="/assignment/02-problem-set-solutions_files/figure-html/unnamed-chunk-5-1.png" width="672" />

Aside from a few people, the lines are slope upwards. Why do some of them zig-zag? Because the variation around the expectation from σ allows it. If you think of this as measurement error, it’s not necessarily bad. If measurement error is small however, you’d have to think harder. Much later in the book, you’ll learn some tools to help with this kind of problem.

## Question 5

**4. Refit model m4.3 from the chapter, but omit the mean weight xbar this time. Compare the new model’s posterior to that of the original model. In particular, look at the covariance among the parameters. What is different? Then compare the posterior predictions of both models.**

``` r
library(rethinking)
data(Howell1); 
d <- Howell1; 
d2 <- d[ d$age >= 18 , ]

xbar <- mean(d2$weight)
# run original model from code
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*(weight - xbar),
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d2 )
precis( m4.3 )
```

``` language-r
##              mean         sd        5.5%       94.5%
## a     154.6012818 0.27029702 154.1692950 155.0332687
## b       0.9032859 0.04192198   0.8362865   0.9702853
## sigma   5.0716813 0.19113598   4.7662091   5.3771535
```

``` r
m4.3b <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d2 )
precis( m4.3b )
```

``` language-r
##            mean         sd        5.5%       94.5%
## a     114.53431 1.89774691 111.5013449 117.5672771
## b       0.89073 0.04175798   0.8239927   0.9574674
## sigma   5.07272 0.19124901   4.7670668   5.3783725
```

Notice that the models yield similar outputs **except for the a coefficients**.

Let’s now look at the covariation which is due to running quadratic approximation.

``` r
round( vcov( m4.3 ) , 2 )
```

``` language-r
##          a b sigma
## a     0.07 0  0.00
## b     0.00 0  0.00
## sigma 0.00 0  0.04
```

Focus on the off-diagonals … we see nearly zero covariation. This is good.

We can also view this through the `pairs()` function.

``` r
pairs(m4.3)
```

<img src="/assignment/02-problem-set-solutions_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Look at the cov(A,B) plot. Notice there’s no covariation.

While if we do this for the non-scaled, we’ll see something different. First, let’s just run covariation.

``` r
round( vcov( m4.3b ) , 2 )
```

``` language-r
##           a     b sigma
## a      3.60 -0.08  0.01
## b     -0.08  0.00  0.00
## sigma  0.01  0.00  0.04
```

Notice some covariation (-0.08) for the non-scaled. While -0.08 covariation isn’t much, let’s translate this into correlation (as covariation depends on the scale of variables).

``` r
round( cov2cor(vcov( m4.3b )) , 2 )
```

``` language-r
##           a     b sigma
## a      1.00 -0.99  0.03
## b     -0.99  1.00 -0.03
## sigma  0.03 -0.03  1.00
```

Wow – unscaled our A-B are nearly -1 negative correlation. We’ll see this in the `pairs()` function.

``` r
pairs(m4.3b)
```

<img src="/assignment/02-problem-set-solutions_files/figure-html/unnamed-chunk-12-1.png" width="672" />

We can also run posterior prediction simulations.

First, let’s run the original model (scaled).

``` r
col2 <- col.alpha(2,0.8)
# get posterior mean via link function
xseq <- seq(from=0,to=75,len=75)
mean = mean(d2$weight)
mu <- link(m4.3,data=list(weight = xseq, xbar = mean))
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=.89)
# get posterior predictions via sim function
sim.weight <- sim( m4.3 , data=list(weight = xseq, Wbar = mean(d2$weight)))
weight.PI <- apply( sim.weight , 2 , PI , prob=0.89 )

plot(d2$weight, d2$height, col=col2, lwd=3,
     cex=1.2, xlab="weight (kg)", ylab="height (cm)")
lines(xseq, mu.mean, lwd=4)
shade( mu.PI , xseq )
shade( weight.PI , xseq )
```

<img src="/assignment/02-problem-set-solutions_files/figure-html/unnamed-chunk-13-1.png" width="672" />

Now run the unscaled model simulations.

``` r
col2 <- col.alpha(2,0.8)
# get posterior mean via link function
xseq <- seq(from=0,to=75,len=75)
mean = mean(d2$weight)
mu <- link(m4.3b,data=list(weight = xseq))
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=.89)
# get posterior predictions via sim function
sim.weight <- sim( m4.3b , data=list(weight = xseq, Wbar = mean(d2$weight)))
weight.PI <- apply( sim.weight , 2 , PI , prob=0.89 )

plot(d2$weight, d2$height, col=col2, lwd=3,
     cex=1.2, xlab="weight (kg)", ylab="height (cm)")
lines(xseq, mu.mean, lwd=4)
shade( mu.PI , xseq )
shade( weight.PI , xseq )
```

<img src="/assignment/02-problem-set-solutions_files/figure-html/unnamed-chunk-14-1.png" width="672" />

We have the conclusion that while these models make the same posterior predictions, but the parameters have quite different meanings and relationships with one another. There is nothing wrong with this new version of the model. But usually it is much easier to set priors, when we center the predictor variables. But you can always use prior simulations to set sensible priors, when in doubt.

## Package versions

``` r
sessionInfo()
```

``` language-r
## R version 4.1.1 (2021-08-10)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Monterey 12.1
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices datasets  utils     methods  
## [8] base     
## 
## other attached packages:
## [1] rethinking_2.21      cmdstanr_0.4.0.9001  rstan_2.21.3        
## [4] ggplot2_3.3.5        StanHeaders_2.21.0-7
## 
## loaded via a namespace (and not attached):
##  [1] sass_0.4.0           jsonlite_1.7.2       bslib_0.3.1         
##  [4] RcppParallel_5.1.4   assertthat_0.2.1     posterior_1.1.0     
##  [7] distributional_0.2.2 highr_0.9            stats4_4.1.1        
## [10] tensorA_0.36.2       renv_0.14.0          yaml_2.2.1          
## [13] pillar_1.6.4         backports_1.4.1      lattice_0.20-44     
## [16] glue_1.6.0           uuid_1.0-3           digest_0.6.29       
## [19] checkmate_2.0.0      colorspace_2.0-2     htmltools_0.5.2     
## [22] pkgconfig_2.0.3      bookdown_0.24        purrr_0.3.4         
## [25] mvtnorm_1.1-3        scales_1.1.1         processx_3.5.2      
## [28] tibble_3.1.6         generics_0.1.1       farver_2.1.0        
## [31] ellipsis_0.3.2       withr_2.4.3          cli_3.1.0           
## [34] mime_0.12            magrittr_2.0.1       crayon_1.4.2        
## [37] evaluate_0.14        ps_1.6.0             fs_1.5.0            
## [40] fansi_0.5.0          MASS_7.3-54          pkgbuild_1.3.1      
## [43] blogdown_1.5         tools_4.1.1          loo_2.4.1           
## [46] prettyunits_1.1.1    lifecycle_1.0.1      matrixStats_0.61.0  
## [49] stringr_1.4.0        munsell_0.5.0        callr_3.7.0         
## [52] compiler_4.1.1       jquerylib_0.1.4      rlang_0.4.12        
## [55] grid_4.1.1           rstudioapi_0.13      base64enc_0.1-3     
## [58] rmarkdown_2.11       xaringanExtra_0.5.5  gtable_0.3.0        
## [61] codetools_0.2-18     inline_0.3.19        abind_1.4-5         
## [64] DBI_1.1.1            R6_2.5.1             gridExtra_2.3       
## [67] lubridate_1.8.0      knitr_1.36           dplyr_1.0.7         
## [70] fastmap_1.1.0        utf8_1.2.2           downloadthis_0.2.1  
## [73] bsplus_0.1.3         KernSmooth_2.23-20   shape_1.4.6         
## [76] stringi_1.7.6        Rcpp_1.0.7           vctrs_0.3.8         
## [79] tidyselect_1.1.1     xfun_0.28            coda_0.19-4
```
