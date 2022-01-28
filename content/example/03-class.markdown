---
date: "2022-01-27"
title: "Class 3"
menu:
  example:
    parent: Examples
weight: 3
toc: true
type: docs
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:text/x-markdown;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyAzIgotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQpgYGAKCiMjIEludHJvZHVjdGlvbgoKRm9yIHRoaXMgY2xhc3MsIHdlJ2xsIHJldmlldyBjb2RlIGV4YW1wbGVzIGZvdW5kIGluIENoYXB0ZXIgNC4KClRoaXMgYXNzdW1lcyB0aGF0IHlvdSBoYXZlIGFscmVhZHkgaW5zdGFsbGVkIHRoZSBgcmV0aGlua2luZ2AgcGFja2FnZS4KCklmIHlvdSBuZWVkIGhlbHAsIGJlIHN1cmUgdG8gcmVtZW1iZXIgdGhlIHJlZmVyZW5jZXMgaW4gdGhlIFtSZXNvdXJjZXNdKC9yZXNvdXJjZS8pOgoKKiBbSW5zdGFsbGluZyBSL1JTdHVkaW9dKC9yZXNvdXJjZS9pbnN0YWxsLykKKiBbSW5zdGFsbGluZyBgcmV0aGlua2luZ2AgcGFja2FnZV0oL3Jlc291cmNlL2luc3RhbGwtcmV0aGlua2luZy8pCiogW1JtYXJrZG93bl0oL3Jlc291cmNlL3JtYXJrZG93bi8pCiogW1IgU3R5bGUgZ3VpZGVdKC9yZXNvdXJjZS9zdHlsZS8pCgojIyBDaGFwdGVyIDQgCgojIyMgRHJhd2luZyB0aGUgT3dsCgohW10oLi4vLi4vaW1nL3N5bGxhYnVzL293bC5wbmcpCgoKIyMjIDEuIFF1ZXN0aW9uIG9yIGVzdGltYW5kCgpPYmplY3RpdmU6IERlc2NyaWJlIHRoZSBhc3NvY2lhdGlvbiBiZXR3ZWVuIEFkdWx0ICoqd2VpZ2h0KiogYW5kICoqaGVpZ2h0KioKCmBgYHtyfQpsaWJyYXJ5KHJldGhpbmtpbmcpCmRhdGEoSG93ZWxsMSkKZCA8LSBIb3dlbGwxW0hvd2VsbDEkYWdlPj0xOCxdCgpwbG90KGQkaGVpZ2h0LCBkJHdlaWdodCwgY29sID0gMiwgIHhsYWIgPSAiaGVpZ2h0IChjbSkiLCB5bGFiID0gIndlaWdodCAoa2cpIiwgIGx3ZD0zKQpgYGAKCiMjIyAyLiBTY2llbnRpZmljIG1vZGVsCgpXZWlnaHQgaXMgYSBmdW5jdGlvbiBvZiBoZWlnaHQuIAoKYGBge3IgZmlnLmhlaWdodD0xLCBmaWcud2lkdGg9Mn0KbGlicmFyeShkYWdpdHR5KQoKZyA8LSBkYWdpdHR5KCdkYWcgewogICAgSCBbcG9zPSIwLDEiXQogICAgVyBbcG9zPSIxLDEiXQogICAgCiAgICBIIC0+IFcKfScpCnBsb3QoZykKYGBgCgojIyMgMy4gU3RhdGlzdGljYWwgbW9kZWwKCiMjIyMgR2VuZXJhdGl2ZSBNb2RlbAoKTGV0J3MgY29uc2lkZXIgdGhlIEdlbmVyYXRpdmUgTW9kZWwgKEggLT4gVykgZnJvbSB0aGUgbGVjdHVyZToKCiRXX2kgXHNpbSBOb3JtYWwoXG11X2ksXHNpZ21hKSQ8YnI+CiRcbXVfaSA9IFxhbHBoYSArIFxiZXRhIEhfaSQ8YnI+CgpMZXQncyBub3cgY29uZHVjdCBhIHByaW9yIHByZWRpY3RpdmUgc2ltdWxhdGlvbiB0byBzaW11bGF0ZSAic3ludGhldGljIGluZGl2aWR1YWxzIi4KCmBgYHtyfQpzZXQuc2VlZCgxNykKIyBmb3J3YXJkIHNpbXVsYXRpb24gYXMgd2UgY2hvb3NlIHRoZXNlIHBhcmFtZXRlcnMKYWxwaGEgPC0gMCAjIGltcGxpZXMgemVybyBoZWlnaHQgPT4gemVybyB3ZWlnaHQKYmV0YSA8LSAwLjUgCnNpZ21hIDwtIDUKbl9pbmRpdmlkdWFscyA8LSAxMDAKCkggPC0gcnVuaWYobl9pbmRpdmlkdWFscywxMzAsMTcwKSAjIGhlaWdodHMsIHVuaWZvcm0gYmV0d2VlbiAxMzAgLSAxNzAgY20KbXUgPC0gYWxwaGEgKyBiZXRhKkgKVyA8LSBybm9ybShuX2luZGl2aWR1YWxzLG11LHNpZ21hKSAjIHNhbXBsZSBmcm9tIE5vcm1hbApgYGAKCmBgYHtyfQpjb2wyIDwtIGNvbC5hbHBoYSgyLDAuOCkKcGxvdChXLEgsICBjb2w9Y29sMiwgbHdkPTMsCiAgICAgY2V4PTEuMiwgIHhsYWIgPSAid2VpZ2h0IChrZykiLCB5bGFiID0gImhlaWdodCAoY20pIikKbXRleHQoICIxMDAgU2ltdWxhdGVkIHBlb3BsZSIgKQpgYGAKCiMjIyMgU2FtcGxpbmcgdGhlIHByaW9yIGRpc3RyaWJ1dGlvbgoKYGBge3J9Cm5fc2FtcGxlcyA8LSAxMAoKYWxwaGEgPC0gcm5vcm0obl9zYW1wbGVzLDAsMSkKYmV0YSA8LSBybm9ybShuX3NhbXBsZXMsMCwxKQoKcGxvdChOVUxMLHhsaW09YygtMiwyKSx5bGltPWMoLTIsMikseGxhYj0ieCIseWxhYj0ieSIpCmZvciAoaSBpbiAxOm5fc2FtcGxlcyl7CiAgYWJsaW5lKGFscGhhW2ldLGJldGFbaV0sbHdkPTQsY29sPTIpCn0KYGBgCk91ciBsaW5lcyBhcmUgc2ltaWxhciBidXQgc2xpZ2h0bHkgZGlmZmVyZW50IHRoYW4gd2hhdCB3YXMgc2hvd24KCiMjIyAzLiBTdGF0aXN0aWNhbCBtb2RlbCBmb3IgSC0+VwoKYGBge3J9Cm4gPC0gMTAKYWxwaGEgPC0gcm5vcm0obiw2MCwxMCkKYmV0YSA8LSBybm9ybShuLDAsMTApCgpIYmFyIDwtIDE1MApIc2VxIDwtIHNlcShmcm9tPTEzMCx0bz0xNzAsbGVuPTMwKQpwbG90KE5VTEwsIHhsaW09YygxMzAsMTcwKSwgeWxpbT1jKDEwLDEwMCksCiAgICAgeGxhYj0iaGVpZ2h0IChjbSkiLCB5bGFiPSJ3ZWlnaHQgKGtnKSIpCgpmb3IgKGkgaW4gMTpuKXsKICBsaW5lcyhIc2VxLCBhbHBoYVtpXSArIGJldGFbaV0qKEhzZXEtSGJhciksbHdkPTMsY29sPTIpCn0KYGBgCgpJcyB0aGlzIGEgZ29vZCBwcmlvciB0byBiZSB1c2VkPyBXaHkgb3Igd2h5IG5vdCBhcmUgdGhleSBpbnRlcnByZXRhYmxlPwoKUmVtZW1iZXIsIGEgbG9nbm9ybWFsIGRpc3RyaWJ1dGlvbiBpcyBhIGRpc3RyaWJ1dGlvbiB0aGF0IGlmIHlvdSB0YWtlIHRoZSBsb2dhcml0aG0gb2YgdGhlIHZhbHVlcywgdGhlbiBhbGwgb2YgaXQncyB2YWx1ZXMgd291bGQgYmUgbm9ybWFsLiAKCmBgYHtyfQojIHNpbXVsYXRlZCBsb2dub3JtYWwKYiA8LSBybG5vcm0oMWU0LCAwLCAxKSAjNC40MApkZW5zKGIsIHhsaW09YygwLDUpLCBhZGo9MC4xKQpgYGAKCkxldCdzIGRvIGEgcHJlZGljdGl2ZSBzaW11bGF0aW9uIG5vdyB1c2luZyB0aGUgTG9nLU5vcm1hbCBwcmlvci4KCmBgYHtyfQpzZXQuc2VlZCgyOTcxKQpuIDwtIDEwCmEgPC0gcm5vcm0oIG4gLCA2MCAsIDUgKQpiIDwtIHJsbm9ybSggbiAsIDAgLCAxICkKCnBsb3QoTlVMTCwgeWxpbT1jKDEzMCwxNzApLCB4bGltPWMoMTAsMTAwKSwKICAgICB5bGFiPSJoZWlnaHQgKGNtKSIsIHhsYWI9IndlaWdodCAoa2cpIikKCmZvciAoaSBpbiAxOm4pewogIGxpbmVzKGFbaV0gKyBiW2ldKihIc2VxLUhiYXIpLEhzZXEsIGx3ZD0zLGNvbD0yKQp9CmBgYAp7eyUgY2FsbG91dCBub3RlICV9fQoKS2V5IGlzIGp1c3RpZnkgcHJpb3JzIHdpdGggaW5mb3JtYXRpb24gb3V0c2lkZSBvZiB0aGUgZGF0YSAodGhhdCB3aWxsIGJlIG1vZGVsZWQpLiBUaGlzIGlzIHNpbWlsYXIgdG8gbWFjaGluZSBsZWFybmluZyB3aGVyZSB3ZSBkb24ndCB3YW50IHRvIGluY2x1ZGUgcmVjb3JkcyBpbiBvdXIgdGVzdCBkYXRhc2V0IHRoYXQgd2VyZSBhbHNvIGluIG91ciB0cmFpbmluZy4gVXNpbmcgbW9kZWxlZCBkYXRhIHRvIGZvcm0gcHJpb3JzIGNhbiBiZSB0aG91Z2h0IG9mIGFzICJwcmlvci1oYWNraW5nIi4gVHlwaWNhbGx5IGluIGxpdGVyYXR1cmUsIEJheWVzaWFuIGFwcHJvYWNoZXMgcmVxdWlyZSBwcmUtcmVnaXN0cmF0aW9uIHdoZW4gdXNpbmcgaW5mb3JtYXRpdmUgcHJpb3JzIChzZWUgW3RoaXMgZXhhbXBsZSBmcm9tIEZlcm5hbmRlcyBldCBhbC4sIDIwMThdKGh0dHBzOi8vZ2l0aHViLmNvbS9taWNoYWVsLWZlcm5hbmRlcy91bmNlcnRhaW50eS1kaXNwbGF5cy1mb3ItdHJhbnNpdC9ibG9iL21hc3Rlci9wcmUtcmVnaXN0cmF0aW9uLnBkZikpLgoKe3slIC9jYWxsb3V0ICV9fQoKJFdfaSBcc2ltIE5vcm1hbChcbXVfaSxcc2lnbWEpJDxicj4KJFxtdV9pID0gXGFscGhhICsgXGJldGEoSF9pIC0gXG92ZXJsaW5le0h9KSQ8YnI+CiRcYWxwaGEgXHNpbSBOb3JtYWwoNjAsMTApJDxicj4KJFxiZXRhIFxzaW0gTG9nTm9ybWFsKDAsMSkkPGJyPgokXHNpZ21hIFxzaW0gVW5pZm9ybSgwLDEwKSQ8YnI+CgpgYGB7cn0KIyBkZWZpbmUgdGhlIGF2ZXJhZ2Ugd2VpZ2h0LCB4LWJhcgp4YmFyIDwtIG1lYW4oZCR3ZWlnaHQpCgojIGZpdCBtb2RlbAptNC4zIDwtIHF1YXAoCiAgICBhbGlzdCgKICAgICAgICBoZWlnaHQgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgICAgICBtdSA8LSBhICsgYiooIHdlaWdodCAtIHhiYXIgKSAsCiAgICAgICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgICAgICBiIH4gZGxub3JtKCAwICwgMSApICwKICAgICAgICBzaWdtYSB+IGR1bmlmKCAwICwgNTAgKQogICAgKSAsIGRhdGE9ZCApCgojIyBSIGNvZGUgNC40NApwcmVjaXMoIG00LjMgKQpgYGAKClRoZSBmaXJzdCByb3cgZ2l2ZXMgdGhlIHF1YWRyYXRpYyBhcHByb3hpbWF0aW9uIGZvciDOsSwgdGhlIHNlY29uZCB0aGUgYXBwcm94aW1hdGlvbiBmb3IgzrIsIGFuZCB0aGUgdGhpcmQgYXBwcm94aW1hdGlvbiBmb3Igz4MuCgpMZXTigJlzIGZvY3VzIG9uIGIgKM6yKS4gU2luY2UgzrIgaXMgYSBzbG9wZSwgdGhlIHZhbHVlIDAuOTAgY2FuIGJlIHJlYWQgYXMgYSAqKnBlcnNvbiAxIGtnIGhlYXZpZXIgaXMgZXhwZWN0ZWQgdG8gYmUgMC45MCBjbSB0YWxsZXIuKiogODklIG9mIHRoZSBwb3N0ZXJpb3IgcHJvYmFiaWxpdHkgbGllcyBiZXR3ZWVuIDAuODQgYW5kIDAuOTcuIFRoYXQgc3VnZ2VzdHMgdGhhdCAqKs6yIHZhbHVlcyBjbG9zZSB0byB6ZXJvIG9yIGdyZWF0bHkgYWJvdmUgb25lIGFyZSBoaWdobHkgaW5jb21wYXRpYmxlIHdpdGggdGhlc2UgZGF0YSBhbmQgdGhpcyBtb2RlbC4qKiBJdCBpcyBtb3N0IGNlcnRhaW5seSBub3QgZXZpZGVuY2UgdGhhdCB0aGUgcmVsYXRpb25zaGlwIGJldHdlZW4gd2VpZ2h0IGFuZCBoZWlnaHQgaXMgbGluZWFyLCBiZWNhdXNlIHRoZSBtb2RlbCBvbmx5IGNvbnNpZGVyZWQgbGluZXMuIEl0IGp1c3Qgc2F5cyB0aGF0LCBpZiB5b3UgYXJlIGNvbW1pdHRlZCB0byBhIGxpbmUsIHRoZW4gbGluZXMgd2l0aCBhIHNsb3BlIGFyb3VuZCAwLjkgYXJlIHBsYXVzaWJsZSBvbmVzLgoKYGBge3J9CiMjIFIgY29kZSA0LjQ1CnJvdW5kKCB2Y292KCBtNC4zICkgLCAzICkKYGBgCgpgYGB7cn0KIyBzaG93cyBib3RoIHRoZSBtYXJnaW5hbCBwb3N0ZXJpb3JzIGFuZCB0aGUgY292YXJpYW5jZS4KcGFpcnMobTQuMykKYGBgCgpUaGVyZSBpcyBsaXR0bGUgY292YXJpYXRpb24gYW1vbmcgdGhlIHBhcmFtZXRlcnMgaW4gdGhpcyBjYXNlLiBUaGUgbGFjayBvZiBjb3ZhcmlhbmNlIGFtb25nIHRoZSBwYXJhbWV0ZXJzICoqcmVzdWx0cyBmcm9tIGNlbnRlcmluZy4qKgoKQXMgYW4gZXhlcmNpc2UsIGNvbnNpZGVyIHJlcnVubmluZyB0aGUgcmVncmVzc2lvbiBhYm92ZSB3aXRob3V0IGNlbnRlcmluZyBhbmQgY29tcGFyZSB0aGUgY292YXJpYXRpb24uCgojIyMgNC4gVmFsaWRhdGUgbW9kZWwKCldlJ2xsIHVzZSBhICoqc2ltdWxhdGlvbi1iYXNlZCoqIHZhbGlkYXRpb24uCgpXZSdsbCBmaXJzdCB2YWxpZGF0ZSB3aXRoIGEgc2ltdWxhdGlvbiAoYWthIGZha2UgZGF0YSkuCgpgYGB7cn0KYWxwaGEgPC0gNzAKYmV0YSA8LSAwLjUKc2lnbWEgPC0gNQpuX2luZGl2aWR1YWxzIDwtIDEwMAoKIyBzaW11bGF0aW9uCkggPC0gcnVuaWYobl9pbmRpdmlkdWFscywxMzAsMTcwKQptdSA8LSBhbHBoYSArIGJldGEqKEgtbWVhbihIKSkKVyA8LSBybm9ybShuX2luZGl2aWR1YWxzLG11LHNpZ21hKQoKZGF0IDwtIGxpc3QoSD1ILFc9VyxIYmFyPW1lYW4oSCkpCgptX3ZhbGlkYXRlIDwtIHF1YXAoCiAgICBhbGlzdCgKICAgICAgICBXIH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICAgICAgbXUgPC0gYSArIGIqKCBIIC0gSGJhciApLAogICAgICAgIGEgfiBkbm9ybSggNjAgLCAxMCApICwKICAgICAgICBiIH4gZGxub3JtKCAwICwgMSApICwKICAgICAgICBzaWdtYSB+IGR1bmlmKCAwICwgMTAgKQogICAgKSAsIGRhdGE9ZGF0ICkKCnByZWNpcyhtX3ZhbGlkYXRlKQpgYGAKCmBgYHtyfQpkYXQgPC0gbGlzdChXID0gZCR3ZWlnaHQsIEggPSBkJGhlaWdodCwgSGJhciA9IG1lYW4oZCRoZWlnaHQpKQoKbV9hZHVsdHMgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgICAgICBtdSA8LSBhICsgYiooIEggLSBIYmFyICksCiAgICAgICAgYSB+IGRub3JtKCA2MCAsIDEwICkgLAogICAgICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgICAgIHNpZ21hIH4gZHVuaWYoIDAgLCAxMCApCiAgICApICwgZGF0YT1kYXQgKQoKcHJlY2lzKG1fYWR1bHRzKQpgYGAKCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpGaXJzdCBMYXcgb2YgU3RhdGlzdGljYWwgSW50ZXJwcmV0YXRpb246IFRoZSAqKnBhcmFtZXRlcnMgYXJlIG5vdCBpbmRlcGVuZGVudCoqIG9mIG9uZSBhbm90aGVyIGFuZCBjYW5ub3QgYWx3YXlzIGJlIGluZGVwZW5kZW50bHkgaW50ZXJwcmV0ZWQuCgpJbnN0ZWFkLCBkcmF3IChwdXNoIG91dCkgKipwb3N0ZXJpb3IgcHJlZGljdGlvbnMqKiBhbmQgZGVzY3JpYmUvaW50ZXJwcmV0IHRoZW0uCgp7eyUgL2NhbGxvdXQgJX19CgpgYGB7cn0KcG9zdCA8LSBleHRyYWN0LnNhbXBsZXMobV9hZHVsdHMpCmhlYWQocG9zdCkKYGBgCgojIyMjIDEuIFBsb3QgdGhlIHNhbXBsZQoKYGBge3J9CiMgNC40LjMKY29sMiA8LSBjb2wuYWxwaGEoMiwwLjgpCnBsb3QoZCRoZWlnaHQsIGQkd2VpZ2h0LCBjb2w9Y29sMiwgbHdkPTMsCiAgICAgY2V4PTEuMiwgeGxhYj0iaGVpZ2h0IChjbSkiLCB5bGFiPSJ3ZWlnaHQgKGtnKSIpCmBgYAoKIyMjIyAyLiBQbG90IHRoZSBwb3N0ZXJpb3IgbWVhbgoKCmBgYHtyfQojIGdldCBwb3N0ZXJpb3IgbWVhbiB2aWEgbGluayBmdW5jdGlvbgp4c2VxIDwtIHNlcShmcm9tPTEzMCx0bz0xOTAsbGVuPTUwKQptdSA8LSBsaW5rKG1fYWR1bHRzLGRhdGE9bGlzdCggSD14c2VxLEhiYXI9bWVhbihkJGhlaWdodCkpKQptdS5tZWFuIDwtIGFwcGx5KCBtdSAsIDIgLCBtZWFuICkKCiMgcGxvdCBzYW1lIHdpdGggbGluZXMgZm9yIG11Lm1lYW4KcGxvdChkJGhlaWdodCwgZCR3ZWlnaHQsIGNvbD1jb2wyLCBsd2Q9MywKICAgICBjZXg9MS4yLCB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKbGluZXMoeHNlcSwgbXUubWVhbiwgbHdkPTQpCmBgYAoKIyMjIyAzLiBQbG90IHVuY2VydGFpbnR5IG9mIHRoZSBtZWFuCgpgYGB7cn0KIyBnZXQgUEkgZm9yIG11Cm11LlBJIDwtIGFwcGx5KCBtdSAsIDIgLCBQSSAsIHByb2I9MC44OSApCgojIHJlcGxvdCBzYW1lIGFzIDIKcGxvdChkJGhlaWdodCwgZCR3ZWlnaHQsIGNvbD1jb2wyICwgbHdkPTMsCiAgICAgY2V4PTEuMiwgeGxhYj0iaGVpZ2h0IChjbSkiLCB5bGFiPSJ3ZWlnaHQgKGtnKSIpCmxpbmVzKCB4c2VxICwgbXUubWVhbiApCiMgYWRkIHBsb3QgYSBzaGFkZWQgcmVnaW9uIGZvciA4OSUgUEkKc2hhZGUoIG11LlBJICwgeHNlcSApCgojYWx0ZXJuYXRpdmUgd2F5IHRvIHBsb3QgdW5jZXJ0YWludHkgb2YgdGhlIG1lYW4KI2ZvciAoIGkgaW4gMToxMDAgKQojICAgIGxpbmVzKCB4c2VxICwgbXVbaSxdICwgcGNoPTE2LCBjb2w9Y29sLmFscGhhKHJhbmdpMiwwLjEpICkKYGBgCgojIyMjIDQuIFBsb3QgdW5jZXJ0YWludHkgb2YgcHJlZGljdGlvbnMKCmBgYHtyfQojIHNpbXVsYXRlIGh5cG90aGV0aWNhbCBhZGRqdXN0cyBnaXZlbiB4c2VxCnNpbS5oZWlnaHQgPC0gc2ltKCBtX2FkdWx0cyAsIGRhdGE9bGlzdChIPXhzZXEsSGJhcj1tZWFuKGQkaGVpZ2h0KSkpIApoZWlnaHQuUEkgPC0gYXBwbHkoIHNpbS5oZWlnaHQgLCAyICwgUEkgLCBwcm9iPTAuODkgKQoKIyByZXBsb3QgMwpwbG90KCBkJGhlaWdodCxkJHdlaWdodCwgY29sPWNvbDIgLCBsd2Q9MywKICAgICBjZXg9MS4yLCB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKbGluZXMoIHhzZXEsIG11Lm1lYW4pCnNoYWRlKCBtdS5QSSAsIHhzZXEgKQoKIyBhZGQgaW4gUEkgcmVnaW9uIGZvciBzaW11bGF0ZWQgaGVpZ2h0cwpzaGFkZSggaGVpZ2h0LlBJICwgeHNlcSApCmBgYAoKIyMjIEV4YW1wbGUKCkxldCdzIHNheSB3ZSB3YW50IHRvIHNpbXVsYXRlIHVzaW5nIHRoZSBgbV9hZHVsdHNgIG1vZGVsIGZvdXIgaW5kaXZpZHVhbHMsIGVhY2ggd2l0aCBoZWlnaHRzIDE0MCwgMTUwLCAxNzAsIGFuZCAxOTAuCgpDYWxjdWxhdGUgdGhlIHNpbXVsYXRlZCBtZWFuIHdlaWdodHMgYW5kIDg5JSBwZXJjZW50aWxlIGludGVydmFscyBmb3IgdGhlc2UgZm91ciBpbmRpdmlkdWFscy4KCmBgYHtyfQpzZXQuc2VlZCgxMDApCnNhbXBsZV9oZWlnaHRzID0gYygxMzUsMTUwLDE3MCwxOTApCgpzaW11bF93ZWlnaHRzIDwtIHNpbSggbV9hZHVsdHMgLCBkYXRhPWxpc3QoSD1zYW1wbGVfaGVpZ2h0cyxIYmFyPW1lYW4oZCRoZWlnaHQpKSkgCgojIHNpbXVsYXRlZCBtZWFucwptZWFuX3dlaWdodHMgPSBhcHBseSggc2ltdWxfd2VpZ2h0cyAsIDIgLCBtZWFuICkKbWVhbl93ZWlnaHRzCmBgYAoKYGBge3J9CiMgc2ltdWxhdGVkIFBJJ3MKcGlfd2VpZ2h0cyA9IGFwcGx5KCBzaW11bF93ZWlnaHRzICwgMiAsIFBJICwgcHJvYj0wLjg5ICkKcGlfd2VpZ2h0cwpgYGAKCmBgYHtyfQpkYXRhLmZyYW1lKAogIHNhbXBsZV9oZWlnaHRzID0gc2FtcGxlX2hlaWdodHMsCiAgc2ltdWxhdGVkX21lYW4gPSBtZWFuX3dlaWdodHMsCiAgbG93X3BpID0gcGlfd2VpZ2h0c1sxLF0sCiAgaGlnaF9waSA9IHBpX3dlaWdodHNbMixdCikKYGBgCgoKIyMgUGFja2FnZSB2ZXJzaW9ucwoKYGBge3J9CnNlc3Npb25JbmZvKCkKYGBg" download="03-class.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010_examples" target="_blank"><img src="https://gitpod.io/button/open-in-gitpod.svg" style="display: block; margin: auto auto auto 0;" /></a>

## Introduction

For this class, we’ll review code examples found in Chapter 4.

This assumes that you have already installed the `rethinking` package.

If you need help, be sure to remember the references in the [Resources](/resource/):

-   [Installing R/RStudio](/resource/install/)
-   [Installing `rethinking` package](/resource/install-rethinking/)
-   [Rmarkdown](/resource/rmarkdown/)
-   [R Style guide](/resource/style/)

## Chapter 4

### Drawing the Owl

![](../../img/syllabus/owl.png)

### 1. Question or estimand

Objective: Describe the association between Adult **weight** and **height**

``` r
library(rethinking)
data(Howell1)
d <- Howell1[Howell1$age>=18,]

plot(d$height, d$weight, col = 2,  xlab = "height (cm)", ylab = "weight (kg)",  lwd=3)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-3-1.png" width="672" />

### 2. Scientific model

Weight is a function of height.

``` r
library(dagitty)

g <- dagitty('dag {
    H [pos="0,1"]
    W [pos="1,1"]
    
    H -> W
}')
plot(g)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-4-1.png" width="192" />

### 3. Statistical model

#### Generative Model

Let’s consider the Generative Model (H -&gt; W) from the lecture:

`\(W_i \sim Normal(\mu_i,\sigma)\)`<br>
`\(\mu_i = \alpha + \beta H_i\)`<br>

Let’s now conduct a prior predictive simulation to simulate “synthetic individuals.”

``` r
set.seed(17)
# forward simulation as we choose these parameters
alpha <- 0 # implies zero height => zero weight
beta <- 0.5 
sigma <- 5
n_individuals <- 100

H <- runif(n_individuals,130,170) # heights, uniform between 130 - 170 cm
mu <- alpha + beta*H
W <- rnorm(n_individuals,mu,sigma) # sample from Normal
```

``` r
col2 <- col.alpha(2,0.8)
plot(W,H,  col=col2, lwd=3,
     cex=1.2,  xlab = "weight (kg)", ylab = "height (cm)")
mtext( "100 Simulated people" )
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-6-1.png" width="672" />

#### Sampling the prior distribution

``` r
n_samples <- 10

alpha <- rnorm(n_samples,0,1)
beta <- rnorm(n_samples,0,1)

plot(NULL,xlim=c(-2,2),ylim=c(-2,2),xlab="x",ylab="y")
for (i in 1:n_samples){
  abline(alpha[i],beta[i],lwd=4,col=2)
}
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### 3. Statistical model for H-&gt;W

``` r
n <- 10
alpha <- rnorm(n,60,10)
beta <- rnorm(n,0,10)

Hbar <- 150
Hseq <- seq(from=130,to=170,len=30)
plot(NULL, xlim=c(130,170), ylim=c(10,100),
     xlab="height (cm)", ylab="weight (kg)")

for (i in 1:n){
  lines(Hseq, alpha[i] + beta[i]*(Hseq-Hbar),lwd=3,col=2)
}
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Is this a good prior to be used? Why or why not are they interpretable?

Remember, a lognormal distribution is a distribution that if you take the logarithm of the values, then all of it’s values would be normal.

``` r
# simulated lognormal
b <- rlnorm(1e4, 0, 1) #4.40
dens(b, xlim=c(0,5), adj=0.1)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Let’s do a predictive simulation now using the Log-Normal prior.

``` r
set.seed(2971)
n <- 10
a <- rnorm( n , 60 , 5 )
b <- rlnorm( n , 0 , 1 )

plot(NULL, ylim=c(130,170), xlim=c(10,100),
     ylab="height (cm)", xlab="weight (kg)")

for (i in 1:n){
  lines(a[i] + b[i]*(Hseq-Hbar),Hseq, lwd=3,col=2)
}
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-10-1.png" width="672" />
{{% callout note %}}

Key is justify priors with information outside of the data (that will be modeled). This is similar to machine learning where we don’t want to include records in our test dataset that were also in our training. Using modeled data to form priors can be thought of as “prior-hacking.” Typically in literature, Bayesian approaches require pre-registration when using informative priors (see [this example from Fernandes et al., 2018](https://github.com/michael-fernandes/uncertainty-displays-for-transit/blob/master/pre-registration.pdf)).

{{% /callout %}}

`\(W_i \sim Normal(\mu_i,\sigma)\)`<br>
`\(\mu_i = \alpha + \beta(H_i - \overline{H})\)`<br>
`\(\alpha \sim Normal(178,20)\)`<br>
`\(\beta \sim LogNormal(0,1)\)`<br>
`\(\sigma \sim Uniform(0,50)\)`<br>

``` r
# define the average weight, x-bar
xbar <- mean(d$weight)

# fit model
m4.3 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d )

## R code 4.44
precis( m4.3 )
```

``` language-r
##              mean         sd        5.5%       94.5%
## a     154.6013728 0.27030783 154.1693687 155.0333770
## b       0.9032803 0.04192366   0.8362782   0.9702824
## sigma   5.0718842 0.19115509   4.7663814   5.3773869
```

The first row gives the quadratic approximation for α, the second the approximation for β, and the third approximation for σ.

Let’s focus on b (β). Since β is a slope, the value 0.90 can be read as a **person 1 kg heavier is expected to be 0.90 cm taller.** 89% of the posterior probability lies between 0.84 and 0.97. That suggests that **β values close to zero or greatly above one are highly incompatible with these data and this model.** It is most certainly not evidence that the relationship between weight and height is linear, because the model only considered lines. It just says that, if you are committed to a line, then lines with a slope around 0.9 are plausible ones.

``` r
## R code 4.45
round( vcov( m4.3 ) , 3 )
```

``` language-r
##           a     b sigma
## a     0.073 0.000 0.000
## b     0.000 0.002 0.000
## sigma 0.000 0.000 0.037
```

``` r
# shows both the marginal posteriors and the covariance.
pairs(m4.3)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-13-1.png" width="672" />

There is little covariation among the parameters in this case. The lack of covariance among the parameters **results from centering.**

As an exercise, consider rerunning the regression above without centering and compare the covariation.

### 4. Validate model

We’ll use a **simulation-based** validation.

We’ll first validate with a simulation (aka fake data).

`\(W_i \sim Normal(\mu_i,\sigma)\)`<br>
`\(\mu_i = \alpha + \beta(H_i - \overline{H})\)`<br>
`\(\alpha \sim Normal(60,10)\)`<br>
`\(\beta \sim LogNormal(0,1)\)`<br>
`\(\sigma \sim Uniform(0,10)\)`<br>

``` r
alpha <- 70
beta <- 0.5
sigma <- 5
n_individuals <- 100

# simulation
H <- runif(n_individuals,130,170)
mu <- alpha + beta*(H-mean(H))
W <- rnorm(n_individuals,mu,sigma)

dat <- list(H=H,W=W,Hbar=mean(H))

m_validate <- quap(
    alist(
        W ~ dnorm( mu , sigma ) ,
        mu <- a + b*( H - Hbar ),
        a ~ dnorm( 60 , 10 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 10 )
    ) , data=dat )

precis(m_validate)
```

``` language-r
##             mean         sd       5.5%      94.5%
## a     69.9483419 0.46670241 69.2024614 70.6942225
## b      0.5139771 0.03867635  0.4521648  0.5757894
## sigma  4.6720149 0.33036867  4.1440220  5.2000079
```

Let’s now run it with real data.

``` r
dat <- list(W = d$weight, H = d$height, Hbar = mean(d$height))

m_adults <- quap(
    alist(
        W ~ dnorm( mu , sigma ) ,
        mu <- a + b*( H - Hbar ),
        a ~ dnorm( 60 , 10 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 10 )
    ) , data=dat )

precis(m_adults)
```

``` language-r
##             mean         sd       5.5%      94.5%
## a     44.9981241 0.22538919 44.6379087 45.3583396
## b      0.6286797 0.02914554  0.5820995  0.6752599
## sigma  4.2297367 0.15941778  3.9749563  4.4845171
```

{{% callout note %}}

First Law of Statistical Interpretation: The **parameters are not independent** of one another and cannot always be independently interpreted.

Instead, draw (push out) **posterior predictions** and describe/interpret them.

{{% /callout %}}

``` r
post <- extract.samples(m_adults)
head(post)
```

``` language-r
##          a         b    sigma
## 1 44.80946 0.6584680 4.121912
## 2 45.56093 0.6486405 4.262163
## 3 45.07283 0.6567565 4.228691
## 4 44.98621 0.5895380 4.215656
## 5 44.99444 0.6983031 3.891291
## 6 45.05029 0.5888397 4.196087
```

#### 1. Plot the sample

``` r
# 4.4.3
col2 <- col.alpha(2,0.8)
plot(d$height, d$weight, col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-17-1.png" width="672" />

#### 2. Plot the posterior mean

``` r
# get posterior mean via link function
xseq <- seq(from=130,to=190,len=50)
mu <- link(m_adults,data=list( H=xseq,Hbar=mean(d$height)))
mu.mean <- apply( mu , 2 , mean )

# plot same with lines for mu.mean
plot(d$height, d$weight, col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
lines(xseq, mu.mean, lwd=4)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-18-1.png" width="672" />

#### 3. Plot uncertainty of the mean

``` r
# get PI for mu
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# replot same as 2
plot(d$height, d$weight, col=col2 , lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
lines( xseq , mu.mean )
# add plot a shaded region for 89% PI
shade( mu.PI , xseq )
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-19-1.png" width="672" />

``` r
#alternative way to plot uncertainty of the mean
#for ( i in 1:100 )
#    lines( xseq , mu[i,] , pch=16, col=col.alpha(rangi2,0.1) )
```

#### 4. Plot uncertainty of predictions

``` r
# simulate hypothetical adjusts given xseq
sim.height <- sim( m_adults , data=list(H=xseq,Hbar=mean(d$height))) 
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# replot 3
plot( d$height,d$weight, col=col2 , lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
lines( xseq, mu.mean)
shade( mu.PI , xseq )

# add in PI region for simulated heights
shade( height.PI , xseq )
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-20-1.png" width="672" />

### Example

Let’s say we want to simulate using the `m_adults` model four individuals, each with heights 140, 150, 170, and 190.

Calculate the simulated mean weights and 89% percentile intervals for these four individuals.

``` r
set.seed(100)
sample_heights = c(135,150,170,190)

simul_weights <- sim( m_adults , data=list(H=sample_heights,Hbar=mean(d$height))) 

# simulated means
mean_weights = apply( simul_weights , 2 , mean )
mean_weights
```

``` language-r
## [1] 32.52395 42.15084 54.84502 67.30787
```

``` r
# simulated PI's
pi_weights = apply( simul_weights , 2 , PI , prob=0.89 )
pi_weights
```

``` language-r
##         [,1]     [,2]     [,3]     [,4]
## 5%  25.86437 35.55815 48.21862 60.84940
## 94% 39.40483 48.65104 61.45551 73.87156
```

``` r
data.frame(
  sample_heights = sample_heights,
  simulated_mean = mean_weights,
  low_pi = pi_weights[1,],
  high_pi = pi_weights[2,]
)
```

``` language-r
##   sample_heights simulated_mean   low_pi  high_pi
## 1            135       32.52395 25.86437 39.40483
## 2            150       42.15084 35.55815 48.65104
## 3            170       54.84502 48.21862 61.45551
## 4            190       67.30787 60.84940 73.87156
```

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
## [1] dagitty_0.3-1        rethinking_2.21      cmdstanr_0.4.0.9001 
## [4] rstan_2.21.3         ggplot2_3.3.5        StanHeaders_2.21.0-7
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
## [49] stringr_1.4.0        V8_3.6.0             munsell_0.5.0       
## [52] callr_3.7.0          compiler_4.1.1       jquerylib_0.1.4     
## [55] rlang_0.4.12         grid_4.1.1           rstudioapi_0.13     
## [58] base64enc_0.1-3      rmarkdown_2.11       boot_1.3-28         
## [61] xaringanExtra_0.5.5  gtable_0.3.0         codetools_0.2-18    
## [64] curl_4.3.2           inline_0.3.19        abind_1.4-5         
## [67] DBI_1.1.1            R6_2.5.1             gridExtra_2.3       
## [70] lubridate_1.8.0      knitr_1.36           dplyr_1.0.7         
## [73] fastmap_1.1.0        utf8_1.2.2           downloadthis_0.2.1  
## [76] bsplus_0.1.3         KernSmooth_2.23-20   shape_1.4.6         
## [79] stringi_1.7.6        Rcpp_1.0.7           vctrs_0.3.8         
## [82] tidyselect_1.1.1     xfun_0.28            coda_0.19-4
```
