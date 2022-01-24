---
date: "2022-01-23"
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
<a href="data:application/octet-stream;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyAzIgptZW51OgogIGV4YW1wbGU6CiAgICBwYXJlbnQ6IEV4YW1wbGVzCndlaWdodDogMwp0b2M6IHRydWUKdHlwZTogZG9jcwotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFLCBmaWcud2lkdGg9NSwgZmlnLmhlaWdodD00fQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIGNsYXNzLnNvdXJjZT0ibGFuZ3VhZ2UtciIsIGNsYXNzLm91dHB1dD0ibGFuZ3VhZ2UtciIsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQp4YXJpbmdhbkV4dHJhOjp1c2VfY2xpcGJvYXJkKCkKbGlicmFyeShyZXRoaW5raW5nKQpgYGAKCgpgYGB7ciBlY2hvPUZBTFNFfQpkb3dubG9hZHRoaXM6OmRvd25sb2FkX2ZpbGUoCiAgcGF0aCA9ICIwMy1jbGFzcy5SbWFya2Rvd24iLAogIG91dHB1dF9uYW1lID0gIjAzLWNsYXNzIiwKICBidXR0b25fbGFiZWwgPSAiRG93bmxvYWQgdGhpcyBjb2RlIiwKICBidXR0b25fdHlwZSA9ICJkYW5nZXIiLAogIGhhc19pY29uID0gVFJVRSwKICBpY29uID0gImZhIGZhLXNhdmUiLAogIHNlbGZfY29udGFpbmVkID0gRkFMU0UKKQpgYGAKCjxhIGhyZWY9Imh0dHBzOi8vZ2l0cG9kLmlvLyNodHRwczovL2dpdGh1Yi5jb20vd2Vzc2xlbi9kc2JhNjAxMC1leGFtcGxlcyI+CjxpbWcgYWxpZ249ImxlZnQiIHNyYz0iaHR0cHM6Ly9naXRwb2QuaW8vYnV0dG9uL29wZW4taW4tZ2l0cG9kLnN2ZyI+CjwvYT4KCiMjIEludHJvZHVjdGlvbgoKRm9yIHRoaXMgY2xhc3MsIHdlJ2xsIHJldmlldyBjb2RlIGV4YW1wbGVzIGZvdW5kIGluIENoYXB0ZXIgNC4KClRoaXMgYXNzdW1lcyB0aGF0IHlvdSBoYXZlIGFscmVhZHkgaW5zdGFsbGVkIHRoZSBgcmV0aGlua2luZ2AgcGFja2FnZS4KCklmIHlvdSBuZWVkIGhlbHAsIGJlIHN1cmUgdG8gcmVtZW1iZXIgdGhlIHJlZmVyZW5jZXMgaW4gdGhlIFtSZXNvdXJjZXNdKC9yZXNvdXJjZS8pOgoKKiBbSW5zdGFsbGluZyBSL1JTdHVkaW9dKC9yZXNvdXJjZS9pbnN0YWxsLykKKiBbSW5zdGFsbGluZyBgcmV0aGlua2luZ2AgcGFja2FnZV0oL3Jlc291cmNlL2luc3RhbGwtcmV0aGlua2luZy8pCiogW1JtYXJrZG93bl0oL3Jlc291cmNlL3JtYXJrZG93bi8pCiogW1IgU3R5bGUgZ3VpZGVdKC9yZXNvdXJjZS9zdHlsZS8pCgojIyBDaGFwdGVyIDQgCgojIyMgRHJhd2luZyB0aGUgT3dsCgohW10oLi4vLi4vaW1nL3N5bGxhYnVzL293bC5wbmcpCgoKIyMjIDEuIFF1ZXN0aW9uIG9yIGVzdGltYW5kCgpPYmplY3RpdmU6IERlc2NyaWJlIHRoZSBhc3NvY2lhdGlvbiBiZXR3ZWVuIEFkdWx0ICoqd2VpZ2h0KiogYW5kICoqaGVpZ2h0KioKCmBgYHtyfQpsaWJyYXJ5KHJldGhpbmtpbmcpCmRhdGEoSG93ZWxsMSkKZCA8LSBIb3dlbGwxW0hvd2VsbDEkYWdlPj0xOCxdCgpwbG90KGQkaGVpZ2h0LGQkd2VpZ2h0LCBjb2wgPSAyLCB4bGFiID0gImhlaWdodCAoY20pIiwgeWxhYiA9ICJ3ZWlnaHQgKGtnKSIsIGx3ZD0zKQpgYGAKCkFsdGVybmF0aXZlbHksIHVzaW5nIGBnZ3Bsb3QyYCB3ZSBjYW4gcGxvdCB0aGUgc2FtZToKCmBgYHtyfQpsaWJyYXJ5KGdncGxvdDIpCgpnZ3Bsb3QoZCwgYWVzKHggPSBoZWlnaHQsIHkgPSB3ZWlnaHQpKSArCiAgZ2VvbV9wb2ludChjb2xvciA9ICJyZWQiKSArCiAgbGFicyh4ID0gImhlaWdodCAoY20pIiwgeSA9ICJ3ZWlnaHQgKGtnKSIpICsKICB0aGVtZV9idygpCmBgYAoKCiMjIyAyLiBTY2llbnRpZmljIG1vZGVsCgpXZWlnaHQgaXMgYSBmdW5jdGlvbiBvZiBoZWlnaHQuIAoKYGBge3IgZmlnLmhlaWdodD0xLCBmaWcud2lkdGg9Mn0KbGlicmFyeShkYWdpdHR5KQoKZyA8LSBkYWdpdHR5KCdkYWcgewogICAgVyBbcG9zPSIwLDEiXQogICAgSCBbcG9zPSIxLDEiXQogICAgCiAgICBXIC0+IEgKfScpCnBsb3QoZykKYGBgCgojIyMgMy4gU3RhdGlzdGljYWwgbW9kZWwKCiMjIyMgR2VuZXJhdGl2ZSBNb2RlbAoKTGV0J3MgY29uc2lkZXIgdGhlIEdlbmVyYXRpdmUgTW9kZWwgKEggLT4gVykgZnJvbSB0aGUgbGVjdHVyZToKCiRXX2kgXHNpbSBOb3JtYWwoXG11X2ksXHNpZ21hKSQ8YnI+CiRcbXVfaSA9IFxhbHBoYSArIFxiZXRhIEhfaSQ8YnI+CgpgYGB7cn0KIyBmb3J3YXJkIHNpbXVsYXRpb24gYXMgd2UgY2hvb3NlIHRoZXNlIHBhcmFtZXRlcnMKYWxwaGEgPC0gMCAjIGltcGxpZXMgemVybyBoZWlnaHQgPT4gemVybyB3ZWlnaHQKYmV0YSA8LSAwLjUgCnNpZ21hIDwtIDUKbl9pbmRpdmlkdWFscyA8LSAxMDAKCkggPC0gcnVuaWYobl9pbmRpdmlkdWFscywxMzAsMTcwKSAjIGhlaWdodHMsIHVuaWZvcm0gYmV0d2VlbiAxMzAgLSAxNzAgY20KCm11IDwtIGFscGhhICsgYmV0YSpIClcgPC0gcm5vcm0obl9pbmRpdmlkdWFscyxtdSxzaWdtYSkgIyBzYW1wbGUgZnJvbSBOb3JtYWwKYGBgCgpgYGB7cn0KY29sMiA8LSBjb2wuYWxwaGEoMiwwLjgpCnBsb3QoSCxXLCAgY29sPWNvbDIsIGx3ZD0zLAogICAgIGNleD0xLjIsIHhsYWIgPSAiaGVpZ2h0IChjbSkiLCB5bGFiID0gIndlaWdodCAoa2cpIikKbXRleHQoICIxMDAgU2ltdWxhdGVkIHBlb3BsZSIgKQpgYGAKYGBge3J9CiMgZ2dwbG90MiB2ZXJzaW9uCmRmIDwtIGRhdGEuZnJhbWUoCiAgaGVpZ2h0ID0gSCwKICB3ZWlnaHQgPSBXCikKCmdncGxvdChkZiwgYWVzKHggPSBoZWlnaHQsIHkgPSB3ZWlnaHQpKSArCiAgZ2VvbV9wb2ludChjb2xvcj0icmVkIikgKwogIGxhYnMoeCA9ICJoZWlnaHQgKGNtKSIsIHkgPSAid2VpZ2h0IChrZykiLCB0aXRsZSA9ICIxMDAgU2ltdWxhdGVkIHBlb3BsZSIpCmBgYAoKIyMjIyBTYW1wbGluZyB0aGUgcHJpb3IgZGlzdHJpYnV0aW9uCgpgYGB7cn0Kbl9zYW1wbGVzIDwtIDEwCgphbHBoYSA8LSBybm9ybShuX3NhbXBsZXMsMCwxKQpiZXRhIDwtIHJub3JtKG5fc2FtcGxlcywwLDEpCgpwbG90KE5VTEwseGxpbT1jKC0yLDIpLHlsaW09YygtMiwyKSx4bGFiPSJ4Iix5bGFiPSJ5IikKZm9yIChpIGluIDE6bl9zYW1wbGVzKXsKICBhYmxpbmUoYWxwaGFbaV0sYmV0YVtpXSxsd2Q9NCxjb2w9MikKfQpgYGAKCmBgYHtyfQpkZjIgPC0gZGF0YS5mcmFtZSgKICBhbHBoYSA9IGFscGhhLAogIGJldGEgPSBiZXRhLAogIHNhbXAgPSAxOm5fc2FtcGxlcwopCgpwIDwtIGdncGxvdChkZjIpICsKICBnZW9tX2FibGluZShhZXMoc2xvcGUgPSBiZXRhLCBpbnRlcmNlcHQgPSBhbHBoYSwgZ3JvdXAgPSBzYW1wKSkgKwogIHhsaW0oLTMsMykgKwogIHlsaW0oLTMsMykKCnAKYGBgCmBgYHtyIGZpZy5oZWlnaHQ9MixmaWcud2lkdGg9Mn0KbGlicmFyeShnZ2FuaW1hdGUpCgphbmltIDwtIHAgKyB0cmFuc2l0aW9uX3N0YXRlcyhzYW1wLCB0cmFuc2l0aW9uX2xlbmd0aD0yLCBzdGF0ZV9sZW5ndGg9MSkgKyAKICBnZ3RpdGxlKCdTYW1wbGUge2Nsb3Nlc3Rfc3RhdGV9IG9mIDEwJykgKwogIGVudGVyX2ZhZGUoKSArCiAgZXhpdF9mYWRlKCkKCmFuaW0KI2FuaW1hdGUoYW5pbSwgaGVpZ2h0PTIsIHdpZHRoPTMpCmBgYAoKIyMjIDMuIFN0YXRpc3RpY2FsIG1vZGVsIGZvciBILT5XCgpgYGB7cn0KbiA8LSAxMAphbHBoYSA8LSBybm9ybShuLDYwLDEwKQpiZXRhIDwtIHJub3JtKG4sMCwxMCkKCkhiYXIgPC0gMTUwCkhzZXEgPC0gc2VxKGZyb209MTMwLHRvPTE3MCxsZW49MzApCnBsb3QoTlVMTCwgeGxpbT1jKDEzMCwxNzApLCB5bGltPWMoMTAsMTAwKSwKICAgICB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKCmZvciAoaSBpbiAxOm4pewogIGxpbmVzKEhzZXEsIGFscGhhW2ldICsgYmV0YVtpXSooSHNlcS1IYmFyKSxsd2Q9Myxjb2w9MikKfQpgYGAKSXMgdGhpcyBhIGdvb2QgcHJpb3IgdG8gYmUgdXNlZD8gV2h5IG9yIHdoeSBub3QgYXJlIHRoZXkgaW50ZXJwcmV0YWJsZT8KClJlbWVtYmVyLCBhIGxvZ25vcm1hbCBkaXN0cmlidXRpb24gaXMgYSBkaXN0cmlidXRpb24gdGhhdCBpZiB5b3UgdGFrZSB0aGUgbG9nYXJpdGhtIG9mIHRoZSB2YWx1ZXMsIHRoZW4gYWxsIG9mIGl0J3MgdmFsdWVzIHdvdWxkIGJlIG5vcm1hbC4gCgpgYGB7cn0KIyBzaW11bGF0ZWQgbG9nbm9ybWFsCmIgPC0gcmxub3JtKDFlNCwgMCwgMSkgIzQuNDAKZGVucyhiLCB4bGltPWMoMCw1KSwgYWRqPTAuMSkKYGBgCgpMZXQncyBkbyBhIHByZWRpY3RpdmUgc2ltdWxhdGlvbiBub3cgdXNpbmcgdGhlIExvZy1Ob3JtYWwgcHJpb3IuCgpgYGB7cn0Kc2V0LnNlZWQoMjk3MSkKTiA8LSAxMDAgIAphIDwtIHJub3JtKCBOICwgNjAgLCAxMCApCmIgPC0gcmxub3JtKCBOICwgMCAsIDEgKQoKcGxvdChOVUxMLCB4bGltPWMoMTMwLDE3MCksIHlsaW09YygxMCwxMDApLAogICAgIHhsYWI9ImhlaWdodCAoY20pIiwgeWxhYj0id2VpZ2h0IChrZykiKQoKZm9yIChpIGluIDE6bil7CiAgbGluZXMoSHNlcSwgYVtpXSArIGJbaV0qKEhzZXEtSGJhciksbHdkPTMsY29sPTIpCn0KYGBgCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpLZXkgaXMganVzdGlmeSBwcmlvcnMgd2l0aCBpbmZvcm1hdGlvbiBvdXRzaWRlIG9mIHRoZSBkYXRhICh0aGF0IHdpbGwgYmUgbW9kZWxlZCkuIFRoaXMgaXMgc2ltaWxhciB0byBtYWNoaW5lIGxlYXJuaW5nIHdoZXJlIHdlIGRvbid0IHdhbnQgdG8gaW5jbHVkZSByZWNvcmRzIGluIG91ciB0ZXN0IGRhdGFzZXQgdGhhdCB3ZXJlIGFsc28gaW4gb3VyIHRyYWluaW5nLiBVc2luZyBtb2RlbGVkIGRhdGEgdG8gZm9ybSBwcmlvcnMgY2FuIGJlIHRob3VnaHQgb2YgYXMgInByaW9yLWhhY2tpbmciLiBUeXBpY2FsbHkgaW4gbGl0ZXJhdHVyZSwgQmF5ZXNpYW4gYXBwcm9hY2hlcyByZXF1aXJlIHByZS1yZWdpc3RyYXRpb24gd2hlbiB1c2luZyBub24taW5mb3JtYXRpdmUgcHJpb3JzIChzZWUgW3RoaXMgZXhhbXBsZSBmcm9tIEZlcm5hbmRlcyBldCBhbC4sIDIwMThdKGh0dHBzOi8vZ2l0aHViLmNvbS9taWNoYWVsLWZlcm5hbmRlcy91bmNlcnRhaW50eS1kaXNwbGF5cy1mb3ItdHJhbnNpdC9ibG9iL21hc3Rlci9wcmUtcmVnaXN0cmF0aW9uLnBkZikpLgoKe3slIC9jYWxsb3V0ICV9fQoKJFdfaSBcc2ltIE5vcm1hbChcbXVfaSxcc2lnbWEpJDxicj4KJFxtdV9pID0gXGFscGhhICsgXGJldGEoSF9pIC0gXG92ZXJsaW5le0h9KSQ8YnI+CiRcYWxwaGEgXHNpbSBOb3JtYWwoNjAsMTApJDxicj4KJFxiZXRhIFxzaW0gTG9nTm9ybWFsKDAsMSkkPGJyPgokXHNpZ21hIFxzaW0gVW5pZm9ybSgwLDEwKSQ8YnI+CgpgYGB7cn0KIyBkZWZpbmUgdGhlIGF2ZXJhZ2Ugd2VpZ2h0LCB4LWJhcgp4YmFyIDwtIG1lYW4oZCR3ZWlnaHQpCgojIGZpdCBtb2RlbAptNC4zIDwtIHF1YXAoCiAgICBhbGlzdCgKICAgICAgICBoZWlnaHQgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgICAgICBtdSA8LSBhICsgYiooIHdlaWdodCAtIHhiYXIgKSAsCiAgICAgICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgICAgICBiIH4gZGxub3JtKCAwICwgMSApICwKICAgICAgICBzaWdtYSB+IGR1bmlmKCAwICwgNTAgKQogICAgKSAsIGRhdGE9ZCApCgojIyBSIGNvZGUgNC40MwptNC4zYiA8LSBxdWFwKAogICAgYWxpc3QoCiAgICAgICAgaGVpZ2h0IH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICAgICAgbXUgPC0gYSArIGV4cChsb2dfYikqKCB3ZWlnaHQgLSB4YmFyICksCiAgICAgICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgICAgICBsb2dfYiB+IGRub3JtKCAwICwgMSApICwKICAgICAgICBzaWdtYSB+IGR1bmlmKCAwICwgNTAgKQogICAgKSAsIGRhdGE9ZCApCgojIyBSIGNvZGUgNC40NApwcmVjaXMoIG00LjMgKQpgYGAKCmBgYHtyfQojIyBSIGNvZGUgNC40NQpyb3VuZCggdmNvdiggbTQuMyApICwgMyApCmBgYAoKIyMjIDQuIFZhbGlkYXRlIG1vZGVsCgpXZSdsbCB1c2UgYSAqKnNpbXVsYXRpb24tYmFzZWQqKiB2YWxpZGF0aW9uLgoKV2UnbGwgZmlyc3QgdmFsaWRhdGUgd2l0aCBhIHNpbXVsYXRpb24gKGFrYSBmYWtlIGRhdGEpLgoKYGBge3J9CmFscGhhIDwtIDcwCmJldGEgPC0gMC41CnNpZ21hIDwtIDUKbl9pbmRpdmlkdWFscyA8LSAxMDAKSCA8LSBydW5pZihuX2luZGl2aWR1YWxzLDEzMCwxNzApCm11IDwtIGFscGhhICsgYmV0YSooSC1tZWFuKEgpKQpXIDwtIHJub3JtKG5faW5kaXZpZHVhbHMsbXUsc2lnbWEpCgpkYXQgPC0gbGlzdChIPUgsVz1XLEhiYXI9bWVhbihIKSkKCm1fdmFsaWRhdGUgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgICAgICBtdSA8LSBhICsgYiooIEggLSBIYmFyICksCiAgICAgICAgYSB+IGRub3JtKCA2MCAsIDEwICkgLAogICAgICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgICAgIHNpZ21hIH4gZHVuaWYoIDAgLCAxMCApCiAgICApICwgZGF0YT1kYXQgKQoKcHJlY2lzKG1fdmFsaWRhdGUpCmBgYAoKYGBge3J9CmRhdCA8LSBsaXN0KFcgPSBkJHdlaWdodCwgSCA9IGQkaGVpZ2h0LCBIYmFyID0gbWVhbihkJGhlaWdodCkpCgptX2FkdWx0cyA8LSBxdWFwKAogICAgYWxpc3QoCiAgICAgICAgVyB+IGRub3JtKCBtdSAsIHNpZ21hICkgLAogICAgICAgIG11IDwtIGEgKyBiKiggSCAtIEhiYXIgKSwKICAgICAgICBhIH4gZG5vcm0oIDYwICwgMTAgKSAsCiAgICAgICAgYiB+IGRsbm9ybSggMCAsIDEgKSAsCiAgICAgICAgc2lnbWEgfiBkdW5pZiggMCAsIDEwICkKICAgICkgLCBkYXRhPWRhdCApCgpwcmVjaXMobV9hZHVsdHMpCmBgYAoKe3slIGNhbGxvdXQgbm90ZSAlfX0KCkZpcnN0IExhdyBvZiBTdGF0aXN0aWNhbCBJbnRlcnByZXRhdGlvbjogVGhlICoqcGFyYW1ldGVycyBhcmUgbm90IGluZGVwZW5kZW50Kiogb2Ygb25lIGFub3RoZXIgYW5kIGNhbm5vdCBhbHdheXMgYmUgaW5kZXBlbmRlbnRseSBpbnRlcnByZXRlZC4KCkluc3RlYWQsIGRyYXcgKHB1c2ggb3V0KSAqKnBvc3RlcmlvciBwcmVkaWN0aW9ucyoqIGFuZCBkZXNjcmliZS9pbnRlcnByZXQgdGhlbS4KCnt7JSAvY2FsbG91dCAlfX0KCmBgYHtyfQpwb3N0IDwtIGV4dHJhY3Quc2FtcGxlcyhtX2FkdWx0cykKaGVhZChwb3N0KQpgYGAKCiMjIyMgMS4gUGxvdCB0aGUgc2FtcGxlCgpgYGB7cn0KIyA0LjQuMwpjb2wyIDwtIGNvbC5hbHBoYSgyLDAuOCkKcGxvdChkJGhlaWdodCwgZCR3ZWlnaHQsIGNvbD1jb2wyLCBsd2Q9MywKICAgICBjZXg9MS4yLCB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKYGBgCgojIyMjIDIuIFBsb3QgdGhlIHBvc3RlcmlvciBtZWFuCgoKYGBge3J9CndlaWdodC5zZXEgPC0gc2VxKGZyb209MjUsdG89NzAsYnk9MSkKbXUgPC0gbGluayhtNC4zLGRhdGE9bGlzdCh3ZWlnaHQ9d2VpZ2h0LnNlcSkpCnBsb3QoZCRoZWlnaHQsZCR3ZWlnaHQsICBjb2w9Y29sMiwgbHdkPTMsCiAgICAgY2V4PTEuMiwgeGxhYj0iaGVpZ2h0IChjbSkiLCB5bGFiPSJ3ZWlnaHQgKGtnKSIpCmxpbmVzKCAgYXBwbHkobXUsMixtZWFuKSwgd2VpZ2h0LnNlcSwgbHdkPTQpCmBgYAoKIyMjIyAzLiBQbG90IHVuY2VydGFpbnR5IG9mIHRoZSBtZWFuCgpgYGB7cn0KeHNlcSA8LSBzZXEoZnJvbT0xMzAsdG89MTkwLGxlbj01MCkKcGxvdChkJGhlaWdodCwgZCR3ZWlnaHQsIGNvbD1jb2wyLCBsd2Q9MywKICAgICBjZXg9MS4yLCB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKbGluZXMoICAgYXBwbHkobXUsMixtZWFuKSwgd2VpZ2h0LnNlcSxsd2Q9NCkKc2hhZGUoIGFwcGx5KG11LDIsUEkscHJvYj0wLjk5KSwgd2VpZ2h0LnNlcSwgY29sPWNvbC5hbHBoYSgyLDAuNSkpCmBgYAoKIyMjIyA0LiBQbG90IHVuY2VydGFpbnR5IG9mIHByZWRpY3Rpb25zCgp7eyUgY2FsbG91dCBub3RlICV9fQoKVHJ5IGFzIFtjaGFsbGVuZ2luZyBleGVyY2lzZSBhcyBzaG93biBpbiBTbGlkZSA3MCBvZiBMZWN0dXJlIDNdKGh0dHBzOi8vc3BlYWtlcmRlY2suY29tL3JtY2VscmVhdGgvc3RhdGlzdGljYWwtcmV0aGlua2luZy0yMDIyLWxlY3R1cmUtMDM/c2xpZGU9NzApLgoKe3slIC9jYWxsb3V0ICV9fQoKIyMgUGFja2FnZSB2ZXJzaW9ucwoKYGBge3J9CnNlc3Npb25JbmZvKCkKYGBg" download="03-class.Rmarkdown">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010-examples">
<img align="left" src="https://gitpod.io/button/open-in-gitpod.svg">
</a>

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

plot(d$height,d$weight, col = 2, xlab = "height (cm)", ylab = "weight (kg)", lwd=3)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Alternatively, using `ggplot2` we can plot the same:

``` r
library(ggplot2)

ggplot(d, aes(x = height, y = weight)) +
  geom_point(color = "red") +
  labs(x = "height (cm)", y = "weight (kg)") +
  theme_bw()
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-3-1.png" width="672" />

### 2. Scientific model

Weight is a function of height.

``` r
library(dagitty)

g <- dagitty('dag {
    W [pos="0,1"]
    H [pos="1,1"]
    
    W -> H
}')
plot(g)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-4-1.png" width="192" />

### 3. Statistical model

#### Generative Model

Let’s consider the Generative Model (H -&gt; W) from the lecture:

`\(W_i \sim Normal(\mu_i,\sigma)\)`<br>
`\(\mu_i = \alpha + \beta H_i\)`<br>

``` r
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
plot(H,W,  col=col2, lwd=3,
     cex=1.2, xlab = "height (cm)", ylab = "weight (kg)")
mtext( "100 Simulated people" )
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-6-1.png" width="672" />

``` r
# ggplot2 version
df <- data.frame(
  height = H,
  weight = W
)

ggplot(df, aes(x = height, y = weight)) +
  geom_point(color="red") +
  labs(x = "height (cm)", y = "weight (kg)", title = "100 Simulated people")
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-7-1.png" width="672" />

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

``` r
df2 <- data.frame(
  alpha = alpha,
  beta = beta,
  samp = 1:n_samples
)

p <- ggplot(df2) +
  geom_abline(aes(slope = beta, intercept = alpha, group = samp)) +
  xlim(-3,3) +
  ylim(-3,3)

p
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-9-1.png" width="672" />

``` r
library(gganimate)

anim <- p + transition_states(samp, transition_length=2, state_length=1) + 
  ggtitle('Sample {closest_state} of 10') +
  enter_fade() +
  exit_fade()

anim
```

![](03-class_files/figure-html/unnamed-chunk-10-1.gif)<!-- -->

``` r
#animate(anim, height=2, width=3)
```

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-11-1.png" width="672" />
Is this a good prior to be used? Why or why not are they interpretable?

Remember, a lognormal distribution is a distribution that if you take the logarithm of the values, then all of it’s values would be normal.

``` r
# simulated lognormal
b <- rlnorm(1e4, 0, 1) #4.40
dens(b, xlim=c(0,5), adj=0.1)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-12-1.png" width="672" />

Let’s do a predictive simulation now using the Log-Normal prior.

``` r
set.seed(2971)
N <- 100  
a <- rnorm( N , 60 , 10 )
b <- rlnorm( N , 0 , 1 )

plot(NULL, xlim=c(130,170), ylim=c(10,100),
     xlab="height (cm)", ylab="weight (kg)")

for (i in 1:n){
  lines(Hseq, a[i] + b[i]*(Hseq-Hbar),lwd=3,col=2)
}
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-13-1.png" width="672" />
{{% callout note %}}

Key is justify priors with information outside of the data (that will be modeled). This is similar to machine learning where we don’t want to include records in our test dataset that were also in our training. Using modeled data to form priors can be thought of as “prior-hacking.” Typically in literature, Bayesian approaches require pre-registration when using non-informative priors (see [this example from Fernandes et al., 2018](https://github.com/michael-fernandes/uncertainty-displays-for-transit/blob/master/pre-registration.pdf)).

{{% /callout %}}

`\(W_i \sim Normal(\mu_i,\sigma)\)`<br>
`\(\mu_i = \alpha + \beta(H_i - \overline{H})\)`<br>
`\(\alpha \sim Normal(60,10)\)`<br>
`\(\beta \sim LogNormal(0,1)\)`<br>
`\(\sigma \sim Uniform(0,10)\)`<br>

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

## R code 4.43
m4.3b <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + exp(log_b)*( weight - xbar ),
        a ~ dnorm( 178 , 20 ) ,
        log_b ~ dnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d )

## R code 4.44
precis( m4.3 )
```

``` language-r
##              mean         sd        5.5%       94.5%
## a     154.6013671 0.27030766 154.1693633 155.0333710
## b       0.9032807 0.04192363   0.8362787   0.9702828
## sigma   5.0718809 0.19115478   4.7663786   5.3773831
```

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

### 4. Validate model

We’ll use a **simulation-based** validation.

We’ll first validate with a simulation (aka fake data).

``` r
alpha <- 70
beta <- 0.5
sigma <- 5
n_individuals <- 100
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
## a     69.0995550 0.54004359 68.2364610 69.9626489
## b      0.4964795 0.04541645  0.4238952  0.5690637
## sigma  5.4081970 0.38242915  4.7970013  6.0193926
```

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
## a     44.9981095 0.22538649 44.6378984 45.3583207
## b      0.6286963 0.02914519  0.5821166  0.6752759
## sigma  4.2296862 0.15941301  3.9749134  4.4844589
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
## 1 44.82069 0.6653022 4.255300
## 2 44.61808 0.6124963 4.308619
## 3 44.98197 0.6448394 4.330455
## 4 45.04989 0.6703432 4.207164
## 5 45.01499 0.5998628 4.345470
## 6 44.84421 0.6480714 4.360352
```

#### 1. Plot the sample

``` r
# 4.4.3
col2 <- col.alpha(2,0.8)
plot(d$height, d$weight, col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-19-1.png" width="672" />

#### 2. Plot the posterior mean

``` r
weight.seq <- seq(from=25,to=70,by=1)
mu <- link(m4.3,data=list(weight=weight.seq))
plot(d$height,d$weight,  col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
lines(  apply(mu,2,mean), weight.seq, lwd=4)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-20-1.png" width="672" />

#### 3. Plot uncertainty of the mean

``` r
xseq <- seq(from=130,to=190,len=50)
plot(d$height, d$weight, col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
lines(   apply(mu,2,mean), weight.seq,lwd=4)
shade( apply(mu,2,PI,prob=0.99), weight.seq, col=col.alpha(2,0.5))
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-21-1.png" width="672" />

#### 4. Plot uncertainty of predictions

{{% callout note %}}

Try as [challenging exercise as shown in Slide 70 of Lecture 3](https://speakerdeck.com/rmcelreath/statistical-rethinking-2022-lecture-03?slide=70).

{{% /callout %}}

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
## [1] gganimate_1.0.7      dagitty_0.3-1        rethinking_2.21     
## [4] cmdstanr_0.4.0.9001  rstan_2.21.3         ggplot2_3.3.5       
## [7] StanHeaders_2.21.0-7
## 
## loaded via a namespace (and not attached):
##  [1] matrixStats_0.61.0   fs_1.5.0             lubridate_1.8.0     
##  [4] progress_1.2.2       tensorA_0.36.2       tools_4.1.1         
##  [7] backports_1.4.1      bslib_0.3.1          utf8_1.2.2          
## [10] R6_2.5.1             DBI_1.1.1            colorspace_2.0-2    
## [13] withr_2.4.3          tidyselect_1.1.1     gridExtra_2.3       
## [16] prettyunits_1.1.1    processx_3.5.2       curl_4.3.2          
## [19] compiler_4.1.1       cli_3.1.0            labeling_0.4.2      
## [22] bookdown_0.24        posterior_1.1.0      sass_0.4.0          
## [25] scales_1.1.1         checkmate_2.0.0      mvtnorm_1.1-3       
## [28] callr_3.7.0          bsplus_0.1.3         stringr_1.4.0       
## [31] digest_0.6.29        rmarkdown_2.11       base64enc_0.1-3     
## [34] pkgconfig_2.0.3      htmltools_0.5.2      fastmap_1.1.0       
## [37] highr_0.9            rlang_0.4.12         rstudioapi_0.13     
## [40] shape_1.4.6          jquerylib_0.1.4      farver_2.1.0        
## [43] generics_0.1.1       jsonlite_1.7.2       dplyr_1.0.7         
## [46] distributional_0.2.2 inline_0.3.19        magrittr_2.0.1      
## [49] loo_2.4.1            Rcpp_1.0.7           munsell_0.5.0       
## [52] fansi_0.5.0          abind_1.4-5          lifecycle_1.0.1     
## [55] stringi_1.7.6        yaml_2.2.1           MASS_7.3-54         
## [58] plyr_1.8.6           pkgbuild_1.3.1       grid_4.1.1          
## [61] crayon_1.4.2         lattice_0.20-44      hms_1.1.1           
## [64] magick_2.7.3         knitr_1.36           ps_1.6.0            
## [67] pillar_1.6.4         uuid_1.0-3           boot_1.3-28         
## [70] codetools_0.2-18     stats4_4.1.1         glue_1.6.0          
## [73] evaluate_0.14        blogdown_1.5         V8_3.6.0            
## [76] renv_0.14.0          RcppParallel_5.1.4   tweenr_1.0.2        
## [79] vctrs_0.3.8          gtable_0.3.0         purrr_0.3.4         
## [82] assertthat_0.2.1     xfun_0.28            mime_0.12           
## [85] coda_0.19-4          tibble_3.1.6         ellipsis_0.3.2      
## [88] downloadthis_0.2.1   xaringanExtra_0.5.5
```
