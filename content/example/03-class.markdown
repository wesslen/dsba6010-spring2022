---
date: "2022-01-24"
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
<a href="data:application/octet-stream;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyAzIgptZW51OgogIGV4YW1wbGU6CiAgICBwYXJlbnQ6IEV4YW1wbGVzCndlaWdodDogMwp0b2M6IHRydWUKdHlwZTogZG9jcwotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFLCBmaWcud2lkdGg9NSwgZmlnLmhlaWdodD00fQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIGNsYXNzLnNvdXJjZT0ibGFuZ3VhZ2UtciIsIGNsYXNzLm91dHB1dD0ibGFuZ3VhZ2UtciIsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQp4YXJpbmdhbkV4dHJhOjp1c2VfY2xpcGJvYXJkKCkKbGlicmFyeShyZXRoaW5raW5nKQpgYGAKCmBgYHtyIGVjaG89RkFMU0V9CmRvd25sb2FkdGhpczo6ZG93bmxvYWRfZmlsZSgKICBwYXRoID0gIjAzLWNsYXNzLlJtYXJrZG93biIsCiAgb3V0cHV0X25hbWUgPSAiMDMtY2xhc3MiLAogIGJ1dHRvbl9sYWJlbCA9ICJEb3dubG9hZCB0aGlzIGNvZGUiLAogIGJ1dHRvbl90eXBlID0gImRhbmdlciIsCiAgaGFzX2ljb24gPSBUUlVFLAogIGljb24gPSAiZmEgZmEtc2F2ZSIsCiAgc2VsZl9jb250YWluZWQgPSBGQUxTRQopCmBgYApgYGB7ciBlY2hvPUZBTFNFLCBmaWcuYWxpZ249ImxlZnQiLCBmaWcubGluaz0naHR0cHM6Ly9naXRwb2QuaW8vI2h0dHBzOi8vZ2l0aHViLmNvbS93ZXNzbGVuL2RzYmE2MDEwX2V4YW1wbGVzJ30Ka25pdHI6OmluY2x1ZGVfZ3JhcGhpY3MocGF0aD0iaHR0cHM6Ly9naXRwb2QuaW8vYnV0dG9uL29wZW4taW4tZ2l0cG9kLnN2ZyIpCmBgYAoKIyMgSW50cm9kdWN0aW9uCgpGb3IgdGhpcyBjbGFzcywgd2UnbGwgcmV2aWV3IGNvZGUgZXhhbXBsZXMgZm91bmQgaW4gQ2hhcHRlciA0LgoKVGhpcyBhc3N1bWVzIHRoYXQgeW91IGhhdmUgYWxyZWFkeSBpbnN0YWxsZWQgdGhlIGByZXRoaW5raW5nYCBwYWNrYWdlLgoKSWYgeW91IG5lZWQgaGVscCwgYmUgc3VyZSB0byByZW1lbWJlciB0aGUgcmVmZXJlbmNlcyBpbiB0aGUgW1Jlc291cmNlc10oL3Jlc291cmNlLyk6CgoqIFtJbnN0YWxsaW5nIFIvUlN0dWRpb10oL3Jlc291cmNlL2luc3RhbGwvKQoqIFtJbnN0YWxsaW5nIGByZXRoaW5raW5nYCBwYWNrYWdlXSgvcmVzb3VyY2UvaW5zdGFsbC1yZXRoaW5raW5nLykKKiBbUm1hcmtkb3duXSgvcmVzb3VyY2Uvcm1hcmtkb3duLykKKiBbUiBTdHlsZSBndWlkZV0oL3Jlc291cmNlL3N0eWxlLykKCiMjIENoYXB0ZXIgNCAKCiMjIyBEcmF3aW5nIHRoZSBPd2wKCiFbXSguLi8uLi9pbWcvc3lsbGFidXMvb3dsLnBuZykKCgojIyMgMS4gUXVlc3Rpb24gb3IgZXN0aW1hbmQKCk9iamVjdGl2ZTogRGVzY3JpYmUgdGhlIGFzc29jaWF0aW9uIGJldHdlZW4gQWR1bHQgKip3ZWlnaHQqKiBhbmQgKipoZWlnaHQqKgoKYGBge3J9CmxpYnJhcnkocmV0aGlua2luZykKZGF0YShIb3dlbGwxKQpkIDwtIEhvd2VsbDFbSG93ZWxsMSRhZ2U+PTE4LF0KCnBsb3QoZCR3ZWlnaHQsIGQkaGVpZ2h0LCBjb2wgPSAyLCB4bGFiID0gIndlaWdodCAoa2cpIiwgIHlsYWIgPSAiaGVpZ2h0IChjbSkiLCBsd2Q9MykKYGBgCgpBbHRlcm5hdGl2ZWx5LCB1c2luZyBgZ2dwbG90MmAgd2UgY2FuIHBsb3QgdGhlIHNhbWU6CgpgYGB7cn0KbGlicmFyeShnZ3Bsb3QyKQoKZ2dwbG90KGQsIGFlcyh4ID0gd2VpZ2h0LCB5ID0gaGVpZ2h0KSkgKwogIGdlb21fcG9pbnQoY29sb3IgPSAicmVkIikgKwogIGxhYnMoeCA9ICJ3ZWlnaHQgKGtnKSIsIHkgPSAiaGVpZ2h0IChjbSkiKSArCiAgdGhlbWVfYncoKQpgYGAKCkxldCdzIGNhbGN1bGF0ZSB0aGUgYXZlcmFnZSB3ZWlnaHRzIC0tIHdlJ2xsIHVzZSB0aGlzIGxhdGVyIG9uIHdoZW4gd2Ugd2FudCB0byBzdGFuZGFyZGl6ZSBvdXIgZGF0YS4KCmBgYHtyfQp4YmFyIDwtIG1lYW4oZCR3ZWlnaHQpCnhiYXIKYGBgCgojIyMgMi4gU2NpZW50aWZpYyBtb2RlbAoKV2VpZ2h0IGlzIGEgZnVuY3Rpb24gb2YgaGVpZ2h0LiAKCmBgYHtyIGZpZy5oZWlnaHQ9MSwgZmlnLndpZHRoPTJ9CmxpYnJhcnkoZGFnaXR0eSkKCmcgPC0gZGFnaXR0eSgnZGFnIHsKICAgIFcgW3Bvcz0iMCwxIl0KICAgIEggW3Bvcz0iMSwxIl0KICAgIAogICAgVyAtPiBICn0nKQpwbG90KGcpCmBgYAoKIyMjIDMuIFN0YXRpc3RpY2FsIG1vZGVsCgojIyMjIEdlbmVyYXRpdmUgTW9kZWwKCkxldCdzIGNvbnNpZGVyIHRoZSBHZW5lcmF0aXZlIE1vZGVsIChIIC0+IFcpIGZyb20gdGhlIGxlY3R1cmU6CgokV19pIFxzaW0gTm9ybWFsKFxtdV9pLFxzaWdtYSkkPGJyPgokXG11X2kgPSBcYWxwaGEgKyBcYmV0YSBIX2kkPGJyPgoKTGV0J3Mgbm93IGNvbmR1Y3QgYSBwcmlvciBwcmVkaWN0aXZlIHNpbXVsYXRpb24gdG8gdW5kZXJzdGFuZAoKYGBge3J9CnNldC5zZWVkKDEwMCkKIyBmb3J3YXJkIHNpbXVsYXRpb24gYXMgd2UgY2hvb3NlIHRoZXNlIHBhcmFtZXRlcnMKYWxwaGEgPC0gMCAjIGltcGxpZXMgemVybyBoZWlnaHQgPT4gemVybyB3ZWlnaHQKYmV0YSA8LSAwLjUgCnNpZ21hIDwtIDUKbl9pbmRpdmlkdWFscyA8LSAxMDAKCkggPC0gcnVuaWYobl9pbmRpdmlkdWFscywxMzAsMTcwKSAjIGhlaWdodHMsIHVuaWZvcm0gYmV0d2VlbiAxMzAgLSAxNzAgY20KCm11IDwtIGFscGhhICsgYmV0YSpIClcgPC0gcm5vcm0obl9pbmRpdmlkdWFscyxtdSxzaWdtYSkgIyBzYW1wbGUgZnJvbSBOb3JtYWwKYGBgCgpgYGB7cn0KY29sMiA8LSBjb2wuYWxwaGEoMiwwLjgpCnBsb3QoVyxILCAgY29sPWNvbDIsIGx3ZD0zLAogICAgIGNleD0xLjIsICB4bGFiID0gIndlaWdodCAoa2cpIiwgeWxhYiA9ICJoZWlnaHQgKGNtKSIpCm10ZXh0KCAiMTAwIFNpbXVsYXRlZCBwZW9wbGUiICkKYGBgCgpgYGB7cn0KIyBnZ3Bsb3QyIHZlcnNpb24KZGYgPC0gZGF0YS5mcmFtZSgKICBoZWlnaHQgPSBILAogIHdlaWdodCA9IFcKKQoKZ2dwbG90KGRmLCBhZXMoeCA9IHdlaWdodCwgeSA9IGhlaWdodCkpICsKICBnZW9tX3BvaW50KGNvbG9yPSJyZWQiKSArCiAgbGFicyh4ID0gIndlaWdodCAoa2cpIiwgeSA9ICJoZWlnaHQgKGNtKSIsIHRpdGxlID0gIjEwMCBTaW11bGF0ZWQgcGVvcGxlIikKYGBgCgojIyMjIFNhbXBsaW5nIHRoZSBwcmlvciBkaXN0cmlidXRpb24KCmBgYHtyfQpuX3NhbXBsZXMgPC0gMTAKCmFscGhhIDwtIHJub3JtKG5fc2FtcGxlcywwLDEpCmJldGEgPC0gcm5vcm0obl9zYW1wbGVzLDAsMSkKCnBsb3QoTlVMTCx4bGltPWMoLTIsMikseWxpbT1jKC0yLDIpLHhsYWI9IngiLHlsYWI9InkiKQpmb3IgKGkgaW4gMTpuX3NhbXBsZXMpewogIGFibGluZShhbHBoYVtpXSxiZXRhW2ldLGx3ZD00LGNvbD0yKQp9CmBgYAoKYGBge3J9CmRmMiA8LSBkYXRhLmZyYW1lKAogIGFscGhhID0gYWxwaGEsCiAgYmV0YSA9IGJldGEsCiAgc2FtcCA9IDE6bl9zYW1wbGVzCikKCnAgPC0gZ2dwbG90KGRmMikgKwogIGdlb21fYWJsaW5lKGFlcyhzbG9wZSA9IGJldGEsIGludGVyY2VwdCA9IGFscGhhLCBncm91cCA9IHNhbXApKSArCiAgeGxpbSgtMywzKSArCiAgeWxpbSgtMywzKQoKcApgYGAKCmBgYHtyIGZpZy5oZWlnaHQ9MixmaWcud2lkdGg9Mn0KbGlicmFyeShnZ2FuaW1hdGUpCgphbmltIDwtIHAgKyB0cmFuc2l0aW9uX3N0YXRlcyhzYW1wLCB0cmFuc2l0aW9uX2xlbmd0aD0yLCBzdGF0ZV9sZW5ndGg9MSkgKyAKICBnZ3RpdGxlKCdTYW1wbGUge2Nsb3Nlc3Rfc3RhdGV9IG9mIDEwJykgKwogIGVudGVyX2ZhZGUoKSArCiAgZXhpdF9mYWRlKCkKCmFuaW0KI2FuaW1hdGUoYW5pbSwgaGVpZ2h0PTIsIHdpZHRoPTMpCmBgYAoKIyMjIDMuIFN0YXRpc3RpY2FsIG1vZGVsIGZvciBILT5XCgpgYGB7cn0KbiA8LSAxMAphbHBoYSA8LSBybm9ybShuLDYwLDEwKQpiZXRhIDwtIHJub3JtKG4sMCwxMCkKCkhiYXIgPC0gMTUwCkhzZXEgPC0gc2VxKGZyb209MTMwLHRvPTE3MCxsZW49MzApCnBsb3QoTlVMTCwgeGxpbT1jKDEzMCwxNzApLCB5bGltPWMoMTAsMTAwKSwKICAgICB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKCmZvciAoaSBpbiAxOm4pewogIGxpbmVzKEhzZXEsIGFscGhhW2ldICsgYmV0YVtpXSooSHNlcS1IYmFyKSxsd2Q9Myxjb2w9MikKfQpgYGAKCklzIHRoaXMgYSBnb29kIHByaW9yIHRvIGJlIHVzZWQ/IFdoeSBvciB3aHkgbm90IGFyZSB0aGV5IGludGVycHJldGFibGU/CgpSZW1lbWJlciwgYSBsb2dub3JtYWwgZGlzdHJpYnV0aW9uIGlzIGEgZGlzdHJpYnV0aW9uIHRoYXQgaWYgeW91IHRha2UgdGhlIGxvZ2FyaXRobSBvZiB0aGUgdmFsdWVzLCB0aGVuIGFsbCBvZiBpdCdzIHZhbHVlcyB3b3VsZCBiZSBub3JtYWwuIAoKYGBge3J9CiMgc2ltdWxhdGVkIGxvZ25vcm1hbApiIDwtIHJsbm9ybSgxZTQsIDAsIDEpICM0LjQwCmRlbnMoYiwgeGxpbT1jKDAsNSksIGFkaj0wLjEpCmBgYAoKTGV0J3MgZG8gYSBwcmVkaWN0aXZlIHNpbXVsYXRpb24gbm93IHVzaW5nIHRoZSBMb2ctTm9ybWFsIHByaW9yLgoKYGBge3J9CnNldC5zZWVkKDI5NzEpCk4gPC0gMTAwICAKYSA8LSBybm9ybSggTiAsIDYwICwgMTAgKQpiIDwtIHJsbm9ybSggTiAsIDAgLCAxICkKCnBsb3QoTlVMTCwgeGxpbT1jKDEzMCwxNzApLCB5bGltPWMoMTAsMTAwKSwKICAgICB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKCmZvciAoaSBpbiAxOm4pewogIGxpbmVzKEhzZXEsIGFbaV0gKyBiW2ldKihIc2VxLUhiYXIpLGx3ZD0zLGNvbD0yKQp9CmBgYAp7eyUgY2FsbG91dCBub3RlICV9fQoKS2V5IGlzIGp1c3RpZnkgcHJpb3JzIHdpdGggaW5mb3JtYXRpb24gb3V0c2lkZSBvZiB0aGUgZGF0YSAodGhhdCB3aWxsIGJlIG1vZGVsZWQpLiBUaGlzIGlzIHNpbWlsYXIgdG8gbWFjaGluZSBsZWFybmluZyB3aGVyZSB3ZSBkb24ndCB3YW50IHRvIGluY2x1ZGUgcmVjb3JkcyBpbiBvdXIgdGVzdCBkYXRhc2V0IHRoYXQgd2VyZSBhbHNvIGluIG91ciB0cmFpbmluZy4gVXNpbmcgbW9kZWxlZCBkYXRhIHRvIGZvcm0gcHJpb3JzIGNhbiBiZSB0aG91Z2h0IG9mIGFzICJwcmlvci1oYWNraW5nIi4gVHlwaWNhbGx5IGluIGxpdGVyYXR1cmUsIEJheWVzaWFuIGFwcHJvYWNoZXMgcmVxdWlyZSBwcmUtcmVnaXN0cmF0aW9uIHdoZW4gdXNpbmcgbm9uLWluZm9ybWF0aXZlIHByaW9ycyAoc2VlIFt0aGlzIGV4YW1wbGUgZnJvbSBGZXJuYW5kZXMgZXQgYWwuLCAyMDE4XShodHRwczovL2dpdGh1Yi5jb20vbWljaGFlbC1mZXJuYW5kZXMvdW5jZXJ0YWludHktZGlzcGxheXMtZm9yLXRyYW5zaXQvYmxvYi9tYXN0ZXIvcHJlLXJlZ2lzdHJhdGlvbi5wZGYpKS4KCnt7JSAvY2FsbG91dCAlfX0KCiRXX2kgXHNpbSBOb3JtYWwoXG11X2ksXHNpZ21hKSQ8YnI+CiRcbXVfaSA9IFxhbHBoYSArIFxiZXRhKEhfaSAtIFxvdmVybGluZXtIfSkkPGJyPgokXGFscGhhIFxzaW0gTm9ybWFsKDYwLDEwKSQ8YnI+CiRcYmV0YSBcc2ltIExvZ05vcm1hbCgwLDEpJDxicj4KJFxzaWdtYSBcc2ltIFVuaWZvcm0oMCwxMCkkPGJyPgoKYGBge3J9CiMgZGVmaW5lIHRoZSBhdmVyYWdlIHdlaWdodCwgeC1iYXIKeGJhciA8LSBtZWFuKGQkd2VpZ2h0KQoKIyBmaXQgbW9kZWwKbTQuMyA8LSBxdWFwKAogICAgYWxpc3QoCiAgICAgICAgaGVpZ2h0IH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICAgICAgbXUgPC0gYSArIGIqKCB3ZWlnaHQgLSB4YmFyICkgLAogICAgICAgIGEgfiBkbm9ybSggMTc4ICwgMjAgKSAsCiAgICAgICAgYiB+IGRsbm9ybSggMCAsIDEgKSAsCiAgICAgICAgc2lnbWEgfiBkdW5pZiggMCAsIDUwICkKICAgICkgLCBkYXRhPWQgKQoKIyMgUiBjb2RlIDQuNDMKbTQuM2IgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIGhlaWdodCB+IGRub3JtKCBtdSAsIHNpZ21hICkgLAogICAgICAgIG11IDwtIGEgKyBleHAobG9nX2IpKiggd2VpZ2h0IC0geGJhciApLAogICAgICAgIGEgfiBkbm9ybSggMTc4ICwgMjAgKSAsCiAgICAgICAgbG9nX2IgfiBkbm9ybSggMCAsIDEgKSAsCiAgICAgICAgc2lnbWEgfiBkdW5pZiggMCAsIDUwICkKICAgICkgLCBkYXRhPWQgKQoKIyMgUiBjb2RlIDQuNDQKcHJlY2lzKCBtNC4zICkKYGBgCgpUaGUgZmlyc3Qgcm93IGdpdmVzIHRoZSBxdWFkcmF0aWMgYXBwcm94aW1hdGlvbiBmb3IgzrEsIHRoZSBzZWNvbmQgdGhlIGFwcHJveGltYXRpb24gZm9yIM6yLCBhbmQgdGhlIHRoaXJkIGFwcHJveGltYXRpb24gZm9yIM+DLgoKTGV04oCZcyBmb2N1cyBvbiBiICjOsiksIGJlY2F1c2UgaXTigJlzIHRoZSBuZXcgcGFyYW1ldGVyLiBTaW5jZSDOsiBpcyBhIHNsb3BlLCB0aGUgdmFsdWUgMC45MCBjYW4gYmUgcmVhZCBhcyBhICoqcGVyc29uIDEga2cgaGVhdmllciBpcyBleHBlY3RlZCB0byBiZSAwLjkwIGNtIHRhbGxlci4qKiA4OSUgb2YgdGhlIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBsaWVzIGJldHdlZW4gMC44NCBhbmQgMC45Ny4gVGhhdCBzdWdnZXN0cyB0aGF0ICoqzrIgdmFsdWVzIGNsb3NlIHRvIHplcm8gb3IgZ3JlYXRseSBhYm92ZSBvbmUgYXJlIGhpZ2hseSBpbmNvbXBhdGlibGUgd2l0aCB0aGVzZSBkYXRhIGFuZCB0aGlzIG1vZGVsLioqIEl0IGlzIG1vc3QgY2VydGFpbmx5IG5vdCBldmlkZW5jZSB0aGF0IHRoZSByZWxhdGlvbnNoaXAgYmV0d2VlbiB3ZWlnaHQgYW5kIGhlaWdodCBpcyBsaW5lYXIsIGJlY2F1c2UgdGhlIG1vZGVsIG9ubHkgY29uc2lkZXJlZCBsaW5lcy4gSXQganVzdCBzYXlzIHRoYXQsIGlmIHlvdSBhcmUgY29tbWl0dGVkIHRvIGEgbGluZSwgdGhlbiBsaW5lcyB3aXRoIGEgc2xvcGUgYXJvdW5kIDAuOSBhcmUgcGxhdXNpYmxlIG9uZXMuCgpgYGB7cn0KIyMgUiBjb2RlIDQuNDUKcm91bmQoIHZjb3YoIG00LjMgKSAsIDMgKQpgYGAKCmBgYHtyfQojIHNob3dzIGJvdGggdGhlIG1hcmdpbmFsIHBvc3RlcmlvcnMgYW5kIHRoZSBjb3ZhcmlhbmNlLgpwYWlycyhtNC4zKQpgYGAKClRoZXJlIGlzIGxpdHRsZSBjb3ZhcmlhdGlvbiBhbW9uZyB0aGUgcGFyYW1ldGVycyBpbiB0aGlzIGNhc2UuIFRoZSBsYWNrIG9mIGNvdmFyaWFuY2UgYW1vbmcgdGhlIHBhcmFtZXRlcnMgKipyZXN1bHRzIGZyb20gY2VudGVyaW5nLioqCgpBcyBhbiBleGVyY2lzZSwgY29uc2lkZXIgcmVydW5uaW5nIHRoZSByZWdyZXNzaW9uIGFib3ZlIHdpdGhvdXQgY2VudGVyaW5nIGFuZCBjb21wYXJlIHRoZSBjb3ZhcmlhdGlvbi4KCiMjIyA0LiBWYWxpZGF0ZSBtb2RlbAoKV2UnbGwgdXNlIGEgKipzaW11bGF0aW9uLWJhc2VkKiogdmFsaWRhdGlvbi4KCldlJ2xsIGZpcnN0IHZhbGlkYXRlIHdpdGggYSBzaW11bGF0aW9uIChha2EgZmFrZSBkYXRhKS4KCmBgYHtyfQphbHBoYSA8LSA3MApiZXRhIDwtIDAuNQpzaWdtYSA8LSA1Cm5faW5kaXZpZHVhbHMgPC0gMTAwCkggPC0gcnVuaWYobl9pbmRpdmlkdWFscywxMzAsMTcwKQptdSA8LSBhbHBoYSArIGJldGEqKEgtbWVhbihIKSkKVyA8LSBybm9ybShuX2luZGl2aWR1YWxzLG11LHNpZ21hKQoKZGF0IDwtIGxpc3QoSD1ILFc9VyxIYmFyPW1lYW4oSCkpCgptX3ZhbGlkYXRlIDwtIHF1YXAoCiAgICBhbGlzdCgKICAgICAgICBXIH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICAgICAgbXUgPC0gYSArIGIqKCBIIC0gSGJhciApLAogICAgICAgIGEgfiBkbm9ybSggNjAgLCAxMCApICwKICAgICAgICBiIH4gZGxub3JtKCAwICwgMSApICwKICAgICAgICBzaWdtYSB+IGR1bmlmKCAwICwgMTAgKQogICAgKSAsIGRhdGE9ZGF0ICkKCnByZWNpcyhtX3ZhbGlkYXRlKQpgYGAKCmBgYHtyfQpkYXQgPC0gbGlzdChXID0gZCR3ZWlnaHQsIEggPSBkJGhlaWdodCwgSGJhciA9IG1lYW4oZCRoZWlnaHQpKQoKbV9hZHVsdHMgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgICAgICBtdSA8LSBhICsgYiooIEggLSBIYmFyICksCiAgICAgICAgYSB+IGRub3JtKCA2MCAsIDEwICkgLAogICAgICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgICAgIHNpZ21hIH4gZHVuaWYoIDAgLCAxMCApCiAgICApICwgZGF0YT1kYXQgKQoKcHJlY2lzKG1fYWR1bHRzKQpgYGAKCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpGaXJzdCBMYXcgb2YgU3RhdGlzdGljYWwgSW50ZXJwcmV0YXRpb246IFRoZSAqKnBhcmFtZXRlcnMgYXJlIG5vdCBpbmRlcGVuZGVudCoqIG9mIG9uZSBhbm90aGVyIGFuZCBjYW5ub3QgYWx3YXlzIGJlIGluZGVwZW5kZW50bHkgaW50ZXJwcmV0ZWQuCgpJbnN0ZWFkLCBkcmF3IChwdXNoIG91dCkgKipwb3N0ZXJpb3IgcHJlZGljdGlvbnMqKiBhbmQgZGVzY3JpYmUvaW50ZXJwcmV0IHRoZW0uCgp7eyUgL2NhbGxvdXQgJX19CgpgYGB7cn0KcG9zdCA8LSBleHRyYWN0LnNhbXBsZXMobV9hZHVsdHMpCmhlYWQocG9zdCkKYGBgCgojIyMjIDEuIFBsb3QgdGhlIHNhbXBsZQoKYGBge3J9CiMgNC40LjMKY29sMiA8LSBjb2wuYWxwaGEoMiwwLjgpCnBsb3QoZCRoZWlnaHQsIGQkd2VpZ2h0LCBjb2w9Y29sMiwgbHdkPTMsCiAgICAgY2V4PTEuMiwgeGxhYj0iaGVpZ2h0IChjbSkiLCB5bGFiPSJ3ZWlnaHQgKGtnKSIpCmBgYAoKIyMjIyAyLiBQbG90IHRoZSBwb3N0ZXJpb3IgbWVhbgoKCmBgYHtyfQp3ZWlnaHQuc2VxIDwtIHNlcShmcm9tPTI1LHRvPTcwLGJ5PTEpCm11IDwtIGxpbmsobTQuMyxkYXRhPWxpc3Qod2VpZ2h0PXdlaWdodC5zZXEpKQpwbG90KGQkaGVpZ2h0LGQkd2VpZ2h0LCAgY29sPWNvbDIsIGx3ZD0zLAogICAgIGNleD0xLjIsIHhsYWI9ImhlaWdodCAoY20pIiwgeWxhYj0id2VpZ2h0IChrZykiKQpsaW5lcyggIGFwcGx5KG11LDIsbWVhbiksIHdlaWdodC5zZXEsIGx3ZD00KQpgYGAKCiMjIyMgMy4gUGxvdCB1bmNlcnRhaW50eSBvZiB0aGUgbWVhbgoKYGBge3J9CnhzZXEgPC0gc2VxKGZyb209MTMwLHRvPTE5MCxsZW49NTApCnBsb3QoZCRoZWlnaHQsIGQkd2VpZ2h0LCBjb2w9Y29sMiwgbHdkPTMsCiAgICAgY2V4PTEuMiwgeGxhYj0iaGVpZ2h0IChjbSkiLCB5bGFiPSJ3ZWlnaHQgKGtnKSIpCmxpbmVzKCAgIGFwcGx5KG11LDIsbWVhbiksIHdlaWdodC5zZXEsbHdkPTQpCnNoYWRlKCBhcHBseShtdSwyLFBJLHByb2I9MC45OSksIHdlaWdodC5zZXEsIGNvbD1jb2wuYWxwaGEoMiwwLjUpKQpgYGAKCiMjIyMgNC4gUGxvdCB1bmNlcnRhaW50eSBvZiBwcmVkaWN0aW9ucwoKe3slIGNhbGxvdXQgbm90ZSAlfX0KClRyeSBhcyBbY2hhbGxlbmdpbmcgZXhlcmNpc2UgYXMgc2hvd24gaW4gU2xpZGUgNzAgb2YgTGVjdHVyZSAzXShodHRwczovL3NwZWFrZXJkZWNrLmNvbS9ybWNlbHJlYXRoL3N0YXRpc3RpY2FsLXJldGhpbmtpbmctMjAyMi1sZWN0dXJlLTAzP3NsaWRlPTcwKS4KCnt7JSAvY2FsbG91dCAlfX0KCiMjIFBhY2thZ2UgdmVyc2lvbnMKCmBgYHtyfQpzZXNzaW9uSW5mbygpCmBgYA==" download="03-class.Rmarkdown">
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

plot(d$weight, d$height, col = 2, xlab = "weight (kg)",  ylab = "height (cm)", lwd=3)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Alternatively, using `ggplot2` we can plot the same:

``` r
library(ggplot2)

ggplot(d, aes(x = weight, y = height)) +
  geom_point(color = "red") +
  labs(x = "weight (kg)", y = "height (cm)") +
  theme_bw()
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Let’s calculate the average weights – we’ll use this later on when we want to standardize our data.

``` r
xbar <- mean(d$weight)
xbar
```

``` language-r
## [1] 44.99049
```

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-6-1.png" width="192" />

### 3. Statistical model

#### Generative Model

Let’s consider the Generative Model (H -&gt; W) from the lecture:

`\(W_i \sim Normal(\mu_i,\sigma)\)`<br>
`\(\mu_i = \alpha + \beta H_i\)`<br>

Let’s now conduct a prior predictive simulation to understand

``` r
set.seed(100)
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

<img src="/example/03-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

``` r
# ggplot2 version
df <- data.frame(
  height = H,
  weight = W
)

ggplot(df, aes(x = weight, y = height)) +
  geom_point(color="red") +
  labs(x = "weight (kg)", y = "height (cm)", title = "100 Simulated people")
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-9-1.png" width="672" />

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-10-1.png" width="672" />

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-11-1.png" width="672" />

``` r
library(gganimate)

anim <- p + transition_states(samp, transition_length=2, state_length=1) + 
  ggtitle('Sample {closest_state} of 10') +
  enter_fade() +
  exit_fade()

anim
```

![](03-class_files/figure-html/unnamed-chunk-12-1.gif)<!-- -->

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-13-1.png" width="672" />

Is this a good prior to be used? Why or why not are they interpretable?

Remember, a lognormal distribution is a distribution that if you take the logarithm of the values, then all of it’s values would be normal.

``` r
# simulated lognormal
b <- rlnorm(1e4, 0, 1) #4.40
dens(b, xlim=c(0,5), adj=0.1)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-14-1.png" width="672" />

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-15-1.png" width="672" />
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

The first row gives the quadratic approximation for α, the second the approximation for β, and the third approximation for σ.

Let’s focus on b (β), because it’s the new parameter. Since β is a slope, the value 0.90 can be read as a **person 1 kg heavier is expected to be 0.90 cm taller.** 89% of the posterior probability lies between 0.84 and 0.97. That suggests that **β values close to zero or greatly above one are highly incompatible with these data and this model.** It is most certainly not evidence that the relationship between weight and height is linear, because the model only considered lines. It just says that, if you are committed to a line, then lines with a slope around 0.9 are plausible ones.

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

<img src="/example/03-class_files/figure-html/unnamed-chunk-18-1.png" width="672" />

There is little covariation among the parameters in this case. The lack of covariance among the parameters **results from centering.**

As an exercise, consider rerunning the regression above without centering and compare the covariation.

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
## a     70.3773460 0.46553547 69.6333304 71.1213616
## b      0.5010175 0.04095501  0.4355635  0.5664715
## sigma  4.6602986 0.32954313  4.1336251  5.1869722
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
##            mean         sd       5.5%      94.5%
## a     44.998109 0.22538634 44.6378981 45.3583199
## b      0.628693 0.02914517  0.5821134  0.6752726
## sigma  4.229683 0.15941274  3.9749109  4.4844555
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
## 1 45.09097 0.6114659 4.072269
## 2 44.92196 0.6446553 4.150536
## 3 44.90002 0.6615642 4.240321
## 4 44.77715 0.6528529 4.118222
## 5 45.06723 0.5947575 4.375431
## 6 45.06122 0.6454858 4.268757
```

#### 1. Plot the sample

``` r
# 4.4.3
col2 <- col.alpha(2,0.8)
plot(d$height, d$weight, col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-22-1.png" width="672" />

#### 2. Plot the posterior mean

``` r
weight.seq <- seq(from=25,to=70,by=1)
mu <- link(m4.3,data=list(weight=weight.seq))
plot(d$height,d$weight,  col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
lines(  apply(mu,2,mean), weight.seq, lwd=4)
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-23-1.png" width="672" />

#### 3. Plot uncertainty of the mean

``` r
xseq <- seq(from=130,to=190,len=50)
plot(d$height, d$weight, col=col2, lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
lines(   apply(mu,2,mean), weight.seq,lwd=4)
shade( apply(mu,2,PI,prob=0.99), weight.seq, col=col.alpha(2,0.5))
```

<img src="/example/03-class_files/figure-html/unnamed-chunk-24-1.png" width="672" />

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
## [10] R6_2.5.1             KernSmooth_2.23-20   DBI_1.1.1           
## [13] colorspace_2.0-2     withr_2.4.3          tidyselect_1.1.1    
## [16] gridExtra_2.3        prettyunits_1.1.1    processx_3.5.2      
## [19] curl_4.3.2           compiler_4.1.1       cli_3.1.0           
## [22] labeling_0.4.2       bookdown_0.24        posterior_1.1.0     
## [25] sass_0.4.0           scales_1.1.1         checkmate_2.0.0     
## [28] mvtnorm_1.1-3        callr_3.7.0          bsplus_0.1.3        
## [31] stringr_1.4.0        digest_0.6.29        rmarkdown_2.11      
## [34] base64enc_0.1-3      pkgconfig_2.0.3      htmltools_0.5.2     
## [37] fastmap_1.1.0        highr_0.9            rlang_0.4.12        
## [40] rstudioapi_0.13      shape_1.4.6          jquerylib_0.1.4     
## [43] farver_2.1.0         generics_0.1.1       jsonlite_1.7.2      
## [46] dplyr_1.0.7          distributional_0.2.2 inline_0.3.19       
## [49] magrittr_2.0.1       loo_2.4.1            Rcpp_1.0.7          
## [52] munsell_0.5.0        fansi_0.5.0          abind_1.4-5         
## [55] lifecycle_1.0.1      stringi_1.7.6        yaml_2.2.1          
## [58] MASS_7.3-54          plyr_1.8.6           pkgbuild_1.3.1      
## [61] grid_4.1.1           crayon_1.4.2         lattice_0.20-44     
## [64] hms_1.1.1            magick_2.7.3         knitr_1.36          
## [67] ps_1.6.0             pillar_1.6.4         uuid_1.0-3          
## [70] boot_1.3-28          codetools_0.2-18     stats4_4.1.1        
## [73] glue_1.6.0           evaluate_0.14        blogdown_1.5        
## [76] V8_3.6.0             renv_0.14.0          RcppParallel_5.1.4  
## [79] tweenr_1.0.2         vctrs_0.3.8          gtable_0.3.0        
## [82] purrr_0.3.4          assertthat_0.2.1     xfun_0.28           
## [85] mime_0.12            coda_0.19-4          tibble_3.1.6        
## [88] ellipsis_0.3.2       downloadthis_0.2.1   xaringanExtra_0.5.5
```
