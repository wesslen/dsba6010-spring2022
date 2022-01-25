---
date: "2022-01-25"
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
<a href="data:application/octet-stream;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyAzIgptZW51OgogIGV4YW1wbGU6CiAgICBwYXJlbnQ6IEV4YW1wbGVzCndlaWdodDogMwp0b2M6IHRydWUKdHlwZTogZG9jcwotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFLCBmaWcud2lkdGg9NSwgZmlnLmhlaWdodD00fQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIGNsYXNzLnNvdXJjZT0ibGFuZ3VhZ2UtciIsIGNsYXNzLm91dHB1dD0ibGFuZ3VhZ2UtciIsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQp4YXJpbmdhbkV4dHJhOjp1c2VfY2xpcGJvYXJkKCkKbGlicmFyeShyZXRoaW5raW5nKQpgYGAKCmBgYHtyIGVjaG89RkFMU0V9CmRvd25sb2FkdGhpczo6ZG93bmxvYWRfZmlsZSgKICBwYXRoID0gIjAzLWNsYXNzLlJtYXJrZG93biIsCiAgb3V0cHV0X25hbWUgPSAiMDMtY2xhc3MiLAogIGJ1dHRvbl9sYWJlbCA9ICJEb3dubG9hZCB0aGlzIGNvZGUiLAogIGJ1dHRvbl90eXBlID0gImRhbmdlciIsCiAgaGFzX2ljb24gPSBUUlVFLAogIGljb24gPSAiZmEgZmEtc2F2ZSIsCiAgc2VsZl9jb250YWluZWQgPSBGQUxTRQopCmBgYApgYGB7ciBlY2hvPUZBTFNFLCBmaWcuYWxpZ249ImxlZnQiLCBmaWcubGluaz0naHR0cHM6Ly9naXRwb2QuaW8vI2h0dHBzOi8vZ2l0aHViLmNvbS93ZXNzbGVuL2RzYmE2MDEwX2V4YW1wbGVzJ30Ka25pdHI6OmluY2x1ZGVfZ3JhcGhpY3MocGF0aD0iaHR0cHM6Ly9naXRwb2QuaW8vYnV0dG9uL29wZW4taW4tZ2l0cG9kLnN2ZyIpCmBgYAoKIyMgSW50cm9kdWN0aW9uCgpGb3IgdGhpcyBjbGFzcywgd2UnbGwgcmV2aWV3IGNvZGUgZXhhbXBsZXMgZm91bmQgaW4gQ2hhcHRlciA0LgoKVGhpcyBhc3N1bWVzIHRoYXQgeW91IGhhdmUgYWxyZWFkeSBpbnN0YWxsZWQgdGhlIGByZXRoaW5raW5nYCBwYWNrYWdlLgoKSWYgeW91IG5lZWQgaGVscCwgYmUgc3VyZSB0byByZW1lbWJlciB0aGUgcmVmZXJlbmNlcyBpbiB0aGUgW1Jlc291cmNlc10oL3Jlc291cmNlLyk6CgoqIFtJbnN0YWxsaW5nIFIvUlN0dWRpb10oL3Jlc291cmNlL2luc3RhbGwvKQoqIFtJbnN0YWxsaW5nIGByZXRoaW5raW5nYCBwYWNrYWdlXSgvcmVzb3VyY2UvaW5zdGFsbC1yZXRoaW5raW5nLykKKiBbUm1hcmtkb3duXSgvcmVzb3VyY2Uvcm1hcmtkb3duLykKKiBbUiBTdHlsZSBndWlkZV0oL3Jlc291cmNlL3N0eWxlLykKCiMjIENoYXB0ZXIgNCAKCiMjIyBEcmF3aW5nIHRoZSBPd2wKCiFbXSguLi8uLi9pbWcvc3lsbGFidXMvb3dsLnBuZykKCgojIyMgMS4gUXVlc3Rpb24gb3IgZXN0aW1hbmQKCk9iamVjdGl2ZTogRGVzY3JpYmUgdGhlIGFzc29jaWF0aW9uIGJldHdlZW4gQWR1bHQgKip3ZWlnaHQqKiBhbmQgKipoZWlnaHQqKgoKYGBge3J9CmxpYnJhcnkocmV0aGlua2luZykKZGF0YShIb3dlbGwxKQpkIDwtIEhvd2VsbDFbSG93ZWxsMSRhZ2U+PTE4LF0KCnBsb3QoZCRoZWlnaHQsIGQkd2VpZ2h0LCBjb2wgPSAyLCAgeGxhYiA9ICJoZWlnaHQgKGNtKSIsIHlsYWIgPSAid2VpZ2h0IChrZykiLCAgbHdkPTMpCmBgYAoKIyMjIDIuIFNjaWVudGlmaWMgbW9kZWwKCldlaWdodCBpcyBhIGZ1bmN0aW9uIG9mIGhlaWdodC4gCgpgYGB7ciBmaWcuaGVpZ2h0PTEsIGZpZy53aWR0aD0yfQpsaWJyYXJ5KGRhZ2l0dHkpCgpnIDwtIGRhZ2l0dHkoJ2RhZyB7CiAgICBIIFtwb3M9IjAsMSJdCiAgICBXIFtwb3M9IjEsMSJdCiAgICAKICAgIEggLT4gVwp9JykKcGxvdChnKQpgYGAKCiMjIyAzLiBTdGF0aXN0aWNhbCBtb2RlbAoKIyMjIyBHZW5lcmF0aXZlIE1vZGVsCgpMZXQncyBjb25zaWRlciB0aGUgR2VuZXJhdGl2ZSBNb2RlbCAoSCAtPiBXKSBmcm9tIHRoZSBsZWN0dXJlOgoKJFdfaSBcc2ltIE5vcm1hbChcbXVfaSxcc2lnbWEpJDxicj4KJFxtdV9pID0gXGFscGhhICsgXGJldGEgSF9pJDxicj4KCkxldCdzIG5vdyBjb25kdWN0IGEgcHJpb3IgcHJlZGljdGl2ZSBzaW11bGF0aW9uIHRvIHNpbXVsYXRlICJzeW50aGV0aWMgaW5kaXZpZHVhbHMiLgoKYGBge3J9CnNldC5zZWVkKDE3KQojIGZvcndhcmQgc2ltdWxhdGlvbiBhcyB3ZSBjaG9vc2UgdGhlc2UgcGFyYW1ldGVycwphbHBoYSA8LSAwICMgaW1wbGllcyB6ZXJvIGhlaWdodCA9PiB6ZXJvIHdlaWdodApiZXRhIDwtIDAuNSAKc2lnbWEgPC0gNQpuX2luZGl2aWR1YWxzIDwtIDEwMAoKSCA8LSBydW5pZihuX2luZGl2aWR1YWxzLDEzMCwxNzApICMgaGVpZ2h0cywgdW5pZm9ybSBiZXR3ZWVuIDEzMCAtIDE3MCBjbQptdSA8LSBhbHBoYSArIGJldGEqSApXIDwtIHJub3JtKG5faW5kaXZpZHVhbHMsbXUsc2lnbWEpICMgc2FtcGxlIGZyb20gTm9ybWFsCmBgYAoKYGBge3J9CmNvbDIgPC0gY29sLmFscGhhKDIsMC44KQpwbG90KFcsSCwgIGNvbD1jb2wyLCBsd2Q9MywKICAgICBjZXg9MS4yLCAgeGxhYiA9ICJ3ZWlnaHQgKGtnKSIsIHlsYWIgPSAiaGVpZ2h0IChjbSkiKQptdGV4dCggIjEwMCBTaW11bGF0ZWQgcGVvcGxlIiApCmBgYAoKIyMjIyBTYW1wbGluZyB0aGUgcHJpb3IgZGlzdHJpYnV0aW9uCgpgYGB7cn0Kbl9zYW1wbGVzIDwtIDEwCgphbHBoYSA8LSBybm9ybShuX3NhbXBsZXMsMCwxKQpiZXRhIDwtIHJub3JtKG5fc2FtcGxlcywwLDEpCgpwbG90KE5VTEwseGxpbT1jKC0yLDIpLHlsaW09YygtMiwyKSx4bGFiPSJ4Iix5bGFiPSJ5IikKZm9yIChpIGluIDE6bl9zYW1wbGVzKXsKICBhYmxpbmUoYWxwaGFbaV0sYmV0YVtpXSxsd2Q9NCxjb2w9MikKfQpgYGAKT3VyIGxpbmVzIGFyZSBzaW1pbGFyIGJ1dCBzbGlnaHRseSBkaWZmZXJlbnQgdGhhbiB3aGF0IHdhcyBzaG93bgoKIyMjIDMuIFN0YXRpc3RpY2FsIG1vZGVsIGZvciBILT5XCgpgYGB7cn0KbiA8LSAxMAphbHBoYSA8LSBybm9ybShuLDYwLDEwKQpiZXRhIDwtIHJub3JtKG4sMCwxMCkKCkhiYXIgPC0gMTUwCkhzZXEgPC0gc2VxKGZyb209MTMwLHRvPTE3MCxsZW49MzApCnBsb3QoTlVMTCwgeGxpbT1jKDEzMCwxNzApLCB5bGltPWMoMTAsMTAwKSwKICAgICB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKCmZvciAoaSBpbiAxOm4pewogIGxpbmVzKEhzZXEsIGFscGhhW2ldICsgYmV0YVtpXSooSHNlcS1IYmFyKSxsd2Q9Myxjb2w9MikKfQpgYGAKCklzIHRoaXMgYSBnb29kIHByaW9yIHRvIGJlIHVzZWQ/IFdoeSBvciB3aHkgbm90IGFyZSB0aGV5IGludGVycHJldGFibGU/CgpSZW1lbWJlciwgYSBsb2dub3JtYWwgZGlzdHJpYnV0aW9uIGlzIGEgZGlzdHJpYnV0aW9uIHRoYXQgaWYgeW91IHRha2UgdGhlIGxvZ2FyaXRobSBvZiB0aGUgdmFsdWVzLCB0aGVuIGFsbCBvZiBpdCdzIHZhbHVlcyB3b3VsZCBiZSBub3JtYWwuIAoKYGBge3J9CiMgc2ltdWxhdGVkIGxvZ25vcm1hbApiIDwtIHJsbm9ybSgxZTQsIDAsIDEpICM0LjQwCmRlbnMoYiwgeGxpbT1jKDAsNSksIGFkaj0wLjEpCmBgYAoKTGV0J3MgZG8gYSBwcmVkaWN0aXZlIHNpbXVsYXRpb24gbm93IHVzaW5nIHRoZSBMb2ctTm9ybWFsIHByaW9yLgoKYGBge3J9CnNldC5zZWVkKDI5NzEpCm4gPC0gMTAKYSA8LSBybm9ybSggbiAsIDYwICwgNSApCmIgPC0gcmxub3JtKCBuICwgMCAsIDEgKQoKcGxvdChOVUxMLCB5bGltPWMoMTMwLDE3MCksIHhsaW09YygxMCwxMDApLAogICAgIHlsYWI9ImhlaWdodCAoY20pIiwgeGxhYj0id2VpZ2h0IChrZykiKQoKZm9yIChpIGluIDE6bil7CiAgbGluZXMoYVtpXSArIGJbaV0qKEhzZXEtSGJhciksSHNlcSwgbHdkPTMsY29sPTIpCn0KYGBgCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpLZXkgaXMganVzdGlmeSBwcmlvcnMgd2l0aCBpbmZvcm1hdGlvbiBvdXRzaWRlIG9mIHRoZSBkYXRhICh0aGF0IHdpbGwgYmUgbW9kZWxlZCkuIFRoaXMgaXMgc2ltaWxhciB0byBtYWNoaW5lIGxlYXJuaW5nIHdoZXJlIHdlIGRvbid0IHdhbnQgdG8gaW5jbHVkZSByZWNvcmRzIGluIG91ciB0ZXN0IGRhdGFzZXQgdGhhdCB3ZXJlIGFsc28gaW4gb3VyIHRyYWluaW5nLiBVc2luZyBtb2RlbGVkIGRhdGEgdG8gZm9ybSBwcmlvcnMgY2FuIGJlIHRob3VnaHQgb2YgYXMgInByaW9yLWhhY2tpbmciLiBUeXBpY2FsbHkgaW4gbGl0ZXJhdHVyZSwgQmF5ZXNpYW4gYXBwcm9hY2hlcyByZXF1aXJlIHByZS1yZWdpc3RyYXRpb24gd2hlbiB1c2luZyBpbmZvcm1hdGl2ZSBwcmlvcnMgKHNlZSBbdGhpcyBleGFtcGxlIGZyb20gRmVybmFuZGVzIGV0IGFsLiwgMjAxOF0oaHR0cHM6Ly9naXRodWIuY29tL21pY2hhZWwtZmVybmFuZGVzL3VuY2VydGFpbnR5LWRpc3BsYXlzLWZvci10cmFuc2l0L2Jsb2IvbWFzdGVyL3ByZS1yZWdpc3RyYXRpb24ucGRmKSkuCgp7eyUgL2NhbGxvdXQgJX19CgokV19pIFxzaW0gTm9ybWFsKFxtdV9pLFxzaWdtYSkkPGJyPgokXG11X2kgPSBcYWxwaGEgKyBcYmV0YShIX2kgLSBcb3ZlcmxpbmV7SH0pJDxicj4KJFxhbHBoYSBcc2ltIE5vcm1hbCg2MCwxMCkkPGJyPgokXGJldGEgXHNpbSBMb2dOb3JtYWwoMCwxKSQ8YnI+CiRcc2lnbWEgXHNpbSBVbmlmb3JtKDAsMTApJDxicj4KCmBgYHtyfQojIGRlZmluZSB0aGUgYXZlcmFnZSB3ZWlnaHQsIHgtYmFyCnhiYXIgPC0gbWVhbihkJHdlaWdodCkKCiMgZml0IG1vZGVsCm00LjMgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIGhlaWdodCB+IGRub3JtKCBtdSAsIHNpZ21hICkgLAogICAgICAgIG11IDwtIGEgKyBiKiggd2VpZ2h0IC0geGJhciApICwKICAgICAgICBhIH4gZG5vcm0oIDE3OCAsIDIwICkgLAogICAgICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgICAgIHNpZ21hIH4gZHVuaWYoIDAgLCA1MCApCiAgICApICwgZGF0YT1kICkKCiMjIFIgY29kZSA0LjQ0CnByZWNpcyggbTQuMyApCmBgYAoKVGhlIGZpcnN0IHJvdyBnaXZlcyB0aGUgcXVhZHJhdGljIGFwcHJveGltYXRpb24gZm9yIM6xLCB0aGUgc2Vjb25kIHRoZSBhcHByb3hpbWF0aW9uIGZvciDOsiwgYW5kIHRoZSB0aGlyZCBhcHByb3hpbWF0aW9uIGZvciDPgy4KCkxldOKAmXMgZm9jdXMgb24gYiAozrIpLiBTaW5jZSDOsiBpcyBhIHNsb3BlLCB0aGUgdmFsdWUgMC45MCBjYW4gYmUgcmVhZCBhcyBhICoqcGVyc29uIDEga2cgaGVhdmllciBpcyBleHBlY3RlZCB0byBiZSAwLjkwIGNtIHRhbGxlci4qKiA4OSUgb2YgdGhlIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBsaWVzIGJldHdlZW4gMC44NCBhbmQgMC45Ny4gVGhhdCBzdWdnZXN0cyB0aGF0ICoqzrIgdmFsdWVzIGNsb3NlIHRvIHplcm8gb3IgZ3JlYXRseSBhYm92ZSBvbmUgYXJlIGhpZ2hseSBpbmNvbXBhdGlibGUgd2l0aCB0aGVzZSBkYXRhIGFuZCB0aGlzIG1vZGVsLioqIEl0IGlzIG1vc3QgY2VydGFpbmx5IG5vdCBldmlkZW5jZSB0aGF0IHRoZSByZWxhdGlvbnNoaXAgYmV0d2VlbiB3ZWlnaHQgYW5kIGhlaWdodCBpcyBsaW5lYXIsIGJlY2F1c2UgdGhlIG1vZGVsIG9ubHkgY29uc2lkZXJlZCBsaW5lcy4gSXQganVzdCBzYXlzIHRoYXQsIGlmIHlvdSBhcmUgY29tbWl0dGVkIHRvIGEgbGluZSwgdGhlbiBsaW5lcyB3aXRoIGEgc2xvcGUgYXJvdW5kIDAuOSBhcmUgcGxhdXNpYmxlIG9uZXMuCgpgYGB7cn0KIyMgUiBjb2RlIDQuNDUKcm91bmQoIHZjb3YoIG00LjMgKSAsIDMgKQpgYGAKCmBgYHtyfQojIHNob3dzIGJvdGggdGhlIG1hcmdpbmFsIHBvc3RlcmlvcnMgYW5kIHRoZSBjb3ZhcmlhbmNlLgpwYWlycyhtNC4zKQpgYGAKClRoZXJlIGlzIGxpdHRsZSBjb3ZhcmlhdGlvbiBhbW9uZyB0aGUgcGFyYW1ldGVycyBpbiB0aGlzIGNhc2UuIFRoZSBsYWNrIG9mIGNvdmFyaWFuY2UgYW1vbmcgdGhlIHBhcmFtZXRlcnMgKipyZXN1bHRzIGZyb20gY2VudGVyaW5nLioqCgpBcyBhbiBleGVyY2lzZSwgY29uc2lkZXIgcmVydW5uaW5nIHRoZSByZWdyZXNzaW9uIGFib3ZlIHdpdGhvdXQgY2VudGVyaW5nIGFuZCBjb21wYXJlIHRoZSBjb3ZhcmlhdGlvbi4KCiMjIyA0LiBWYWxpZGF0ZSBtb2RlbAoKV2UnbGwgdXNlIGEgKipzaW11bGF0aW9uLWJhc2VkKiogdmFsaWRhdGlvbi4KCldlJ2xsIGZpcnN0IHZhbGlkYXRlIHdpdGggYSBzaW11bGF0aW9uIChha2EgZmFrZSBkYXRhKS4KCmBgYHtyfQphbHBoYSA8LSA3MApiZXRhIDwtIDAuNQpzaWdtYSA8LSA1Cm5faW5kaXZpZHVhbHMgPC0gMTAwCgojIHNpbXVsYXRpb24KSCA8LSBydW5pZihuX2luZGl2aWR1YWxzLDEzMCwxNzApCm11IDwtIGFscGhhICsgYmV0YSooSC1tZWFuKEgpKQpXIDwtIHJub3JtKG5faW5kaXZpZHVhbHMsbXUsc2lnbWEpCgpkYXQgPC0gbGlzdChIPUgsVz1XLEhiYXI9bWVhbihIKSkKCm1fdmFsaWRhdGUgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgICAgICBtdSA8LSBhICsgYiooIEggLSBIYmFyICksCiAgICAgICAgYSB+IGRub3JtKCA2MCAsIDEwICkgLAogICAgICAgIGIgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgICAgIHNpZ21hIH4gZHVuaWYoIDAgLCAxMCApCiAgICApICwgZGF0YT1kYXQgKQoKcHJlY2lzKG1fdmFsaWRhdGUpCmBgYAoKYGBge3J9CmRhdCA8LSBsaXN0KFcgPSBkJHdlaWdodCwgSCA9IGQkaGVpZ2h0LCBIYmFyID0gbWVhbihkJGhlaWdodCkpCgptX2FkdWx0cyA8LSBxdWFwKAogICAgYWxpc3QoCiAgICAgICAgVyB+IGRub3JtKCBtdSAsIHNpZ21hICkgLAogICAgICAgIG11IDwtIGEgKyBiKiggSCAtIEhiYXIgKSwKICAgICAgICBhIH4gZG5vcm0oIDYwICwgMTAgKSAsCiAgICAgICAgYiB+IGRsbm9ybSggMCAsIDEgKSAsCiAgICAgICAgc2lnbWEgfiBkdW5pZiggMCAsIDEwICkKICAgICkgLCBkYXRhPWRhdCApCgpwcmVjaXMobV9hZHVsdHMpCmBgYAoKe3slIGNhbGxvdXQgbm90ZSAlfX0KCkZpcnN0IExhdyBvZiBTdGF0aXN0aWNhbCBJbnRlcnByZXRhdGlvbjogVGhlICoqcGFyYW1ldGVycyBhcmUgbm90IGluZGVwZW5kZW50Kiogb2Ygb25lIGFub3RoZXIgYW5kIGNhbm5vdCBhbHdheXMgYmUgaW5kZXBlbmRlbnRseSBpbnRlcnByZXRlZC4KCkluc3RlYWQsIGRyYXcgKHB1c2ggb3V0KSAqKnBvc3RlcmlvciBwcmVkaWN0aW9ucyoqIGFuZCBkZXNjcmliZS9pbnRlcnByZXQgdGhlbS4KCnt7JSAvY2FsbG91dCAlfX0KCmBgYHtyfQpwb3N0IDwtIGV4dHJhY3Quc2FtcGxlcyhtX2FkdWx0cykKaGVhZChwb3N0KQpgYGAKCiMjIyMgMS4gUGxvdCB0aGUgc2FtcGxlCgpgYGB7cn0KIyA0LjQuMwpjb2wyIDwtIGNvbC5hbHBoYSgyLDAuOCkKcGxvdChkJGhlaWdodCwgZCR3ZWlnaHQsIGNvbD1jb2wyLCBsd2Q9MywKICAgICBjZXg9MS4yLCB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKYGBgCgojIyMjIDIuIFBsb3QgdGhlIHBvc3RlcmlvciBtZWFuCgoKYGBge3J9CiMgZ2V0IHBvc3RlcmlvciBtZWFuIHZpYSBsaW5rIGZ1bmN0aW9uCnhzZXEgPC0gc2VxKGZyb209MTMwLHRvPTE5MCxsZW49NTApCm11IDwtIGxpbmsobV9hZHVsdHMsZGF0YT1saXN0KCBIPXhzZXEsSGJhcj1tZWFuKGQkaGVpZ2h0KSkpCm11Lm1lYW4gPC0gYXBwbHkoIG11ICwgMiAsIG1lYW4gKQoKIyBwbG90IHNhbWUgd2l0aCBsaW5lcyBmb3IgbXUubWVhbgpwbG90KGQkaGVpZ2h0LCBkJHdlaWdodCwgY29sPWNvbDIsIGx3ZD0zLAogICAgIGNleD0xLjIsIHhsYWI9ImhlaWdodCAoY20pIiwgeWxhYj0id2VpZ2h0IChrZykiKQpsaW5lcyh4c2VxLCBtdS5tZWFuLCBsd2Q9NCkKYGBgCgojIyMjIDMuIFBsb3QgdW5jZXJ0YWludHkgb2YgdGhlIG1lYW4KCmBgYHtyfQojIGdldCBQSSBmb3IgbXUKbXUuUEkgPC0gYXBwbHkoIG11ICwgMiAsIFBJICwgcHJvYj0wLjg5ICkKCiMgcmVwbG90IHNhbWUgYXMgMgpwbG90KGQkaGVpZ2h0LCBkJHdlaWdodCwgY29sPWNvbDIgLCBsd2Q9MywKICAgICBjZXg9MS4yLCB4bGFiPSJoZWlnaHQgKGNtKSIsIHlsYWI9IndlaWdodCAoa2cpIikKbGluZXMoIHhzZXEgLCBtdS5tZWFuICkKIyBhZGQgcGxvdCBhIHNoYWRlZCByZWdpb24gZm9yIDg5JSBQSQpzaGFkZSggbXUuUEkgLCB4c2VxICkKCiNhbHRlcm5hdGl2ZSB3YXkgdG8gcGxvdCB1bmNlcnRhaW50eSBvZiB0aGUgbWVhbgojZm9yICggaSBpbiAxOjEwMCApCiMgICAgbGluZXMoIHhzZXEgLCBtdVtpLF0gLCBwY2g9MTYsIGNvbD1jb2wuYWxwaGEocmFuZ2kyLDAuMSkgKQpgYGAKCiMjIyMgNC4gUGxvdCB1bmNlcnRhaW50eSBvZiBwcmVkaWN0aW9ucwoKYGBge3J9CiMgc2ltdWxhdGUgaHlwb3RoZXRpY2FsIGFkZGp1c3RzIGdpdmVuIHhzZXEKc2ltLmhlaWdodCA8LSBzaW0oIG1fYWR1bHRzICwgZGF0YT1saXN0KEg9eHNlcSxIYmFyPW1lYW4oZCRoZWlnaHQpKSkgCmhlaWdodC5QSSA8LSBhcHBseSggc2ltLmhlaWdodCAsIDIgLCBQSSAsIHByb2I9MC44OSApCgojIHJlcGxvdCAzCnBsb3QoIGQkaGVpZ2h0LGQkd2VpZ2h0LCBjb2w9Y29sMiAsIGx3ZD0zLAogICAgIGNleD0xLjIsIHhsYWI9ImhlaWdodCAoY20pIiwgeWxhYj0id2VpZ2h0IChrZykiKQpsaW5lcyggeHNlcSwgbXUubWVhbikKc2hhZGUoIG11LlBJICwgeHNlcSApCgojIGFkZCBpbiBQSSByZWdpb24gZm9yIHNpbXVsYXRlZCBoZWlnaHRzCnNoYWRlKCBoZWlnaHQuUEkgLCB4c2VxICkKYGBgCgojIyMgRXhhbXBsZQoKTGV0J3Mgc2F5IHdlIHdhbnQgdG8gc2ltdWxhdGUgdXNpbmcgdGhlIGBtX2FkdWx0c2AgbW9kZWwgZm91ciBpbmRpdmlkdWFscywgZWFjaCB3aXRoIGhlaWdodHMgMTQwLCAxNTAsIDE3MCwgYW5kIDE5MC4KCkNhbGN1bGF0ZSB0aGUgc2ltdWxhdGVkIG1lYW4gd2VpZ2h0cyBhbmQgODklIHBlcmNlbnRpbGUgaW50ZXJ2YWxzIGZvciB0aGVzZSBmb3VyIGluZGl2aWR1YWxzLgoKYGBge3J9CnNldC5zZWVkKDEwMCkKc2FtcGxlX2hlaWdodHMgPSBjKDEzNSwxNTAsMTcwLDE5MCkKCnNpbXVsX3dlaWdodHMgPC0gc2ltKCBtX2FkdWx0cyAsIGRhdGE9bGlzdChIPXNhbXBsZV9oZWlnaHRzLEhiYXI9bWVhbihkJGhlaWdodCkpKSAKCiMgc2ltdWxhdGVkIG1lYW5zCm1lYW5fd2VpZ2h0cyA9IGFwcGx5KCBzaW11bF93ZWlnaHRzICwgMiAsIG1lYW4gKQptZWFuX3dlaWdodHMKYGBgCgpgYGB7cn0KIyBzaW11bGF0ZWQgUEkncwpwaV93ZWlnaHRzID0gYXBwbHkoIHNpbXVsX3dlaWdodHMgLCAyICwgUEkgLCBwcm9iPTAuODkgKQpwaV93ZWlnaHRzCmBgYAoKYGBge3J9CmRhdGEuZnJhbWUoCiAgc2FtcGxlX2hlaWdodHMgPSBzYW1wbGVfaGVpZ2h0cywKICBzaW11bGF0ZWRfbWVhbiA9IG1lYW5fd2VpZ2h0cywKICBsb3dfcGkgPSBwaV93ZWlnaHRzWzEsXSwKICBoaWdoX3BpID0gcGlfd2VpZ2h0c1syLF0KKQpgYGAKCgojIyBQYWNrYWdlIHZlcnNpb25zCgpgYGB7cn0Kc2Vzc2lvbkluZm8oKQpgYGA=" download="03-class.Rmarkdown">
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
Our lines are similar but slightly different than what was shown

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
# simulate hypothetical addjusts given xseq
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
