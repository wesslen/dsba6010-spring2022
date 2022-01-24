---
date: "2022-01-23"
title: "Class 2"
menu:
  example:
    parent: Examples
weight: 2
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:application/octet-stream;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyAyIgptZW51OgogIGV4YW1wbGU6CiAgICBwYXJlbnQ6IEV4YW1wbGVzCndlaWdodDogMgp0eXBlOiBkb2NzCnRvYzogdHJ1ZQotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIGNsYXNzLnNvdXJjZT0ibGFuZ3VhZ2UtciIsIGNsYXNzLm91dHB1dD0ibGFuZ3VhZ2UtciIsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQp4YXJpbmdhbkV4dHJhOjp1c2VfY2xpcGJvYXJkKCkKbGlicmFyeShyZXRoaW5raW5nKQpgYGAKCmBgYHtyIGVjaG89RkFMU0V9CmRvd25sb2FkdGhpczo6ZG93bmxvYWRfZmlsZSgKICBwYXRoID0gIjAyLWNsYXNzLlJtYXJrZG93biIsCiAgb3V0cHV0X25hbWUgPSAiMDItY2xhc3MiLAogIGJ1dHRvbl9sYWJlbCA9ICJEb3dubG9hZCB0aGlzIGNvZGUiLAogIGJ1dHRvbl90eXBlID0gImRhbmdlciIsCiAgaGFzX2ljb24gPSBUUlVFLAogIGljb24gPSAiZmEgZmEtc2F2ZSIsCiAgc2VsZl9jb250YWluZWQgPSBGQUxTRQopCmBgYApgYGB7ciBlY2hvPUZBTFNFLCBmaWcuYWxpZ249ImxlZnQiLCBmaWcubGluaz0naHR0cHM6Ly9naXRwb2QuaW8vI2h0dHBzOi8vZ2l0aHViLmNvbS93ZXNzbGVuL2RzYmE2MDEwLWV4YW1wbGVzJ30Ka25pdHI6OmluY2x1ZGVfZ3JhcGhpY3MocGF0aD0iaHR0cHM6Ly9naXRwb2QuaW8vYnV0dG9uL29wZW4taW4tZ2l0cG9kLnN2ZyIpCmBgYAoKIyMgSW50cm9kdWN0aW9uCgpGb3IgdGhpcyBjbGFzcywgd2UnbGwgcmV2aWV3IGNvZGUgZXhhbXBsZXMgZm91bmQgaW4gQ2hhcHRlciAyLgoKVGhpcyBhc3N1bWVzIHRoYXQgeW91IGhhdmUgYWxyZWFkeSBpbnN0YWxsZWQgdGhlIGByZXRoaW5raW5nYCBwYWNrYWdlLgoKSWYgeW91IG5lZWQgaGVscCwgYmUgc3VyZSB0byByZW1lbWJlciB0aGUgcmVmZXJlbmNlcyBpbiB0aGUgW1Jlc291cmNlc10oL3Jlc291cmNlLyk6CgoqIFtJbnN0YWxsaW5nIFIvUlN0dWRpb10oL3Jlc291cmNlL2luc3RhbGwvKQoqIFtJbnN0YWxsaW5nIGByZXRoaW5raW5nYCBwYWNrYWdlXSgvcmVzb3VyY2UvaW5zdGFsbC1yZXRoaW5raW5nLykKKiBbUm1hcmtkb3duXSgvcmVzb3VyY2Uvcm1hcmtkb3duLykKKiBbUiBTdHlsZSBndWlkZV0oL3Jlc291cmNlL3N0eWxlLykKCiMjIENoYXB0ZXIgMgoKIyMjIEJheWVzaWFuIFVwZGF0aW5nOiBHcmlkIEFwcHJveGltYXRpb24KCkxldCdzIGFzc3VtZSB3ZSBoYXZlIHRoZSB0YWJsZSBpbiAyLjEKCmBgYHtyfQojIyBSIGNvZGUgMi4xCndheXMgPC0gYyggMCAsIDMgLCA4ICwgOSAsIDAgKQp3YXlzL3N1bSh3YXlzKQpgYGAKCkxldCdzIGNvbXB1dGUgdGhlIGxpa2VsaWhvb2Qgb2Ygc2l4IFcncyBpbiBuaW5lIHRvc3NlcyAoYXNzdW1pbmcgYSA1MCUgcHJvYmFiaWxpdHkpOgoKYGBge3J9CiMjIFIgY29kZSAyLjIKZGJpbm9tKCA2ICwgc2l6ZT05ICwgcHJvYj0wLjUgKQpgYGAKCldlIGNhbiBzZWUgaXQncyAxNi40JS4KCkJlIHN1cmUgdG8gZXhhbWluZSB0aGUgYGRiaW5vbWAgZnVuY3Rpb24gYnkgdHlwaW5nIGluIGA/ZGJpbm9tYCBhbmQgZXhwbG9yaW5nIHRoZSBkb2N1bWVudGF0aW9uLiBXZSdsbCB1c2UgdGhpcyBmdW5jdGlvbiBhIGxvdCBpbiB0aGlzIGNsYXNzLgoKTmV4dCwgbGV0J3MgZGVmaW5lIGEgZ3JpZC4gVGhpcyBpcyByZXF1aXJlZCB3aGVuIHdlIGFyZSB1c2luZyBHcmlkIEFwcHJveGltYXRpb24gZm9yIG91ciBCYXllc2lhbiBjYWxjdWxhdGlvbnMgKGkuZS4sIHRvIGVzdGltYXRlIHRoZSBwb3N0ZXJpb3IpLgoKYGBge3J9CiMjIFIgY29kZSAyLjMKIyBkZWZpbmUgZ3JpZApwX2dyaWQgPC0gc2VxKCBmcm9tPTAgLCB0bz0xICwgbGVuZ3RoLm91dD0yMCApCgpwX2dyaWQKYGBgCgpOb3RpY2UgdGhhdCB0aGlzIGZ1bmN0aW9uIGNyZWF0ZXMgYSB2ZWN0b3Igd2l0aCBsZW5ndGggMjAgYW5kIHRoYXQgcmFuZ2VzIGZyb20gMCB0byAxLiBOb3RlIGFzIHdlbGwgdGhhdCBlYWNoIHZlY3RvciBlbGVtZW50IGlzIGV2ZW5seSBzcGFjZWQgaW4gaW5jcmVtZW50cyBvZiBgKHRvIC0gZnJvbSkvKGxlbmd0aC5vdXQgLSAxKWAuCgpUaGluayBhYm91dCB0aGUgdHJhZGUtb2ZmIGJldHdlZW4gaGF2aW5nIGEgc21hbGxlciBvciBsYXJnZXIgYGxlbmd0aC5vdXRgLgoKTmV4dCwgbGV0J3MgZGVmaW5lIG91ciBwcmlvci4gV2UnbGwgYXNzdW1lIGEgImZsYXQiIHByaW9yLiAKCmBgYHtyfQojIGRlZmluZSBwcmlvcgpwcmlvciA8LSByZXAoIDEgLCAyMCApCgpwbG90KHBfZ3JpZCwgcHJpb3IsIHR5cGU9ImIiLCB5bGltPWMoMCw1KSkKYGBgCgpgYGB7cn0KIyBjb21wdXRlIGxpa2VsaWhvb2QgYXQgZWFjaCB2YWx1ZSBpbiBncmlkCmxpa2VsaWhvb2QgPC0gZGJpbm9tKCA2ICwgc2l6ZT05ICwgcHJvYj1wX2dyaWQgKQoKcGxvdChwX2dyaWQsIGxpa2VsaWhvb2QsIHR5cGU9ImIiLCB5bGltPWMoMCwwLjMpKQpgYGAKCmBgYHtyfQojIGNvbXB1dGUgcHJvZHVjdCBvZiBsaWtlbGlob29kIGFuZCBwcmlvcgp1bnN0ZC5wb3N0ZXJpb3IgPC0gbGlrZWxpaG9vZCAqIHByaW9yCgpwbG90KHBfZ3JpZCwgdW5zdGQucG9zdGVyaW9yLCB0eXBlPSJiIiwgeWxpbT1jKDAsMC4zKSkKYGBgCgpgYGB7cn0KIyBzdGFuZGFyZGl6ZSB0aGUgcG9zdGVyaW9yLCBzbyBpdCBzdW1zIHRvIDEKcG9zdGVyaW9yIDwtIHVuc3RkLnBvc3RlcmlvciAvIHN1bSh1bnN0ZC5wb3N0ZXJpb3IpCgojIyBSIGNvZGUgMi40CnBsb3QoIHBfZ3JpZCAsIHBvc3RlcmlvciAsIHR5cGU9ImIiICwKICAgIHhsYWI9InByb2JhYmlsaXR5IG9mIHdhdGVyIiAsIHlsYWI9InBvc3RlcmlvciBwcm9iYWJpbGl0eSIgKQptdGV4dCggIjIwIHBvaW50cyIgKQpgYGAKCgoKe3slIGNhbGxvdXQgbm90ZSAlfX0KClByYWN0aWNlOiBXaGF0IGhhcHBlbnMgaWYgd2UgYWx0ZXIgdGhlIHByaW9ycz8gV2hhdCB3aWxsIGJlIHRoZSBuZXcgcG9zdGVyaW9ycz8KCkFzc3VtZSA2IFcncyBhbmQgMyBMJ3MgKDkgdG9zc2VzKS4gUGxvdCB0aGUgcG9zdGVyaW9yIGFuZCBjb21wYXJlIHRoZW0gdG8gdXNpbmcgYSB1bmlmb3JtIHByaW9yLgoKYGBge3J9CiMgcHJpb3IgMQpwcmlvciA8LSBpZmVsc2UoIHBfZ3JpZCA8IDAuNSAsIDAgLCAxICkKYGBgCgpgYGB7cn0KIyBwcmlvciAyCnByaW9yIDwtIGV4cCggLTUqYWJzKCBwX2dyaWQgLSAwLjUgKSApCmBgYAoKe3slIC9jYWxsb3V0ICV9fQoKIyMjIEJheWVzaWFuIFVwZGF0aW5nOiBRdWFkcmF0aWMgQXBwcm94aW1hdGlvbgoKV2UgY2FuIGFsc28gdXNlIHF1YWRyYXRpYyBhcHByb3hpbWF0aW9uLCB3aGljaCBpcyBkaXNjdXNzZWQgb24gcGFnZSA0MiBvZiBDaGFwdGVyMi4gV2UnbGwgdXNlIHF1YWRyYXRpYyBhcHByb3hpbWF0aW9uIGFwcHJvYWNoIG92ZXIgdGhlIG5leHQgZmV3IHdlZWtzIGJlZm9yZSBtb3ZpbmcgdG8gTUNNQyBtZXRob2RzIHZpYSBTdGFuLgoKYGBge3J9CiMjIFIgY29kZSAyLjYKbGlicmFyeShyZXRoaW5raW5nKQpnbG9iZS5xYSA8LSBxdWFwKAogICAgYWxpc3QoCiAgICAgICAgVyB+IGRiaW5vbSggVytMICxwKSAsICAjIGJpbm9taWFsIGxpa2VsaWhvb2QKICAgICAgICBwIH4gZHVuaWYoMCwxKSAgICAgIyB1bmlmb3JtIHByaW9yCiAgICApICwKICAgIGRhdGE9bGlzdChXPTYsTD0zKSApCgpnbG9iZS5xYQpgYGAKCldlIGNhbiBhbHNvIHVzZSB0aGUgYHByZWNpc2AgZnVuY3Rpb24gdG8gc3VtbWFyaXplIHBhcmFtZXRlciBlc3RpbWF0ZXMuIEkgcmVjb21tZW5kIHJ1bm5pbmcgYD9wcmVjaXNgIHRvIGxvb2sgdXAgcGFyYW1ldGVycyBhc3NvY2lhdGVkIHdpdGggdGhpcyBmdW5jdGlvbi4KCmBgYHtyfQojIGRpc3BsYXkgc3VtbWFyeSBvZiBxdWFkcmF0aWMgYXBwcm94aW1hdGlvbgpwcmVjaXMoIGdsb2JlLnFhICkKYGBgCgojIyMgSG93IGRvZXMgZ3JpZCBhcHByb3hpbWF0aW9uIGNvbXBhcmUgdG8gYW5hbHl0aWNhbCBwb3N0ZXJpb3IgY2FsY3VsYXRpb24/CgpgYGB7cn0KIyMgUiBjb2RlIDIuNwojIGFuYWx5dGljYWwgY2FsY3VsYXRpb24KVyA8LSA2CkwgPC0gMwpjdXJ2ZSggZGJldGEoIHggLCBXKzEgLCBMKzEgKSAsIGZyb209MCAsIHRvPTEgLCBjb2wgPSAxKSAjIHNvbGlkIGxpbmUKIyBxdWFkcmF0aWMgYXBwcm94aW1hdGlvbgpjdXJ2ZSggZG5vcm0oIHggLCAwLjY3ICwgMC4xNiApICwgbHR5PTIgLCBhZGQ9VFJVRSAsIGNvbCA9IDIpICMgZG90dGVkIGxpbmUKYGBgCgoKIyMjIERlbW8gUHJvYmxlbXMKCjJNMTogUmVjYWxsIHRoZSBnbG9iZSB0b3NzaW5nIG1vZGVsIGZyb20gdGhlIGNoYXB0ZXIuIENvbXB1dGUgYW5kIHBsb3QgdGhlIGdyaWQgYXBwcm94aW1hdGUgcG9zdGVyaW9yIGRpc3RyaWJ1dGlvbiBmb3IgZWFjaCBvZiB0aGUgZm9sbG93aW5nIHNldHMgb2Ygb2JzZXJ2YXRpb25zLiBJbiBlYWNoIGNhc2UsIGFzc3VtZSBhIHVuaWZvcm0gcHJpb3IgZm9yIHAuCgpgYGB7cn0KcF9ncmlkIDwtIHNlcSggZnJvbT0wICwgdG89MSAsIGxlbmd0aC5vdXQ9MTAwICkgIyBncmlkIGZyb20gMCB0byAxIHdpdGggbGVuZ3RoIDEwMApwcmlvciA8LSByZXAoMSwxMDApICMgdW5pZm9ybSBwcmlvcgoKIyBsaWtlbGlob29kIG9mIDMgd2F0ZXIgaW4gMyB0b3NzZXMKbGlrZWxpaG9vZCA8LSBkYmlub20oIDMgLCBzaXplPTMgLCBwcm9iPXBfZ3JpZCApCgpwb3N0ZXJpb3IgPC0gbGlrZWxpaG9vZCAqIHByaW9yCnBvc3RlcmlvciA8LSBwb3N0ZXJpb3IgLyBzdW0ocG9zdGVyaW9yKSAjIHN0YW5kYXJkaXplCgpwbG90KCBwb3N0ZXJpb3IgfiBwX2dyaWQgLCB0eXBlPSJsIiwgbWFpbiA9ICJXLCBXLCBXIikKYGBgCgpgYGB7cn0KIyBsaWtlbGlob29kIG9mIDMgd2F0ZXIgaW4gNCB0b3NzZXMKbGlrZWxpaG9vZCA8LSBkYmlub20oIDMgLCBzaXplPTQgLCBwcm9iPXBfZ3JpZCApCgpwb3N0ZXJpb3IgPC0gbGlrZWxpaG9vZCAqIHByaW9yCnBvc3RlcmlvciA8LSBwb3N0ZXJpb3IgLyBzdW0ocG9zdGVyaW9yKSAjIHN0YW5kYXJkaXplCgpwbG90KCBwb3N0ZXJpb3IgfiBwX2dyaWQgLCB0eXBlPSJsIiAsIG1haW4gPSAiVywgVywgVywgTCIpCmBgYAoKYGBge3J9CiMgbGlrZWxpaG9vZCBvZiA1IHdhdGVyIGluIDcgdG9zc2VzCmxpa2VsaWhvb2QgPC0gZGJpbm9tKCA1ICwgc2l6ZT03ICwgcHJvYj1wX2dyaWQgKQoKcG9zdGVyaW9yIDwtIGxpa2VsaWhvb2QgKiBwcmlvcgpwb3N0ZXJpb3IgPC0gcG9zdGVyaW9yIC8gc3VtKHBvc3RlcmlvcikgIyBzdGFuZGFyZGl6ZQoKcGxvdCggcG9zdGVyaW9yIH4gcF9ncmlkICwgdHlwZT0ibCIgLCBtYWluID0gIkwsIFcsIFcsIFcsIEwsIFcsIFcsIFciKQpgYGAKCiMjIENoYXB0ZXIgMwoKQXNzdW1lIHdlIGhhdmUgdGhlIGZvbGxvd2luZyBtb2RlbDoKCmBgYHtyfQpwX2dyaWQgPC0gc2VxKGZyb20gPSAwLCB0byA9IDEsIGxlbmd0aC5vdXQgPSAxMDAwKQpwcmlvciA8LSByZXAoMSwgMTAwMCkKbGlrZWxpaG9vZCA8LSBkYmlub20oNiwgc2l6ZSA9IDksIHByb2IgPSBwX2dyaWQpCnBvc3RlcmlvciA8LSBsaWtlbGlob29kICogcHJpb3IKcG9zdGVyaW9yIDwtIHBvc3RlcmlvciAvIHN1bShwb3N0ZXJpb3IpCgpzZXQuc2VlZCgxMDApICMgdmVyeSBpbXBvcnRhbnQgd2hlbiB1c2luZyByYW5kb21pemVkIGZ1bmN0aW9ucyAoZS5nLiwgc2FtcGxlKQpzYW1wbGVzIDwtIHNhbXBsZShwX2dyaWQsIHByb2IgPSBwb3N0ZXJpb3IsIHNpemUgPSAxZTQsIHJlcGxhY2UgPSBUUlVFKQpgYGAKCiMjIyBEZW1vIFByb2JsZW1zCgpMZXQncyBmb2xsb3cgd29yayBpbiBzZWN0aW9uIDMuMiB0byB1bmRlcnN0YW5kIGhvdyB0byBzdW1tYXJpemUgaW5mb3JtYXRpb24gZnJvbSB0aGUgcG9zdGVyaW9yLgoKM0UxOiBIb3cgbXVjaCBwb3N0ZXJpb3IgcHJvYmFiaWxpdHkgbGllcyAqKmJlbG93KiogcCA9IDAuMj8KCmBgYHtyfQptZWFuKHNhbXBsZXMgPCAwLjIpCmBgYAoKM0UyOiBIb3cgbXVjaCBwb3N0ZXJpb3IgcHJvYmFiaWxpdHkgbGllcyAqKmFib3ZlKiogcCA9IDAuOD8KCmBgYHtyfQptZWFuKHNhbXBsZXMgPiAwLjgpCmBgYAoKM0UzOiBIb3cgbXVjaCBwb3N0ZXJpb3IgcHJvYmFiaWxpdHkgbGllcyAqKmJldHdlZW4qKiBwID0gMC4yIGFuZCBwID0gMC44PwoKYGBge3J9CnN1bSggc2FtcGxlcyA+IDAuMiAmIHNhbXBsZXMgPCAwLjggKSAvIDFlNApgYGAKCjNFNDogMjAlIG9mIHRoZSBwb3N0ZXJpb3IgcHJvYmFiaWxpdHkgbGllcyAqKmJlbG93Kiogd2hpY2ggdmFsdWUgb2YgcD8KCmBgYHtyfQpxdWFudGlsZShzYW1wbGVzLCBwcm9icyA9IDAuMikKYGBgCgozRTU6IDIwJSBvZiB0aGUgcG9zdGVyaW9yIHByb2JhYmlsaXR5IGxpZXMgKiphYm92ZSoqIHdoaWNoIHZhbHVlIG9mIHA/CgpgYGB7cn0KcXVhbnRpbGUoc2FtcGxlcywgcHJvYnMgPSAwLjgpCmBgYAoKM0U2OiBXaGljaCB2YWx1ZXMgb2YgcCBjb250YWluIHRoZSBuYXJyb3dlc3QgaW50ZXJ2YWwgZXF1YWwgdG8gNjYlIG9mIHRoZSBwb3N0ZXJpb3IgcHJvYmFiaWxpdHk/CgpgYGB7cn0KSFBESShzYW1wbGVzLCBwcm9iID0gMC42NikKYGBgCgozRTc6IFdoaWNoIHZhbHVlcyBvZiBwIGNvbnRhaW4gNjYlIG9mIHRoZSBwb3N0ZXJpb3IgcHJvYmFiaWxpdHksIGFzc3VtaW5nIGVxdWFsIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBib3RoIGJlbG93IGFuZCBhYm92ZSB0aGUgaW50ZXJ2YWw/CgpgYGB7cn0KUEkoc2FtcGxlcywgcHJvYiA9IDAuNjYpCmBgYAoKe3slIGNhbGxvdXQgbm90ZSAlfX0KCkNvbXByZWhlbnNpb24gcXVlc3Rpb246IHVuZGVyIHdoYXQgY2lyY3Vtc3RhbmNlcyB3b3VsZCB0aGUgUEkgZGlmZmVyIGZyb20gdGhlIEhQREkgKGhvbGRpbmcgaW50ZXJ2YWwgdmFsdWUgdGhlIHNhbWUpPwoKe3slIC9jYWxsb3V0ICV9fQoKM00xOiBTdXBwb3NlIHRoZSBnbG9iZSB0b3NzaW5nIGRhdGEgaGFkIHR1cm5lZCBvdXQgdG8gYmUgOCB3YXRlciBpbiAxNSB0b3NzZXMuIENvbnN0cnVjdGUgdGhlIHBvc3RlcmlvciBkaXN0cmlidXRpb24sIHVzaW5nIGdyaWQgYXBwcm94aW1hdGlvbi4gVXNlIHRoZSBzYW1lIGZsYXQgcHJpb3IgYXMgYmVmb3JlLgoKYGBge3J9CnBfZ3JpZCA8LSBzZXEoZnJvbSA9IDAsIHRvID0gMSwgbGVuZ3RoLm91dCA9IDEwMDApCnByaW9yIDwtIHJlcCgxLCAxMDAwKQpsaWtlbGlob29kIDwtIGRiaW5vbSg4LCBzaXplID0gMTUsIHByb2IgPSBwX2dyaWQpCnBvc3RlcmlvciA8LSBsaWtlbGlob29kICogcHJpb3IKcG9zdGVyaW9yIDwtIHBvc3RlcmlvciAvIHN1bShwb3N0ZXJpb3IpCgpwbG90KHBvc3RlcmlvcikKYGBgCgozTTIuIERyYXcgMTAsMDAwIHNhbXBsZXMgZnJvbSB0aGUgZ3JpZCBhcHByb3hpbWF0aW9uIGZyb20gYWJvdmUuIFRoZW4gdXNlIHRoZSBzYW1wbGUgdG8gY2FsY3VsYXRlIHRoZSA5MCUgSFBESSBmb3IgcC4KCmBgYHtyfQpzYW1wbGVzIDwtIHNhbXBsZShwX2dyaWQsIHByb2IgPSBwb3N0ZXJpb3IsIHNpemUgPSAxZTQsIHJlcGxhY2UgPSBUUlVFKQoKSFBESShzYW1wbGVzLCBwcm9iID0gMC45KQpgYGAKCjNNMy4gQ29uc3RydWN0IGEgKipwb3N0ZXJpb3IgcHJlZGljdGl2ZSBjaGVjayoqIGZvciB0aGlzIG1vZGVsIGFuZCBkYXRhLiBUaGUgbWVhbnMgc2ltdWxhdGUgdGhlIGRpc3RyaWJ1dGlvbiBvZiBzYW1wbGVzLCBhdmVyYWdpbmcgb3ZlciB0aGUgcG9zdGVyaW9yIHVuY2VydGFpbnR5IGluIHAuIFdoYXQgaXMgdGhlIHByb2JhYmlsaXR5IG9mIG9ic2VydmluZyA4IHdhdGVyIGluIDE1IHRvc3Nlcz8KCmBgYHtyfQp3IDwtIHJiaW5vbSgxZTQsIHNpemUgPSAxNSwgcHJvYiA9IHNhbXBsZXMpCm1lYW4odyA9PSA4KQpgYGAKCjNNNDogVXNpbmcgdGhlIHBvc3RlcmlvciBkaXN0cmlidXRpb24gY29uc3RydWN0ZWQgZnJvbSB0aGUgbmV3ICg4LzE1KSBkYXRhLCBub3cgY2FsY3VsYXRlIHRoZSBwcm9iYWJpbGl0eSBvZiBvYnNlcnZpbmcgNiB3YXRlciBpbiA5IHRvc3Nlcy4KCmBgYHtyfQp3IDwtIHJiaW5vbSgxZTQsIHNpemUgPSA5LCBwcm9iID0gc2FtcGxlcykKbWVhbih3ID09IDYpCmBgYAoKe3slIGNhbGxvdXQgbm90ZSAlfX0KCk1vZGlmeSB0aGUgdmFsdWVzIHcgKDAgdG8gOSkgZm9yIHRoZSBzaXplID0gOSBleGFtcGxlIGluIDNNNC4gQ29tcGFyZSB0aGVzZSB2YWx1ZXMgdG8gRmlndXJlIDMuNi4KCnt7JSAvY2FsbG91dCAlfX0KCiMjIEFwcGVuZGl4OiBgdGlkeXZlcnNlYCBjb252ZXJzaW9uCgpTdGF0aXN0aWNhbCBSZXRoaW5raW5nIHVzZXMgYmFzZSBSIGZ1bmN0aW9ucy4gTW9yZSByZWNlbnRseSwgU29sb21hbiBLdXJ6IGhhcyBjcmVhdGVkIGEgW3RyYW5zbGF0aW9uIG9mIHRoZSBib29rJ3MgZnVuY3Rpb25zXShodHRwczovL2Jvb2tkb3duLm9yZy9jb250ZW50LzQ4NTcvKSBpbnRvIGB0aWR5dmVyc2VgIChhbmQgbGF0ZXIgYGJybXNgKSBjb2RlLiBUaGlzIGlzIG5vdCBuZWNlc3NhcnkgYnV0IGNvdWxkIGJlIGV4dHJlbWVseSBoZWxwZnVsIHRvIGNsYXNzbWF0ZXMgd2hvIGFyZSBmYW1pbGlhciB3aXRoIGB0aWR5dmVyc2VgIGFscmVhZHkuCgpGaXJzdCwgd2UnbGwgbmVlZCB0byBjYWxsIGB0aWR5dmVyc2VgLiBJZiB5b3UgZG8gbm90IGhhdmUgYHRpZHl2ZXJzZWAsIHlvdSdsbCBuZWVkIHRvIGluc3RhbGwgaXQuCgpgYGB7cn0KbGlicmFyeSh0aWR5dmVyc2UpCmBgYAoKRm9yIGV4YW1wbGUsIHdlIGNhbiB0cmFuc2xhdGUgMi4zIGNvZGUgdXNpbmcgcGlwZXMgKGAlPiVgKQoKYGBge3J9CmQgPC0gdGliYmxlKHBfZ3JpZCA9IHNlcShmcm9tID0gMCwgdG8gPSAxLCBsZW5ndGgub3V0ID0gMjApLCAgICAgICMgZGVmaW5lIGdyaWQKICAgICAgICAgICBwcmlvciAgPSAxKSAlPiUgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAjIGRlZmluZSBwcmlvcgogICAgbXV0YXRlKGxpa2VsaWhvb2QgPSBkYmlub20oNiwgc2l6ZSA9IDksIHByb2IgPSBwX2dyaWQpKSAlPiUgICMgY29tcHV0ZSBsaWtlbGlob29kIGF0IGVhY2ggdmFsdWUgaW4gZ3JpZAogICAgbXV0YXRlKHVuc3RkX3Bvc3RlcmlvciA9IGxpa2VsaWhvb2QgKiBwcmlvcikgJT4lICAgICAgICAgICAgICMgY29tcHV0ZSBwcm9kdWN0IG9mIGxpa2VsaWhvb2QgYW5kIHByaW9yCiAgICBtdXRhdGUocG9zdGVyaW9yID0gdW5zdGRfcG9zdGVyaW9yIC8gc3VtKHVuc3RkX3Bvc3RlcmlvcikpIAoKZApgYGAKV2l0aCB0aGlzIGNhbGN1bGF0ZWQsIHdlIGNhbiB0aGVuIHVzZSBgZ2dwbG90MmAsIHRoZSBzdGFwbGUgYGdncGxvdDJgIGRhdGEgdmlzdWFsaXphdGlvbiBwYWNrYWdlLCB0byBwbG90IG91ciBwb3N0ZXJpb3IuCgpgYGB7cn0KZCAlPiUgCiAgZ2dwbG90KGFlcyh4ID0gcF9ncmlkLCB5ID0gcG9zdGVyaW9yKSkgKwogIGdlb21fcG9pbnQoKSArCiAgZ2VvbV9saW5lKCkgKwogIGxhYnMoc3VidGl0bGUgPSAiMjAgcG9pbnRzIiwKICAgICAgIHggPSAicHJvYmFiaWxpdHkgb2Ygd2F0ZXIiLAogICAgICAgeSA9ICJwb3N0ZXJpb3IgcHJvYmFiaWxpdHkiKSArCiAgdGhlbWUocGFuZWwuZ3JpZCA9IGVsZW1lbnRfYmxhbmsoKSkKYGBgCgpGb3IgdGhpcyBjbGFzcywgd2UnbGwgb2NjYXNpb25hbGx5IHJlZmVyIHRvIFNvbG9tYW4ncyBndWlkZS4gCgojIyMgRGVtbyBQcm9ibGVtCgoyTTE6IFJlY2FsbCB0aGUgZ2xvYmUgdG9zc2luZyBtb2RlbCBmcm9tIHRoZSBjaGFwdGVyLiBDb21wdXRlIGFuZCBwbG90IHRoZSBncmlkIGFwcHJveGltYXRlIHBvc3RlcmlvciBkaXN0cmlidXRpb24gZm9yIGVhY2ggb2YgdGhlIGZvbGxvd2luZyBzZXRzIG9mIG9ic2VydmF0aW9ucy4gSW4gZWFjaCBjYXNlLCBhc3N1bWUgYSB1bmlmb3JtIHByaW9yIGZvciBwLgoKYGBge3J9CiMjIGJlIHN1cmUgdG8gaGF2ZSB0aWR5dmVyc2UgaW5zdGFsbGVkLCBpLmUuLCBpbnN0YWxsLnBhY2thZ2VzKCd0aWR5dmVyc2UnKQpsaWJyYXJ5KHRpZHl2ZXJzZSkKCmRpc3QgPC0gdGliYmxlKHBfZ3JpZCA9IHNlcShmcm9tID0gMCwgdG8gPSAxLCBsZW5ndGgub3V0ID0gMjApLAogICAgICAgICAgICAgICBwcmlvciA9IHJlcCgxLCB0aW1lcyA9IDIwKSkgJT4lCiAgbXV0YXRlKGxpa2VsaWhvb2RfMSA9IGRiaW5vbSgzLCBzaXplID0gMywgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfMiA9IGRiaW5vbSgzLCBzaXplID0gNCwgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfMyA9IGRiaW5vbSg1LCBzaXplID0gNywgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGFjcm9zcyhzdGFydHNfd2l0aCgibGlrZWxpaG9vZCIpLCB+IC54ICogcHJpb3IpLAogICAgICAgICBhY3Jvc3Moc3RhcnRzX3dpdGgoImxpa2VsaWhvb2QiKSwgfiAueCAvIHN1bSgueCkpKSAlPiUKICBwaXZvdF9sb25nZXIoY29scyA9IHN0YXJ0c193aXRoKCJsaWtlbGlob29kIiksIG5hbWVzX3RvID0gInBhdHRlcm4iLAogICAgICAgICAgICAgICB2YWx1ZXNfdG8gPSAicG9zdGVyaW9yIikgJT4lCiAgc2VwYXJhdGUocGF0dGVybiwgYyhOQSwgInBhdHRlcm4iKSwgc2VwID0gIl8iLCBjb252ZXJ0ID0gVFJVRSkgJT4lCiAgbXV0YXRlKG9icyA9IGNhc2Vfd2hlbihwYXR0ZXJuID09IDFMIH4gIlcsIFcsIFciLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSAyTCB+ICJXLCBXLCBXLCBMIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gM0wgfiAiTCwgVywgVywgTCwgVywgVywgVyIpKQoKZ2dwbG90KGRpc3QsIGFlcyh4ID0gcF9ncmlkLCB5ID0gcG9zdGVyaW9yKSkgKwogIGZhY2V0X3dyYXAodmFycyhmY3RfaW5vcmRlcihvYnMpKSwgbnJvdyA9IDEpICsKICBnZW9tX2xpbmUoKSArCiAgZ2VvbV9wb2ludCgpICsKICBsYWJzKHggPSAiUHJvcG9ydGlvbiBXYXRlciAocCkiLCB5ID0gIlBvc3RlcmlvciBEZW5zaXR5IikKYGBgCgpgYGB7cn0KIyBXLCBXLCBXLCBMLCBXLCBXLCBXCgojIGNoYWxsZW5nZTogZnVuY3Rpb25hbGl6ZSB0aGlzIHRvIGdlbmVyYWxpemUgdGhpcyBmb3IgYW55IHJlYWQgaW4gdG9zcyBzdHJpbmcgCgpkMm0xIDwtIHRpYmJsZShwX2dyaWQgPSBzZXEoZnJvbSA9IDAsIHRvID0gMSwgbGVuZ3RoLm91dCA9IDIwKSwKICAgICAgICAgICAgICAgcHJpb3IgPSByZXAoMSwgdGltZXMgPSAyMCkpICU+JQogIG11dGF0ZSgKICAgICAgICAgbGlrZWxpaG9vZF8xID0gZGJpbm9tKDEsIHNpemUgPSAxLCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF8yID0gZGJpbm9tKDIsIHNpemUgPSAyLCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF8zID0gZGJpbm9tKDMsIHNpemUgPSAzLCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF80ID0gZGJpbm9tKDMsIHNpemUgPSA0LCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF81ID0gZGJpbm9tKDQsIHNpemUgPSA1LCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF82ID0gZGJpbm9tKDUsIHNpemUgPSA2LCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF83ID0gZGJpbm9tKDYsIHNpemUgPSA3LCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgYWNyb3NzKHN0YXJ0c193aXRoKCJsaWtlbGlob29kIiksIH4gLnggKiBwcmlvciksCiAgICAgICAgIGFjcm9zcyhzdGFydHNfd2l0aCgibGlrZWxpaG9vZCIpLCB+IC54IC8gc3VtKC54KSkpICU+JQogIHBpdm90X2xvbmdlcihjb2xzID0gc3RhcnRzX3dpdGgoImxpa2VsaWhvb2QiKSwgbmFtZXNfdG8gPSAicGF0dGVybiIsCiAgICAgICAgICAgICAgIHZhbHVlc190byA9ICJwb3N0ZXJpb3IiKSAlPiUKICBzZXBhcmF0ZShwYXR0ZXJuLCBjKE5BLCAicGF0dGVybiIpLCBzZXAgPSAiXyIsIGNvbnZlcnQgPSBUUlVFKSAlPiUKICBtdXRhdGUob2JzID0gY2FzZV93aGVuKHBhdHRlcm4gPT0gMUwgfiAiVyIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDJMIH4gIlcsIFciLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSAzTCB+ICJXLCBXLCBXLCIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDRMIH4gIlcsIFcsIFcsIEwiLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSA1TCB+ICJXLCBXLCBXLCBMLCBXIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gNkwgfiAiVywgVywgVywgTCwgVywgVyIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDdMIH4gIlcsIFcsIFcsIEwsIFcsIFcsIFciKSkKCmQybTEKYGBgCgpgYGB7cn0KIyBiZSBzdXJlIHRvIGluc3RhbGwgZ2dhbmltYXRlLCBpLmUuLCBydW4gaW5zdGFsbC5wYWNrYWdlcygnZ2dhbmltYXRlJykKbGlicmFyeShnZ2FuaW1hdGUpCgphbmltIDwtIGdncGxvdChkMm0xLCBhZXMoeCA9IHBfZ3JpZCwgeSA9IHBvc3RlcmlvciwgZ3JvdXAgPSBvYnMpKSArIAogIGdlb21fcG9pbnQoKSArCiAgZ2VvbV9saW5lKCkgKyAKICB0aGVtZShsZWdlbmQucG9zaXRpb24gPSAibm9uZSIpICsKICB0cmFuc2l0aW9uX3N0YXRlcyhvYnMsCiAgICAgICAgICAgICAgICAgICAgdHJhbnNpdGlvbl9sZW5ndGggPSAyLAogICAgICAgICAgICAgICAgICAgIHN0YXRlX2xlbmd0aCA9IDEpICsKICBsYWJzKHggPSAiUHJvcG9ydGlvbiBXYXRlciAocCkiLCB5ID0gIlBvc3RlcmlvciBQcm9iYWJpbGl0eSIpICsKICBnZ3RpdGxlKCdUb3NzIFJlc3VsdDoge2Nsb3Nlc3Rfc3RhdGV9JykgKyAKICBlbnRlcl9mYWRlKCkgKwogIGV4aXRfZmFkZSgpCgphbmltYXRlKGFuaW0sIGhlaWdodCA9IDUwMCwgd2lkdGggPSA2MDApCiNhbmltX3NhdmUoIi4uLy4uL3N0YXRpYy9pbWcvZXhhbXBsZS9Xb3JsZC10b3NzaW5nLWJheWVzaWFuLWNoYXB0ZXIyLmdpZiIpCmBgYAoKIVtdKC4uLy4uL2ltZy9leGFtcGxlL1dvcmxkLXRvc3NpbmctYmF5ZXNpYW4tY2hhcHRlcjIuZ2lmKQoKIyMgUGFja2FnZSB2ZXJzaW9ucwoKYGBge3J9CnNlc3Npb25JbmZvKCkKYGBg" download="02-class.Rmarkdown">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010-examples" target="_blank"><img src="https://gitpod.io/button/open-in-gitpod.svg" style="display: block; margin: auto auto auto 0;" /></a>

## Introduction

For this class, we’ll review code examples found in Chapter 2.

This assumes that you have already installed the `rethinking` package.

If you need help, be sure to remember the references in the [Resources](/resource/):

-   [Installing R/RStudio](/resource/install/)
-   [Installing `rethinking` package](/resource/install-rethinking/)
-   [Rmarkdown](/resource/rmarkdown/)
-   [R Style guide](/resource/style/)

## Chapter 2

### Bayesian Updating: Grid Approximation

Let’s assume we have the table in 2.1

``` r
## R code 2.1
ways <- c( 0 , 3 , 8 , 9 , 0 )
ways/sum(ways)
```

``` language-r
## [1] 0.00 0.15 0.40 0.45 0.00
```

Let’s compute the likelihood of six W’s in nine tosses (assuming a 50% probability):

``` r
## R code 2.2
dbinom( 6 , size=9 , prob=0.5 )
```

``` language-r
## [1] 0.1640625
```

We can see it’s 16.4%.

Be sure to examine the `dbinom` function by typing in `?dbinom` and exploring the documentation. We’ll use this function a lot in this class.

Next, let’s define a grid. This is required when we are using Grid Approximation for our Bayesian calculations (i.e., to estimate the posterior).

``` r
## R code 2.3
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )

p_grid
```

``` language-r
##  [1] 0.00000000 0.05263158 0.10526316 0.15789474 0.21052632 0.26315789
##  [7] 0.31578947 0.36842105 0.42105263 0.47368421 0.52631579 0.57894737
## [13] 0.63157895 0.68421053 0.73684211 0.78947368 0.84210526 0.89473684
## [19] 0.94736842 1.00000000
```

Notice that this function creates a vector with length 20 and that ranges from 0 to 1. Note as well that each vector element is evenly spaced in increments of `(to - from)/(length.out - 1)`.

Think about the trade-off between having a smaller or larger `length.out`.

Next, let’s define our prior. We’ll assume a “flat” prior.

``` r
# define prior
prior <- rep( 1 , 20 )

plot(p_grid, prior, type="b", ylim=c(0,5))
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-6-1.png" width="672" />

``` r
# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )

plot(p_grid, likelihood, type="b", ylim=c(0,0.3))
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-7-1.png" width="672" />

``` r
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

plot(p_grid, unstd.posterior, type="b", ylim=c(0,0.3))
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

``` r
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## R code 2.4
plot( p_grid , posterior , type="b" ,
    xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-9-1.png" width="672" />

{{% callout note %}}

Practice: What happens if we alter the priors? What will be the new posteriors?

Assume 6 W’s and 3 L’s (9 tosses). Plot the posterior and compare them to using a uniform prior.

``` r
# prior 1
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
```

``` r
# prior 2
prior <- exp( -5*abs( p_grid - 0.5 ) )
```

{{% /callout %}}

### Bayesian Updating: Quadratic Approximation

We can also use quadratic approximation, which is discussed on page 42 of Chapter2. We’ll use quadratic approximation approach over the next few weeks before moving to MCMC methods via Stan.

``` r
## R code 2.6
library(rethinking)
globe.qa <- quap(
    alist(
        W ~ dbinom( W+L ,p) ,  # binomial likelihood
        p ~ dunif(0,1)     # uniform prior
    ) ,
    data=list(W=6,L=3) )

globe.qa
```

``` language-r
## 
## Quadratic approximate posterior distribution
## 
## Formula:
## W ~ dbinom(W + L, p)
## p ~ dunif(0, 1)
## 
## Posterior means:
##         p 
## 0.6666664 
## 
## Log-likelihood: -1.3
```

We can also use the `precis` function to summarize parameter estimates. I recommend running `?precis` to look up parameters associated with this function.

``` r
# display summary of quadratic approximation
precis( globe.qa )
```

``` language-r
##        mean        sd      5.5%     94.5%
## p 0.6666664 0.1571339 0.4155361 0.9177966
```

### How does grid approximation compare to analytical posterior calculation?

``` r
## R code 2.7
# analytical calculation
W <- 6
L <- 3
curve( dbeta( x , W+1 , L+1 ) , from=0 , to=1 , col = 1) # solid line
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE , col = 2) # dotted line
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-14-1.png" width="672" />

### Demo Problems

2M1: Recall the globe tossing model from the chapter. Compute and plot the grid approximate posterior distribution for each of the following sets of observations. In each case, assume a uniform prior for p.

``` r
p_grid <- seq( from=0 , to=1 , length.out=100 ) # grid from 0 to 1 with length 100
prior <- rep(1,100) # uniform prior

# likelihood of 3 water in 3 tosses
likelihood <- dbinom( 3 , size=3 , prob=p_grid )

posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize

plot( posterior ~ p_grid , type="l", main = "W, W, W")
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-15-1.png" width="672" />

``` r
# likelihood of 3 water in 4 tosses
likelihood <- dbinom( 3 , size=4 , prob=p_grid )

posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize

plot( posterior ~ p_grid , type="l" , main = "W, W, W, L")
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-16-1.png" width="672" />

``` r
# likelihood of 5 water in 7 tosses
likelihood <- dbinom( 5 , size=7 , prob=p_grid )

posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize

plot( posterior ~ p_grid , type="l" , main = "L, W, W, W, L, W, W, W")
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-17-1.png" width="672" />

## Chapter 3

Assume we have the following model:

``` r
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

set.seed(100) # very important when using randomized functions (e.g., sample)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
```

### Demo Problems

Let’s follow work in section 3.2 to understand how to summarize information from the posterior.

3E1: How much posterior probability lies **below** p = 0.2?

``` r
mean(samples < 0.2)
```

``` language-r
## [1] 4e-04
```

3E2: How much posterior probability lies **above** p = 0.8?

``` r
mean(samples > 0.8)
```

``` language-r
## [1] 0.1116
```

3E3: How much posterior probability lies **between** p = 0.2 and p = 0.8?

``` r
sum( samples > 0.2 & samples < 0.8 ) / 1e4
```

``` language-r
## [1] 0.888
```

3E4: 20% of the posterior probability lies **below** which value of p?

``` r
quantile(samples, probs = 0.2)
```

``` language-r
##       20% 
## 0.5185185
```

3E5: 20% of the posterior probability lies **above** which value of p?

``` r
quantile(samples, probs = 0.8)
```

``` language-r
##       80% 
## 0.7557558
```

3E6: Which values of p contain the narrowest interval equal to 66% of the posterior probability?

``` r
HPDI(samples, prob = 0.66)
```

``` language-r
##     |0.66     0.66| 
## 0.5085085 0.7737738
```

3E7: Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?

``` r
PI(samples, prob = 0.66)
```

``` language-r
##       17%       83% 
## 0.5025025 0.7697698
```

{{% callout note %}}

Comprehension question: under what circumstances would the PI differ from the HPDI (holding interval value the same)?

{{% /callout %}}

3M1: Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Constructe the posterior distribution, using grid approximation. Use the same flat prior as before.

``` r
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(posterior)
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-26-1.png" width="672" />

3M2. Draw 10,000 samples from the grid approximation from above. Then use the sample to calculate the 90% HPDI for p.

``` r
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

HPDI(samples, prob = 0.9)
```

``` language-r
##      |0.9      0.9| 
## 0.3293293 0.7167167
```

3M3. Construct a **posterior predictive check** for this model and data. The means simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing 8 water in 15 tosses?

``` r
w <- rbinom(1e4, size = 15, prob = samples)
mean(w == 8)
```

``` language-r
## [1] 0.1444
```

3M4: Using the posterior distribution constructed from the new (8/15) data, now calculate the probability of observing 6 water in 9 tosses.

``` r
w <- rbinom(1e4, size = 9, prob = samples)
mean(w == 6)
```

``` language-r
## [1] 0.1751
```

{{% callout note %}}

Modify the values w (0 to 9) for the size = 9 example in 3M4. Compare these values to Figure 3.6.

{{% /callout %}}

## Appendix: `tidyverse` conversion

Statistical Rethinking uses base R functions. More recently, Soloman Kurz has created a [translation of the book’s functions](https://bookdown.org/content/4857/) into `tidyverse` (and later `brms`) code. This is not necessary but could be extremely helpful to classmates who are familiar with `tidyverse` already.

First, we’ll need to call `tidyverse`. If you do not have `tidyverse`, you’ll need to install it.

``` r
library(tidyverse)
```

For example, we can translate 2.3 code using pipes (`%>%`)

``` r
d <- tibble(p_grid = seq(from = 0, to = 1, length.out = 20),      # define grid
           prior  = 1) %>%                                       # define prior
    mutate(likelihood = dbinom(6, size = 9, prob = p_grid)) %>%  # compute likelihood at each value in grid
    mutate(unstd_posterior = likelihood * prior) %>%             # compute product of likelihood and prior
    mutate(posterior = unstd_posterior / sum(unstd_posterior)) 

d
```

``` language-r
## # A tibble: 20 × 5
##    p_grid prior likelihood unstd_posterior   posterior
##     <dbl> <dbl>      <dbl>           <dbl>       <dbl>
##  1 0          1 0               0          0          
##  2 0.0526     1 0.00000152      0.00000152 0.000000799
##  3 0.105      1 0.0000819       0.0000819  0.0000431  
##  4 0.158      1 0.000777        0.000777   0.000409   
##  5 0.211      1 0.00360         0.00360    0.00189    
##  6 0.263      1 0.0112          0.0112     0.00587    
##  7 0.316      1 0.0267          0.0267     0.0140     
##  8 0.368      1 0.0529          0.0529     0.0279     
##  9 0.421      1 0.0908          0.0908     0.0478     
## 10 0.474      1 0.138           0.138      0.0728     
## 11 0.526      1 0.190           0.190      0.0999     
## 12 0.579      1 0.236           0.236      0.124      
## 13 0.632      1 0.267           0.267      0.140      
## 14 0.684      1 0.271           0.271      0.143      
## 15 0.737      1 0.245           0.245      0.129      
## 16 0.789      1 0.190           0.190      0.0999     
## 17 0.842      1 0.118           0.118      0.0621     
## 18 0.895      1 0.0503          0.0503     0.0265     
## 19 0.947      1 0.00885         0.00885    0.00466    
## 20 1          1 0               0          0
```

With this calculated, we can then use `ggplot2`, the staple `ggplot2` data visualization package, to plot our posterior.

``` r
d %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "20 points",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-32-1.png" width="672" />

For this class, we’ll occasionally refer to Soloman’s guide.

### Demo Problem

2M1: Recall the globe tossing model from the chapter. Compute and plot the grid approximate posterior distribution for each of the following sets of observations. In each case, assume a uniform prior for p.

``` r
## be sure to have tidyverse installed, i.e., install.packages('tidyverse')
library(tidyverse)

dist <- tibble(p_grid = seq(from = 0, to = 1, length.out = 20),
               prior = rep(1, times = 20)) %>%
  mutate(likelihood_1 = dbinom(3, size = 3, prob = p_grid),
         likelihood_2 = dbinom(3, size = 4, prob = p_grid),
         likelihood_3 = dbinom(5, size = 7, prob = p_grid),
         across(starts_with("likelihood"), ~ .x * prior),
         across(starts_with("likelihood"), ~ .x / sum(.x))) %>%
  pivot_longer(cols = starts_with("likelihood"), names_to = "pattern",
               values_to = "posterior") %>%
  separate(pattern, c(NA, "pattern"), sep = "_", convert = TRUE) %>%
  mutate(obs = case_when(pattern == 1L ~ "W, W, W",
                         pattern == 2L ~ "W, W, W, L",
                         pattern == 3L ~ "L, W, W, L, W, W, W"))

ggplot(dist, aes(x = p_grid, y = posterior)) +
  facet_wrap(vars(fct_inorder(obs)), nrow = 1) +
  geom_line() +
  geom_point() +
  labs(x = "Proportion Water (p)", y = "Posterior Density")
```

<img src="/example/02-class_files/figure-html/unnamed-chunk-33-1.png" width="672" />

``` r
# W, W, W, L, W, W, W

# challenge: functionalize this to generalize this for any read in toss string 

d2m1 <- tibble(p_grid = seq(from = 0, to = 1, length.out = 20),
               prior = rep(1, times = 20)) %>%
  mutate(
         likelihood_1 = dbinom(1, size = 1, prob = p_grid),
         likelihood_2 = dbinom(2, size = 2, prob = p_grid),
         likelihood_3 = dbinom(3, size = 3, prob = p_grid),
         likelihood_4 = dbinom(3, size = 4, prob = p_grid),
         likelihood_5 = dbinom(4, size = 5, prob = p_grid),
         likelihood_6 = dbinom(5, size = 6, prob = p_grid),
         likelihood_7 = dbinom(6, size = 7, prob = p_grid),
         across(starts_with("likelihood"), ~ .x * prior),
         across(starts_with("likelihood"), ~ .x / sum(.x))) %>%
  pivot_longer(cols = starts_with("likelihood"), names_to = "pattern",
               values_to = "posterior") %>%
  separate(pattern, c(NA, "pattern"), sep = "_", convert = TRUE) %>%
  mutate(obs = case_when(pattern == 1L ~ "W",
                         pattern == 2L ~ "W, W",
                         pattern == 3L ~ "W, W, W,",
                         pattern == 4L ~ "W, W, W, L",
                         pattern == 5L ~ "W, W, W, L, W",
                         pattern == 6L ~ "W, W, W, L, W, W",
                         pattern == 7L ~ "W, W, W, L, W, W, W"))

d2m1
```

``` language-r
## # A tibble: 140 × 5
##    p_grid prior pattern posterior obs                
##     <dbl> <dbl>   <int>     <dbl> <chr>              
##  1 0          1       1 0         W                  
##  2 0          1       2 0         W, W               
##  3 0          1       3 0         W, W, W,           
##  4 0          1       4 0         W, W, W, L         
##  5 0          1       5 0         W, W, W, L, W      
##  6 0          1       6 0         W, W, W, L, W, W   
##  7 0          1       7 0         W, W, W, L, W, W, W
##  8 0.0526     1       1 0.00526   W                  
##  9 0.0526     1       2 0.000405  W, W               
## 10 0.0526     1       3 0.0000277 W, W, W,           
## # … with 130 more rows
```

``` r
# be sure to install gganimate, i.e., run install.packages('gganimate')
library(gganimate)

anim <- ggplot(d2m1, aes(x = p_grid, y = posterior, group = obs)) + 
  geom_point() +
  geom_line() + 
  theme(legend.position = "none") +
  transition_states(obs,
                    transition_length = 2,
                    state_length = 1) +
  labs(x = "Proportion Water (p)", y = "Posterior Probability") +
  ggtitle('Toss Result: {closest_state}') + 
  enter_fade() +
  exit_fade()

animate(anim, height = 500, width = 600)
```

![](02-class_files/figure-html/unnamed-chunk-35-1.gif)<!-- -->

``` r
#anim_save("../../static/img/example/World-tossing-bayesian-chapter2.gif")
```

![](../../img/example/World-tossing-bayesian-chapter2.gif)

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
##  [1] gganimate_1.0.7      forcats_0.5.1        stringr_1.4.0       
##  [4] dplyr_1.0.7          purrr_0.3.4          readr_2.0.2         
##  [7] tidyr_1.1.4          tibble_3.1.6         tidyverse_1.3.1     
## [10] rethinking_2.21      cmdstanr_0.4.0.9001  rstan_2.21.3        
## [13] ggplot2_3.3.5        StanHeaders_2.21.0-7
## 
## loaded via a namespace (and not attached):
##   [1] colorspace_2.0-2     class_7.3-19         ellipsis_0.3.2      
##   [4] base64enc_0.1-3      fs_1.5.0             xaringanExtra_0.5.5 
##   [7] rstudioapi_0.13      proxy_0.4-26         farver_2.1.0        
##  [10] fansi_0.5.0          mvtnorm_1.1-3        lubridate_1.8.0     
##  [13] xml2_1.3.2           codetools_0.2-18     knitr_1.36          
##  [16] jsonlite_1.7.2       bsplus_0.1.3         broom_0.7.9         
##  [19] dbplyr_2.1.1         compiler_4.1.1       httr_1.4.2          
##  [22] backports_1.4.1      assertthat_0.2.1     fastmap_1.1.0       
##  [25] cli_3.1.0            tweenr_1.0.2         htmltools_0.5.2     
##  [28] prettyunits_1.1.1    tools_4.1.1          coda_0.19-4         
##  [31] gtable_0.3.0         glue_1.6.0           posterior_1.1.0     
##  [34] Rcpp_1.0.7           cellranger_1.1.0     jquerylib_0.1.4     
##  [37] vctrs_0.3.8          blogdown_1.5         transformr_0.1.3    
##  [40] tensorA_0.36.2       xfun_0.28            ps_1.6.0            
##  [43] rvest_1.0.2          mime_0.12            lpSolve_5.6.15      
##  [46] lifecycle_1.0.1      renv_0.14.0          MASS_7.3-54         
##  [49] scales_1.1.1         hms_1.1.1            inline_0.3.19       
##  [52] yaml_2.2.1           gridExtra_2.3        downloadthis_0.2.1  
##  [55] loo_2.4.1            sass_0.4.0           stringi_1.7.6       
##  [58] highr_0.9            e1071_1.7-9          checkmate_2.0.0     
##  [61] pkgbuild_1.3.1       shape_1.4.6          rlang_0.4.12        
##  [64] pkgconfig_2.0.3      matrixStats_0.61.0   distributional_0.2.2
##  [67] evaluate_0.14        lattice_0.20-44      sf_1.0-5            
##  [70] labeling_0.4.2       processx_3.5.2       tidyselect_1.1.1    
##  [73] plyr_1.8.6           magrittr_2.0.1       bookdown_0.24       
##  [76] R6_2.5.1             magick_2.7.3         generics_0.1.1      
##  [79] DBI_1.1.1            pillar_1.6.4         haven_2.4.3         
##  [82] withr_2.4.3          units_0.7-2          abind_1.4-5         
##  [85] modelr_0.1.8         crayon_1.4.2         KernSmooth_2.23-20  
##  [88] uuid_1.0-3           utf8_1.2.2           tzdb_0.1.2          
##  [91] rmarkdown_2.11       progress_1.2.2       grid_4.1.1          
##  [94] readxl_1.3.1         callr_3.7.0          reprex_2.0.1        
##  [97] digest_0.6.29        classInt_0.4-3       RcppParallel_5.1.4  
## [100] stats4_4.1.1         munsell_0.5.0        bslib_0.3.1
```
