---
title: Problem Set 1 Solutions
date: "2022-02-06"
menu:
  assignment:
    parent: Problem sets
    weight: 1
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on January 31, 2022 at 11:59am.

Step 1: Download this file locally.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCAxIFNvbHV0aW9ucwpkYXRlOiAiYHIgU3lzLkRhdGUoKWAiCm1lbnU6CiAgYXNzaWdubWVudDoKICAgIHBhcmVudDogUHJvYmxlbSBzZXQgc29sdXRpb25zCiAgICB3ZWlnaHQ6IDEKdHlwZTogZG9jcwotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQpgYGAKCgotICoqTmFtZSoqOgotICoqVU5DQyBJRCoqOiAKLSAqKk90aGVyIHN0dWRlbnQgd29ya2VkIHdpdGggKG9wdGlvbmFsKSoqOgoKMS4gWW91ciBmcmllbmQganVzdCBiZWNhbWUgaW50ZXJlc3RlZCBpbiBCYXllc2lhbiBzdGF0aXN0aWNzLiBJbiBvbmUgcGFyYWdyYXBoIG9yIGxlc3MgKG5vIGNvZGUpLCBleHBsYWluIHRoZSBmb2xsb3dpbmcgdG8gdGhlbToKKiBXaHkvd2hlbiBpcyBCYXllc2lhbiBzdGF0aXN0aWNzIHVzZWZ1bD8KCk1hbnkgYW5zd2VycyBhcmUgc3VmZmljaWVudC4KCkJheWVzaWFuIHN0YXRpc3RpY3MgYXJlIHVzZWZ1bCBpbiBzaXR1YXRpb25zIHdoZXJlIChzZWUgW0theSBldCBhbC4gQ0hJIDIwMTZdKGh0dHBzOi8vd3d3Lm1qc2theS5jb20vcGFwZXJzL2NoaV8yMDE2X2JheWVzLnBkZikpOgoKMS4gQmF5ZXNpYW4gYW5hbHlzaXMgcHJvdmlkZXMgbW9yZSBwcmVjaXNlIGVzdGltYXRlcyBvZiBwcmV2aW91c2x5LXN0dWRpZWQgY29uZGl0aW9ucyBpbiBlYWNoIHN1Y2Nlc3NpdmUgc3R1ZHkuCjIuIEJheWVzaWFuIGFuYWx5c2lzIGFsbG93cyBtb3JlIHByZWNpc2UgY29tcGFyaXNvbiBvZiBub3ZlbCBjb25kaXRpb25zIGFnYWluc3Qga25vd24gY29uZGl0aW9ucy4KMy4gQmF5ZXNpYW4gYW5hbHlzaXMgZHJhd3MgbW9yZSByZWFzb25hYmxlIGNvbmNsdXNpb25zIGZyb20gc21hbGwtbiBzdHVkaWVzLgo0LiBCYXllc2lhbiBhbmFseXNlcyBoZWxwIHNoaWZ0IHRoZSBjb252ZXJzYXRpb24gZnJvbSDigJxEb2VzIGl0IHdvcms/4oCdIHRvIOKAnEhvdyBzdHJvbmcgaXMgdGhlIGVmZmVjdD/igJ0sIOKAnEhvdyBjb25maWRlbnQgYXJlIHdlIGluIHRoaXMgZXN0aW1hdGU/4oCdLCBhbmQg4oCcU2hvdWxkIHdlIGNhcmU/4oCdCgoqIFdoYXQgYXJlIHRoZSBzaW1pbGFyaXRpZXMgaW4gQmF5ZXNpYW4gYW5kIGZyZXF1ZW50aXN0IHN0YXRpc3RpY3M/CgpCYXllc2lhbiBhbmQgZnJlcXVlbnRpc3QgYW5hbHlzZXMgc2hhcmUgYSBjb21tb24gZ29hbDogdG8gbGVhcm4gZnJvbSBkYXRhIGFib3V0IHRoZSB3b3JsZCBhcm91bmQgdXMuIFRoZXkgZGlmZmVyIGluIGhvdyB0aGV5IGludGVycHJldCBwcm9iYWJpbGl0aWVzLiBCYXllc2lhbnMgaW50ZXJwcmV0IHByb2JhYmlsaXRpZXMgYXMgYSAiZGVncmVlIG9mIGJlbGllZiIgd2hpbGUgRnJlcXVlbnRpc3RzIGludGVycHJldCBwcm9iYWJpbGl0aWVzIGFzIGEgImxvbmcgcnVuIHJlbGF0aXZlIGZyZXF1ZW5jeSIuIAoKSW4gZXNzZW5jZSwgRnJlcXVlbnRpc3Qgc3RhdGlzdGljcyBjYW4gYmUgdGhvdWdodCBvZiBhcyBhIHN1YnNldCBvZiBCYXllc2lhbiBzdGF0aXN0aWNzIGluIHdoaWNoIHdlIGhhdmUgbm8gcHJpb3IgaW5mb3JtYXRpb24gb3Igd2hlbiBvdXIgc2FtcGxlIHNpemUgaXMgc28gbGFyZ2UsIHRoYXQgb3VyIHByaW9yIG5vIGxvbmdlciBoYXMgYW55IHdlaWdodCBpbiBvdXIgcG9zdGVyaW9yLiBJbiB0aG9zZSBjYXNlcywgdHlwaWNhbGx5IEZyZXF1ZW50aXN0IGFuZCBCYXllc2lhbiBzdGF0aXN0aWNzIHByb2R1Y2UgdmVyeSBzaW1pbGFyIHJlc3VsdHMuCgoyLiBTdXBwb3NlIHRoZSBnbG9iZSB0b3NzaW5nIGRhdGEgKENoYXB0ZXIgMikgaGFkIHR1cm5lZCBvdXQgdG8gYmUgNCB3YXRlciBhbmQgMTEgbGFuZC4gQ29uc3RydWN0IHRoZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uLCB1c2luZyBncmlkIGFwcHJveGltYXRpb24uIFVzZSB0aGUgc2FtZSBmbGF0IHByaW9yIGFzIGluIHRoZSBib29rLgoKYGBge3J9CiMjIFIgY29kZSAyLjMKc2V0LnNlZWQoMTAwKQojIGRlZmluZSBncmlkCnBfZ3JpZCA8LSBzZXEoIGZyb209MCAsIHRvPTEgLCBsZW5ndGgub3V0PTEwMDAgKQoKIyBkZWZpbmUgcHJpb3IKcHJpb3IgPC0gcmVwKCAxICwgMTAwMCApCgojIGNvbXB1dGUgbGlrZWxpaG9vZCBhdCBlYWNoIHZhbHVlIGluIGdyaWQKbGlrZWxpaG9vZCA8LSBkYmlub20oIDQgLCBzaXplPTE1ICwgcHJvYj1wX2dyaWQgKQoKIyBjb21wdXRlIHByb2R1Y3Qgb2YgbGlrZWxpaG9vZCBhbmQgcHJpb3IKdW5zdGQucG9zdGVyaW9yIDwtIGxpa2VsaWhvb2QgKiBwcmlvcgoKIyBzdGFuZGFyZGl6ZSB0aGUgcG9zdGVyaW9yLCBzbyBpdCBzdW1zIHRvIDEKcG9zdGVyaW9yIDwtIHVuc3RkLnBvc3RlcmlvciAvIHN1bSh1bnN0ZC5wb3N0ZXJpb3IpCgpwbG90KCBwX2dyaWQgLCBwb3N0ZXJpb3IgLCB0eXBlPSJiIiAsCiAgICB4bGFiPSJwcm9iYWJpbGl0eSBvZiB3YXRlciIgLCB5bGFiPSJwb3N0ZXJpb3IgcHJvYmFiaWxpdHkiICkKbXRleHQoICIxMDAwIHBvaW50cyIgKQpgYGAKCgozLiBOb3cgc3VwcG9zZSB0aGUgZGF0YSBhcmUgNCB3YXRlciBhbmQgMiBsYW5kLiBDb21wdXRlIHRoZSBwb3N0ZXJpb3IgYWdhaW4sIGJ1dCB0aGlzIHRpbWUgdXNlIGEgcHJpb3IgdGhhdCBpcyB6ZXJvIGJlbG93IHAgPSAwLjUgYW5kIGEgY29uc3RhbnQgYWJvdmUgcCA9IDAuNS4gVGhpcyBjb3JyZXNwb25kcyB0byBwcmlvciBpbmZvcm1hdGlvbiB0aGF0IGEgbWFqb3JpdHkgb2YgdGhlIEVhcnRo4oCZcyBzdXJmYWNlIGlzIHdhdGVyLiBDb21wYXJlIHRoZSBuZXcgcG9zdGVyaW9yIHdpdGggdGhlIHByZXZpb3VzIHBvc3RlcmlvciAoZmxhdCkKCgpgYGB7cn0KIyMgUiBjb2RlIDIuMwojIGRlZmluZSBncmlkCnBfZ3JpZCA8LSBzZXEoIGZyb209MCAsIHRvPTEgLCBsZW5ndGgub3V0PTEwMDAgKQoKIyBkZWZpbmUgcHJpb3IKcG9zdF9wcmlvciA8LSBpZmVsc2UoIHBfZ3JpZCA8IDAuNSAsIDAgLCAxICkKCiMgY29tcHV0ZSBsaWtlbGlob29kIGF0IGVhY2ggdmFsdWUgaW4gZ3JpZApwb3N0X2xpa2VsaWhvb2QgPC0gZGJpbm9tKCA0ICwgc2l6ZT02ICwgcHJvYj1wX2dyaWQgKQoKIyBjb21wdXRlIHByb2R1Y3Qgb2YgbGlrZWxpaG9vZCBhbmQgcHJpb3IKcG9zdF91bnN0ZC5wb3N0ZXJpb3IgPC0gcG9zdF9saWtlbGlob29kICogcG9zdF9wcmlvcgoKIyBzdGFuZGFyZGl6ZSB0aGUgcG9zdGVyaW9yLCBzbyBpdCBzdW1zIHRvIDEKcG9zdF9wb3N0ZXJpb3IgPC0gcG9zdF91bnN0ZC5wb3N0ZXJpb3IgLyBzdW0ocG9zdF91bnN0ZC5wb3N0ZXJpb3IpCgpwbG90KCBwX2dyaWQgLCBwb3N0X3Bvc3RlcmlvciAsIHR5cGU9ImIiICwgY29sID0gInJlZCIsCiAgICB4bGFiPSJwcm9iYWJpbGl0eSBvZiB3YXRlciIgLCB5bGFiPSJwb3N0ZXJpb3IgcHJvYmFiaWxpdHkiLCB5bGltPWMoMCwgMC4wMDUpICkKbGluZXMocF9ncmlkICwgcG9zdGVyaW9yICwgdHlwZT0iYiIgLCBjb2wgPSAiZ3JlZW4iKQptdGV4dCggIkNvbXBhcmUgcG9zdGVyaW9ycyIgKQpgYGAKCmBgYHtyfQpwbG90KCBwX2dyaWQgLCBwb3N0X3ByaW9yICwgdHlwZT0iYiIgLCBjb2wgPSAicmVkIiwKICAgIHhsYWI9InByb2JhYmlsaXR5IG9mIHdhdGVyIiAsIHlsYWI9InByaW9yIHByb2JhYmlsaXR5IiwgeWxpbT1jKDAsIDEpICkKbGluZXMocF9ncmlkICwgcHJpb3IgLCB0eXBlPSJiIiAsIGNvbCA9ICJncmVlbiIpCm10ZXh0KCAiQ29tcGFyZSBwcmlvcnMiICkKYGBgCmBgYHtyfQpwbG90KCBwX2dyaWQgLCBwb3N0X2xpa2VsaWhvb2QgLCB0eXBlPSJiIiAsIGNvbCA9ICJyZWQiLAogICAgeGxhYj0icHJvYmFiaWxpdHkgb2Ygd2F0ZXIiICwgeWxhYj0icHJpb3IgbGlrZWxpaG9vZCIsIHlsaW09YygwLCAuNCkgKQpsaW5lcyhwX2dyaWQgLCBsaWtlbGlob29kICwgdHlwZT0iYiIgLCBjb2wgPSAiZ3JlZW4iKQptdGV4dCggIkNvbXBhcmUgbGlrZWxpaG9vZHMiICkKYGBgCgo0LiBGb3IgdGhlIHBvc3RlcmlvciBkaXN0cmlidXRpb24gZnJvbSAzLCBjb21wdXRlIDg5JSBwZXJjZW50aWxlIGFuZCBIUERJIGludGVydmFscy4gQ29tcGFyZSB0aGUgd2lkdGhzIG9mIHRoZXNlIGludGVydmFscy4gV2hpY2ggaXMgd2lkZXI/IFdoeT8gSWYgeW91IGhhZCBvbmx5IHRoZSBpbmZvcm1hdGlvbiBpbiB0aGUgaW50ZXJ2YWwsIHdoYXQgbWlnaHQgeW91IG1pc3VuZGVyc3RhbmQgYWJvdXQgdGhlIHNoYXBlIG9mIHRoZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uPwoKYGBge3Igd2FybmluZz1GQUxTRX0KbGlicmFyeShyZXRoaW5raW5nKQojY2FsY3VsYXRlIHNhbXBsZXMgb2YgcG9zdF9wb3N0ZXJpb3IKc2FtcGxlcyA8LSBzYW1wbGUocF9ncmlkLCBzaXplID0gMWU0LCByZXBsYWNlID0gVFJVRSwgcHJvYj1wb3N0X3Bvc3RlcmlvcikKCiMgODkgcGVyY2VudGlsZQpQSShzYW1wbGVzLCBwcm9iPS44OSkKYGBgCgpgYGB7cn0KIyBIUERJCkhQREkoc2FtcGxlcywgcHJvYj0uODkpCmBgYAoKSFBESSBpcyBkZW5zZXN0IChuYXJyb3dlc3QpIHJlZ2lvbiB3aXRoIDg5JSBtYXNzLCB0aHVzIFBJIGlzIHdpZGVyIHRoYW4gSFBESS4KCldpdGggb25seSBpbnRlcnZhbHMsIGFuIGFuYWx5c3Qgd291bGQgbWlzcyB0aGUgZHJvcCBpbiB0aGUgcG9zdGVyaW9yJ3Mgc2hhcGUgYmVsb3cgNTIlIGR1ZSB0byB0aGUgcHJpb3IgdGhhdCBhc3N1bWVzIGdyZWF0ZXIgdGhhbiA1MCUuCgoKT1BUSU9OQUwgQ0hBTExFTkdFLiBTdXBwb3NlIHRoZXJlIGlzIGJpYXMgaW4gc2FtcGxpbmcgc28gdGhhdCBMYW5kIGlzIG1vcmUgbGlrZWx5IHRoYW4gV2F0ZXIgdG8gYmUgcmVjb3JkZWQuIFNwZWNpZmljYWxseSwgYXNzdW1lIHRoYXQgMS1pbi01ICgyMCUpIG9mIFdhdGVyIHNhbXBsZXMgYXJlIGFjY2lkZW50YWxseSByZWNvcmRlZCBpbnN0ZWFkIGFzICJMYW5kIi4gRmlyc3QsIHdyaXRlIGEgZ2VuZXJhdGl2ZSBzaW11bGF0aW9uIG9mIHRoaXMgc2FtcGxpbmcgcHJvY2Vzcy4gQXNzdW1pbmcgdGhlIHRydWUgcHJvcG9ydGlvbiBvZiBXYXRlciBpcyAwLjcwLCB3aGF0IHByb3BvcnRpb24gZG9lcyB5b3VyIHNpbXVsYXRpb24gdGVuZCB0byBwcm9kdWNlIGluc3RlYWQ/IAoKYGBge3J9CiMgUHIoV3xXKSA9IDAuOAojIFByKFd8TCkgPSAwLjIKIyBQcihXKSA9IDAuNyowLjgKc2V0LnNlZWQoMTAwKQp0cnVlX3Byb2IgPSAwLjcKYmlhcyA9IDAuMgpOPTEwMDAwCgojIGFzc3VtZSAxID0gd2F0ZXIsIDAgPSBsYW5kCnRydWVXIDwtIHJiaW5vbShOLHNpemU9MjAscHJvYj10cnVlX3Byb2IpCm9ic1cgPC0gcmJpbm9tKE4sc2l6ZT10cnVlVyxwcm9iPTEtYmlhcykKbWVhbihvYnNXLzIwKQoKIyBvcgpXIDwtIHJiaW5vbShOLHNpemU9MjAscHJvYj10cnVlX3Byb2IqKDEtYmlhcykpCgptZWFuKFcvMjApCmBgYAoKU2Vjb25kLCB1c2luZyBhIHNpbXVsYXRlZCBzYW1wbGUgb2YgMjAgdG9zc2VzLCBjb21wdXRlIHRoZSB1bmJpYXNlZCBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uIG9mIHRoZSB0cnVlIHByb3BvcnRpb24gb2Ygd2F0ZXIuCgpgYGB7cn0KIyBub3cgYW5hbHl6ZQojIFByKHB8VyxOKSA9IFByKFd8cCxOKVByKHApIC8gWgojIFByKFd8TixwKSA9IFByKFcpUHIoV3xXKQoKVyA8LSByYmlub20oMSxzaXplPTIwLHByb2I9MC43KjAuOCkKIyBncmlkIGFwcHJveApncmlkX3AgPC0gc2VxKGZyb209MCx0bz0xLGxlbj0xMDApCnByX3AgPC0gZGJldGEoZ3JpZF9wLDEsMSkKcHJXIDwtIGRiaW5vbShXLDIwLGdyaWRfcCowLjgpCnBvc3QgPC0gcHJXKnByX3AKCnBvc3RfYmFkIDwtIGRiaW5vbShXLDIwLGdyaWRfcCkKCnBsb3QoZ3JpZF9wLHBvc3QsdHlwZT0ibCIsbHdkPTQseGxhYj0icHJvcG9ydGlvbiB3YXRlciIseWxhYj0icGxhdXNpYmlsaXR5IikKbGluZXMoZ3JpZF9wLHBvc3RfYmFkLGNvbD0yLGx3ZD00KQpgYGAKCi0tLS0tLS0KCiMgdGlkeXZlcnNlCgoyLiBTdXBwb3NlIHRoZSBnbG9iZSB0b3NzaW5nIGRhdGEgKENoYXB0ZXIgMikgaGFkIHR1cm5lZCBvdXQgdG8gYmUgNCB3YXRlciBhbmQgMTEgbGFuZC4gQ29uc3RydWN0IHRoZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uLCB1c2luZyBncmlkIGFwcHJveGltYXRpb24uIFVzZSB0aGUgc2FtZSBmbGF0IHByaW9yIGFzIGluIHRoZSBib29rLgoKYGBge3J9CiMgdGlkeXZlcnNlIGFwcHJvYWNoCmxpYnJhcnkodGlkeXZlcnNlKQojIGhvdyBtYW55IGdyaWQgcG9pbnRzIHdvdWxkIHlvdSBsaWtlPwpuIDwtIDEwMDAKbl9zdWNjZXNzIDwtIDQKbl90cmlhbHMgIDwtIDE1CgpkIDwtCiAgdGliYmxlKHBfZ3JpZCA9IHNlcShmcm9tID0gMCwgdG8gPSAxLCBsZW5ndGgub3V0ID0gbiksCiAgICAgICAgICMgbm90ZSB3ZSdyZSBzdGlsbCB1c2luZyBhIGZsYXQgdW5pZm9ybSBwcmlvcgogICAgICAgICBwcmlvciAgPSAxKSAlPiUgCiAgbXV0YXRlKGxpa2VsaWhvb2QgPSBkYmlub20obl9zdWNjZXNzLCBzaXplID0gbl90cmlhbHMsIHByb2IgPSBwX2dyaWQpKSAlPiUgCiAgbXV0YXRlKHBvc3RlcmlvciA9IChsaWtlbGlob29kICogcHJpb3IpIC8gc3VtKGxpa2VsaWhvb2QgKiBwcmlvcikpCgpkICU+JQogIHBpdm90X2xvbmdlcigtcF9ncmlkKSAlPiUKICBmaWx0ZXIobmFtZSA9PSAicG9zdGVyaW9yIikgJT4lCiAgZ2dwbG90KGFlcyh4ID0gcF9ncmlkLCB5ID0gdmFsdWUpKSArCiAgZ2VvbV9saW5lKCkgKwogIGdndGl0bGUoIlBvc3RlcmlvciAoRmxhdCBwcmlvcik6IDQgb3V0IG9mIDE1IikgKwogIHRoZW1lX2J3KCkKYGBgCgozLiBOb3cgc3VwcG9zZSB0aGUgZGF0YSBhcmUgNCB3YXRlciBhbmQgMiBsYW5kLiBDb21wdXRlIHRoZSBwb3N0ZXJpb3IgYWdhaW4sIGJ1dCB0aGlzIHRpbWUgdXNlIGEgcHJpb3IgdGhhdCBpcyB6ZXJvIGJlbG93IHAgPSAwLjUgYW5kIGEgY29uc3RhbnQgYWJvdmUgcCA9IDAuNS4gVGhpcyBjb3JyZXNwb25kcyB0byBwcmlvciBpbmZvcm1hdGlvbiB0aGF0IGEgbWFqb3JpdHkgb2YgdGhlIEVhcnRo4oCZcyBzdXJmYWNlIGlzIHdhdGVyLiBDb21wYXJlIHRoZSBuZXcgcG9zdGVyaW9yIHdpdGggdGhlIHByZXZpb3VzIHBvc3RlcmlvciAoZmxhdCkKCmBgYHtyfQojIGhvdyBtYW55IGdyaWQgcG9pbnRzIHdvdWxkIHlvdSBsaWtlPwpuIDwtIDEwMDAKbl9zdWNjZXNzIDwtIDQKbl90cmlhbHMgIDwtIDYKCmQgPC0KICB0aWJibGUocF9ncmlkID0gc2VxKGZyb20gPSAwLCB0byA9IDEsIGxlbmd0aC5vdXQgPSBuKSkgJT4lIAogIG11dGF0ZShwcmlvciA9IGlmX2Vsc2UocF9ncmlkID4gMC41LCAwLjMsIDApKSAlPiUKICBtdXRhdGUobGlrZWxpaG9vZCA9IGRiaW5vbShuX3N1Y2Nlc3MsIHNpemUgPSBuX3RyaWFscywgcHJvYiA9IHBfZ3JpZCkpICU+JSAKICBtdXRhdGUocG9zdGVyaW9yID0gKGxpa2VsaWhvb2QgKiBwcmlvcikgLyBzdW0obGlrZWxpaG9vZCAqIHByaW9yKSkKCmQgJT4lCiAgcGl2b3RfbG9uZ2VyKC1wX2dyaWQpICU+JQogIGZpbHRlcihuYW1lID09ICJwb3N0ZXJpb3IiKSAlPiUKICBnZ3Bsb3QoYWVzKHggPSBwX2dyaWQsIHkgPSB2YWx1ZSkpICsKICBnZW9tX2xpbmUoKSArCiAgZ2d0aXRsZSgiUG9zdGVyaW9yICg+NTAlIHByaW9yKTogNCBvdXQgb2YgMTUiKSArCiAgdGhlbWVfYncoKQpgYGAKCiMjIFBhY2thZ2UgdmVyc2lvbnMKCmBgYHtyfQpzZXNzaW9uSW5mbygpCmBgYA==" download="01-problem-set-solutions.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

Step 2: Complete the assignment

Step 3: Knit the assignment as either an html or pdf file.

Step 4: Submit your file here [through this canvas link](https://uncc.instructure.com/courses/171000/assignments/1415432).

------------------------------------------------------------------------

-   **Name**:
-   **UNCC ID**:
-   **Other student worked with (optional)**:

1.  Your friend just became interested in Bayesian statistics. In one paragraph or less (no code), explain the following to them:

-   Why/when is Bayesian statistics useful?

Many answers are sufficient.

Bayesian statistics are useful in situations where (see [Kay et al. CHI 2016](https://www.mjskay.com/papers/chi_2016_bayes.pdf)):

1.  Bayesian analysis provides more precise estimates of previously-studied conditions in each successive study.
2.  Bayesian analysis allows more precise comparison of novel conditions against known conditions.
3.  Bayesian analysis draws more reasonable conclusions from small-n studies.
4.  Bayesian analyses help shift the conversation from “Does it work?” to “How strong is the effect?” “How confident are we in this estimate?” and “Should we care?”

-   What are the similarities in Bayesian and frequentist statistics?

Bayesian and frequentist analyses share a common goal: to learn from data about the world around us. They differ in how they interpret probabilities. Bayesians interpret probabilities as a “degree of belief” while Frequentists interpret probabilities as a “long run relative frequency.”

In essence, Frequentist statistics can be thought of as a subset of Bayesian statistics in which we have no prior information or when our sample size is so large, that our prior no longer has any weight in our posterior. In those cases, typically Frequentist and Bayesian statistics produce very similar results.

2.  Suppose the globe tossing data (Chapter 2) had turned out to be 4 water and 11 land. Construct the posterior distribution, using grid approximation. Use the same flat prior as in the book.

``` r
## R code 2.3
set.seed(100)
# define grid
p_grid <- seq( from=0 , to=1 , length.out=1000 )

# define prior
prior <- rep( 1 , 1000 )

# compute likelihood at each value in grid
likelihood <- dbinom( 4 , size=15 , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
    xlab="probability of water" , ylab="posterior probability" )
mtext( "1000 points" )
```

<img src="/assignment/01-problem-set-solutions_files/figure-html/unnamed-chunk-2-1.png" width="672" />

3.  Now suppose the data are 4 water and 2 land. Compute the posterior again, but this time use a prior that is zero below p = 0.5 and a constant above p = 0.5. This corresponds to prior information that a majority of the Earth’s surface is water. Compare the new posterior with the previous posterior (flat)

``` r
## R code 2.3
# define grid
p_grid <- seq( from=0 , to=1 , length.out=1000 )

# define prior
post_prior <- ifelse( p_grid < 0.5 , 0 , 1 )

# compute likelihood at each value in grid
post_likelihood <- dbinom( 4 , size=6 , prob=p_grid )

# compute product of likelihood and prior
post_unstd.posterior <- post_likelihood * post_prior

# standardize the posterior, so it sums to 1
post_posterior <- post_unstd.posterior / sum(post_unstd.posterior)

plot( p_grid , post_posterior , type="b" , col = "red",
    xlab="probability of water" , ylab="posterior probability", ylim=c(0, 0.005) )
lines(p_grid , posterior , type="b" , col = "green")
mtext( "Compare posteriors" )
```

<img src="/assignment/01-problem-set-solutions_files/figure-html/unnamed-chunk-3-1.png" width="672" />

``` r
plot( p_grid , post_prior , type="b" , col = "red",
    xlab="probability of water" , ylab="prior probability", ylim=c(0, 1) )
lines(p_grid , prior , type="b" , col = "green")
mtext( "Compare priors" )
```

<img src="/assignment/01-problem-set-solutions_files/figure-html/unnamed-chunk-4-1.png" width="672" />

``` r
plot( p_grid , post_likelihood , type="b" , col = "red",
    xlab="probability of water" , ylab="prior likelihood", ylim=c(0, .4) )
lines(p_grid , likelihood , type="b" , col = "green")
mtext( "Compare likelihoods" )
```

<img src="/assignment/01-problem-set-solutions_files/figure-html/unnamed-chunk-5-1.png" width="672" />

4.  For the posterior distribution from 3, compute 89% percentile and HPDI intervals. Compare the widths of these intervals. Which is wider? Why? If you had only the information in the interval, what might you misunderstand about the shape of the posterior distribution?

``` r
library(rethinking)
#calculate samples of post_posterior
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob=post_posterior)

# 89 percentile
PI(samples, prob=.89)
```

``` language-r
##        5%       94% 
## 0.5245245 0.8798799
```

``` r
# HPDI
HPDI(samples, prob=.89)
```

``` language-r
##     |0.89     0.89| 
## 0.5005005 0.8388388
```

HPDI is densest (narrowest) region with 89% mass, thus PI is wider than HPDI.

With only intervals, an analyst would miss the drop in the posterior’s shape below 52% due to the prior that assumes greater than 50%.

OPTIONAL CHALLENGE. Suppose there is bias in sampling so that Land is more likely than Water to be recorded. Specifically, assume that 1-in-5 (20%) of Water samples are accidentally recorded instead as “Land.” First, write a generative simulation of this sampling process. Assuming the true proportion of Water is 0.70, what proportion does your simulation tend to produce instead?

``` r
# Pr(W|W) = 0.8
# Pr(W|L) = 0.2
# Pr(W) = 0.7*0.8
set.seed(100)
true_prob = 0.7
bias = 0.2
N=10000

# assume 1 = water, 0 = land
trueW <- rbinom(N,size=20,prob=true_prob)
obsW <- rbinom(N,size=trueW,prob=1-bias)
mean(obsW/20)
```

``` language-r
## [1] 0.560355
```

``` r
# or
W <- rbinom(N,size=20,prob=true_prob*(1-bias))

mean(W/20)
```

``` language-r
## [1] 0.560975
```

Second, using a simulated sample of 20 tosses, compute the unbiased posterior distribution of the true proportion of water.

``` r
# now analyze
# Pr(p|W,N) = Pr(W|p,N)Pr(p) / Z
# Pr(W|N,p) = Pr(W)Pr(W|W)

W <- rbinom(1,size=20,prob=0.7*0.8)
# grid approx
grid_p <- seq(from=0,to=1,len=100)
pr_p <- dbeta(grid_p,1,1)
prW <- dbinom(W,20,grid_p*0.8)
post <- prW*pr_p

post_bad <- dbinom(W,20,grid_p)

plot(grid_p,post,type="l",lwd=4,xlab="proportion water",ylab="plausibility")
lines(grid_p,post_bad,col=2,lwd=4)
```

<img src="/assignment/01-problem-set-solutions_files/figure-html/unnamed-chunk-9-1.png" width="672" />

------------------------------------------------------------------------

# tidyverse

2.  Suppose the globe tossing data (Chapter 2) had turned out to be 4 water and 11 land. Construct the posterior distribution, using grid approximation. Use the same flat prior as in the book.

``` r
# tidyverse approach
library(tidyverse)
# how many grid points would you like?
n <- 1000
n_success <- 4
n_trials  <- 15

d <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = n),
         # note we're still using a flat uniform prior
         prior  = 1) %>% 
  mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior = (likelihood * prior) / sum(likelihood * prior))

d %>%
  pivot_longer(-p_grid) %>%
  filter(name == "posterior") %>%
  ggplot(aes(x = p_grid, y = value)) +
  geom_line() +
  ggtitle("Posterior (Flat prior): 4 out of 15") +
  theme_bw()
```

<img src="/assignment/01-problem-set-solutions_files/figure-html/unnamed-chunk-10-1.png" width="672" />

3.  Now suppose the data are 4 water and 2 land. Compute the posterior again, but this time use a prior that is zero below p = 0.5 and a constant above p = 0.5. This corresponds to prior information that a majority of the Earth’s surface is water. Compare the new posterior with the previous posterior (flat)

``` r
# how many grid points would you like?
n <- 1000
n_success <- 4
n_trials  <- 6

d <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = n)) %>% 
  mutate(prior = if_else(p_grid > 0.5, 0.3, 0)) %>%
  mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior = (likelihood * prior) / sum(likelihood * prior))

d %>%
  pivot_longer(-p_grid) %>%
  filter(name == "posterior") %>%
  ggplot(aes(x = p_grid, y = value)) +
  geom_line() +
  ggtitle("Posterior (>50% prior): 4 out of 15") +
  theme_bw()
```

<img src="/assignment/01-problem-set-solutions_files/figure-html/unnamed-chunk-11-1.png" width="672" />

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
##  [1] forcats_0.5.1        stringr_1.4.0        dplyr_1.0.7         
##  [4] purrr_0.3.4          readr_2.0.2          tidyr_1.1.4         
##  [7] tibble_3.1.6         tidyverse_1.3.1      rethinking_2.21     
## [10] cmdstanr_0.4.0.9001  rstan_2.21.3         ggplot2_3.3.5       
## [13] StanHeaders_2.21.0-7
## 
## loaded via a namespace (and not attached):
##  [1] matrixStats_0.61.0   fs_1.5.0             lubridate_1.8.0     
##  [4] httr_1.4.2           tensorA_0.36.2       tools_4.1.1         
##  [7] backports_1.4.1      bslib_0.3.1          utf8_1.2.2          
## [10] R6_2.5.1             DBI_1.1.1            colorspace_2.0-2    
## [13] withr_2.4.3          tidyselect_1.1.1     gridExtra_2.3       
## [16] prettyunits_1.1.1    processx_3.5.2       compiler_4.1.1      
## [19] rvest_1.0.2          cli_3.1.0            xml2_1.3.2          
## [22] labeling_0.4.2       bookdown_0.24        posterior_1.1.0     
## [25] sass_0.4.0           scales_1.1.1         checkmate_2.0.0     
## [28] mvtnorm_1.1-3        callr_3.7.0          bsplus_0.1.3        
## [31] digest_0.6.29        rmarkdown_2.11       base64enc_0.1-3     
## [34] pkgconfig_2.0.3      htmltools_0.5.2      dbplyr_2.1.1        
## [37] fastmap_1.1.0        highr_0.9            readxl_1.3.1        
## [40] rlang_0.4.12         rstudioapi_0.13      shape_1.4.6         
## [43] jquerylib_0.1.4      farver_2.1.0         generics_0.1.1      
## [46] jsonlite_1.7.2       distributional_0.2.2 inline_0.3.19       
## [49] magrittr_2.0.1       loo_2.4.1            Rcpp_1.0.7          
## [52] munsell_0.5.0        fansi_0.5.0          abind_1.4-5         
## [55] lifecycle_1.0.1      stringi_1.7.6        yaml_2.2.1          
## [58] MASS_7.3-54          pkgbuild_1.3.1       grid_4.1.1          
## [61] crayon_1.4.2         lattice_0.20-44      haven_2.4.3         
## [64] hms_1.1.1            knitr_1.36           ps_1.6.0            
## [67] pillar_1.6.4         uuid_1.0-3           codetools_0.2-18    
## [70] stats4_4.1.1         reprex_2.0.1         glue_1.6.0          
## [73] evaluate_0.14        blogdown_1.5         modelr_0.1.8        
## [76] renv_0.14.0          RcppParallel_5.1.4   tzdb_0.1.2          
## [79] vctrs_0.3.8          cellranger_1.1.0     gtable_0.3.0        
## [82] assertthat_0.2.1     xfun_0.28            mime_0.12           
## [85] broom_0.7.9          coda_0.19-4          ellipsis_0.3.2      
## [88] downloadthis_0.2.1   xaringanExtra_0.5.5
```
