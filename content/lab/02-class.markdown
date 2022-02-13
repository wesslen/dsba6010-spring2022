---
title: "Class 2"
date: "2022-02-12"
menu:
  example:
    parent: Labs
weight: 2
type: docs
toc: yes
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiAiQ2xhc3MgMiIKZGF0ZTogImByIFN5cy5EYXRlKClgIgotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQpgYGAKCiMjIEludHJvZHVjdGlvbgoKRm9yIHRoaXMgY2xhc3MsIHdlJ2xsIHJldmlldyBjb2RlIGV4YW1wbGVzIGZvdW5kIGluIENoYXB0ZXIgMiBhbmQgMy4KClRoaXMgYXNzdW1lcyB0aGF0IHlvdSBoYXZlIGFscmVhZHkgaW5zdGFsbGVkIHRoZSBgcmV0aGlua2luZ2AgcGFja2FnZS4KCklmIHlvdSBuZWVkIGhlbHAsIGJlIHN1cmUgdG8gcmVtZW1iZXIgdGhlIHJlZmVyZW5jZXMgaW4gdGhlIFtSZXNvdXJjZXNdKC9yZXNvdXJjZS8pOgoKKiBbSW5zdGFsbGluZyBSL1JTdHVkaW9dKC9yZXNvdXJjZS9pbnN0YWxsLykKKiBbSW5zdGFsbGluZyBgcmV0aGlua2luZ2AgcGFja2FnZV0oL3Jlc291cmNlL2luc3RhbGwtcmV0aGlua2luZy8pCiogW1JtYXJrZG93bl0oL3Jlc291cmNlL3JtYXJrZG93bi8pCiogW1IgU3R5bGUgZ3VpZGVdKC9yZXNvdXJjZS9zdHlsZS8pCgojIyBDaGFwdGVyIDIKCiMjIyBCYXllc2lhbiBVcGRhdGluZzogR3JpZCBBcHByb3hpbWF0aW9uCgpMZXQncyBhc3N1bWUgd2UgaGF2ZSB0aGUgdGFibGUgaW4gMi4xCgpgYGB7cn0KIyMgUiBjb2RlIDIuMQp3YXlzIDwtIGMoIDAgLCAzICwgOCAsIDkgLCAwICkKd2F5cy9zdW0od2F5cykKYGBgCgpMZXQncyBjb21wdXRlIHRoZSBsaWtlbGlob29kIG9mIHNpeCBXJ3MgaW4gbmluZSB0b3NzZXMgKGFzc3VtaW5nIGEgNTAlIHByb2JhYmlsaXR5KToKCmBgYHtyfQojIyBSIGNvZGUgMi4yCmRiaW5vbSggNiAsIHNpemU9OSAsIHByb2I9MC41ICkKYGBgCgpXZSBjYW4gc2VlIGl0J3MgMTYuNCUuCgpCZSBzdXJlIHRvIGV4YW1pbmUgdGhlIGBkYmlub21gIGZ1bmN0aW9uIGJ5IHR5cGluZyBpbiBgP2RiaW5vbWAgYW5kIGV4cGxvcmluZyB0aGUgZG9jdW1lbnRhdGlvbi4gV2UnbGwgdXNlIHRoaXMgZnVuY3Rpb24gYSBsb3QgaW4gdGhpcyBjbGFzcy4KCk5leHQsIGxldCdzIGRlZmluZSBhIGdyaWQuIFRoaXMgaXMgcmVxdWlyZWQgd2hlbiB3ZSBhcmUgdXNpbmcgR3JpZCBBcHByb3hpbWF0aW9uIGZvciBvdXIgQmF5ZXNpYW4gY2FsY3VsYXRpb25zIChpLmUuLCB0byBlc3RpbWF0ZSB0aGUgcG9zdGVyaW9yKS4KCmBgYHtyfQojIyBSIGNvZGUgMi4zCiMgZGVmaW5lIGdyaWQKcF9ncmlkIDwtIHNlcSggZnJvbT0wICwgdG89MSAsIGxlbmd0aC5vdXQ9MjAgKQoKcF9ncmlkCmBgYAoKTm90aWNlIHRoYXQgdGhpcyBmdW5jdGlvbiBjcmVhdGVzIGEgdmVjdG9yIHdpdGggbGVuZ3RoIDIwIGFuZCB0aGF0IHJhbmdlcyBmcm9tIDAgdG8gMS4gTm90ZSBhcyB3ZWxsIHRoYXQgZWFjaCB2ZWN0b3IgZWxlbWVudCBpcyBldmVubHkgc3BhY2VkIGluIGluY3JlbWVudHMgb2YgYCh0byAtIGZyb20pLyhsZW5ndGgub3V0IC0gMSlgLgoKVGhpbmsgYWJvdXQgdGhlIHRyYWRlLW9mZiBiZXR3ZWVuIGhhdmluZyBhIHNtYWxsZXIgb3IgbGFyZ2VyIGBsZW5ndGgub3V0YC4KCk5leHQsIGxldCdzIGRlZmluZSBvdXIgcHJpb3IuIFdlJ2xsIGFzc3VtZSBhICJmbGF0IiBwcmlvci4gCgpgYGB7cn0KIyBkZWZpbmUgcHJpb3IKcHJpb3IgPC0gcmVwKCAxICwgMjAgKQoKcGxvdChwX2dyaWQsIHByaW9yLCB0eXBlPSJiIiwgeWxpbT1jKDAsNSkpCmBgYAoKYGBge3J9CiMgY29tcHV0ZSBsaWtlbGlob29kIGF0IGVhY2ggdmFsdWUgaW4gZ3JpZApsaWtlbGlob29kIDwtIGRiaW5vbSggNiAsIHNpemU9OSAsIHByb2I9cF9ncmlkICkKCnBsb3QocF9ncmlkLCBsaWtlbGlob29kLCB0eXBlPSJiIiwgeWxpbT1jKDAsMC4zKSkKYGBgCgpgYGB7cn0KIyBjb21wdXRlIHByb2R1Y3Qgb2YgbGlrZWxpaG9vZCBhbmQgcHJpb3IKdW5zdGQucG9zdGVyaW9yIDwtIGxpa2VsaWhvb2QgKiBwcmlvcgoKcGxvdChwX2dyaWQsIHVuc3RkLnBvc3RlcmlvciwgdHlwZT0iYiIsIHlsaW09YygwLDAuMykpCmBgYAoKYGBge3J9CiMgc3RhbmRhcmRpemUgdGhlIHBvc3Rlcmlvciwgc28gaXQgc3VtcyB0byAxCnBvc3RlcmlvciA8LSB1bnN0ZC5wb3N0ZXJpb3IgLyBzdW0odW5zdGQucG9zdGVyaW9yKQoKIyMgUiBjb2RlIDIuNApwbG90KCBwX2dyaWQgLCBwb3N0ZXJpb3IgLCB0eXBlPSJiIiAsCiAgICB4bGFiPSJwcm9iYWJpbGl0eSBvZiB3YXRlciIgLCB5bGFiPSJwb3N0ZXJpb3IgcHJvYmFiaWxpdHkiICkKbXRleHQoICIyMCBwb2ludHMiICkKYGBgCgoKCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpQcmFjdGljZTogV2hhdCBoYXBwZW5zIGlmIHdlIGFsdGVyIHRoZSBwcmlvcnM/IFdoYXQgd2lsbCBiZSB0aGUgbmV3IHBvc3RlcmlvcnM/CgpBc3N1bWUgNiBXJ3MgYW5kIDMgTCdzICg5IHRvc3NlcykuIFBsb3QgdGhlIHBvc3RlcmlvciBhbmQgY29tcGFyZSB0aGVtIHRvIHVzaW5nIGEgdW5pZm9ybSBwcmlvci4KCmBgYHtyfQojIHByaW9yIDEKcHJpb3IgPC0gaWZlbHNlKCBwX2dyaWQgPCAwLjUgLCAwICwgMSApCmBgYAoKYGBge3J9CiMgcHJpb3IgMgpwcmlvciA8LSBleHAoIC01KmFicyggcF9ncmlkIC0gMC41ICkgKQpgYGAKCnt7JSAvY2FsbG91dCAlfX0KCiMjIyBCYXllc2lhbiBVcGRhdGluZzogUXVhZHJhdGljIEFwcHJveGltYXRpb24KCldlIGNhbiBhbHNvIHVzZSBxdWFkcmF0aWMgYXBwcm94aW1hdGlvbiwgd2hpY2ggaXMgZGlzY3Vzc2VkIG9uIHBhZ2UgNDIgb2YgQ2hhcHRlcjIuIFdlJ2xsIHVzZSBxdWFkcmF0aWMgYXBwcm94aW1hdGlvbiBhcHByb2FjaCBvdmVyIHRoZSBuZXh0IGZldyB3ZWVrcyBiZWZvcmUgbW92aW5nIHRvIE1DTUMgbWV0aG9kcyB2aWEgU3Rhbi4KCmBgYHtyfQojIyBSIGNvZGUgMi42CmxpYnJhcnkocmV0aGlua2luZykKZ2xvYmUucWEgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkYmlub20oIFcrTCAscCkgLCAgIyBiaW5vbWlhbCBsaWtlbGlob29kCiAgICAgICAgcCB+IGR1bmlmKDAsMSkgICAgICMgdW5pZm9ybSBwcmlvcgogICAgKSAsCiAgICBkYXRhPWxpc3QoVz02LEw9MykgKQoKZ2xvYmUucWEKYGBgCgpXZSBjYW4gYWxzbyB1c2UgdGhlIGBwcmVjaXNgIGZ1bmN0aW9uIHRvIHN1bW1hcml6ZSBwYXJhbWV0ZXIgZXN0aW1hdGVzLiBJIHJlY29tbWVuZCBydW5uaW5nIGA/cHJlY2lzYCB0byBsb29rIHVwIHBhcmFtZXRlcnMgYXNzb2NpYXRlZCB3aXRoIHRoaXMgZnVuY3Rpb24uCgpgYGB7cn0KIyBkaXNwbGF5IHN1bW1hcnkgb2YgcXVhZHJhdGljIGFwcHJveGltYXRpb24KcHJlY2lzKCBnbG9iZS5xYSApCmBgYAoKIyMjIEhvdyBkb2VzIGdyaWQgYXBwcm94aW1hdGlvbiBjb21wYXJlIHRvIGFuYWx5dGljYWwgcG9zdGVyaW9yIGNhbGN1bGF0aW9uPwoKYGBge3J9CiMjIFIgY29kZSAyLjcKIyBhbmFseXRpY2FsIGNhbGN1bGF0aW9uClcgPC0gNgpMIDwtIDMKY3VydmUoIGRiZXRhKCB4ICwgVysxICwgTCsxICkgLCBmcm9tPTAgLCB0bz0xICwgY29sID0gMSkgIyBzb2xpZCBsaW5lCiMgcXVhZHJhdGljIGFwcHJveGltYXRpb24KY3VydmUoIGRub3JtKCB4ICwgMC42NyAsIDAuMTYgKSAsIGx0eT0yICwgYWRkPVRSVUUgLCBjb2wgPSAyKSAjIGRvdHRlZCBsaW5lCmBgYAoKCiMjIyBEZW1vIFByb2JsZW1zCgoyTTE6IFJlY2FsbCB0aGUgZ2xvYmUgdG9zc2luZyBtb2RlbCBmcm9tIHRoZSBjaGFwdGVyLiBDb21wdXRlIGFuZCBwbG90IHRoZSBncmlkIGFwcHJveGltYXRlIHBvc3RlcmlvciBkaXN0cmlidXRpb24gZm9yIGVhY2ggb2YgdGhlIGZvbGxvd2luZyBzZXRzIG9mIG9ic2VydmF0aW9ucy4gSW4gZWFjaCBjYXNlLCBhc3N1bWUgYSB1bmlmb3JtIHByaW9yIGZvciBwLgoKYGBge3J9CnBfZ3JpZCA8LSBzZXEoIGZyb209MCAsIHRvPTEgLCBsZW5ndGgub3V0PTEwMCApICMgZ3JpZCBmcm9tIDAgdG8gMSB3aXRoIGxlbmd0aCAxMDAKcHJpb3IgPC0gcmVwKDEsMTAwKSAjIHVuaWZvcm0gcHJpb3IKCiMgbGlrZWxpaG9vZCBvZiAzIHdhdGVyIGluIDMgdG9zc2VzCmxpa2VsaWhvb2QgPC0gZGJpbm9tKCAzICwgc2l6ZT0zICwgcHJvYj1wX2dyaWQgKQoKcG9zdGVyaW9yIDwtIGxpa2VsaWhvb2QgKiBwcmlvcgpwb3N0ZXJpb3IgPC0gcG9zdGVyaW9yIC8gc3VtKHBvc3RlcmlvcikgIyBzdGFuZGFyZGl6ZQoKcGxvdCggcG9zdGVyaW9yIH4gcF9ncmlkICwgdHlwZT0ibCIsIG1haW4gPSAiVywgVywgVyIpCmBgYAoKYGBge3J9CiMgbGlrZWxpaG9vZCBvZiAzIHdhdGVyIGluIDQgdG9zc2VzCmxpa2VsaWhvb2QgPC0gZGJpbm9tKCAzICwgc2l6ZT00ICwgcHJvYj1wX2dyaWQgKQoKcG9zdGVyaW9yIDwtIGxpa2VsaWhvb2QgKiBwcmlvcgpwb3N0ZXJpb3IgPC0gcG9zdGVyaW9yIC8gc3VtKHBvc3RlcmlvcikgIyBzdGFuZGFyZGl6ZQoKcGxvdCggcG9zdGVyaW9yIH4gcF9ncmlkICwgdHlwZT0ibCIgLCBtYWluID0gIlcsIFcsIFcsIEwiKQpgYGAKCmBgYHtyfQojIGxpa2VsaWhvb2Qgb2YgNSB3YXRlciBpbiA3IHRvc3NlcwpsaWtlbGlob29kIDwtIGRiaW5vbSggNSAsIHNpemU9NyAsIHByb2I9cF9ncmlkICkKCnBvc3RlcmlvciA8LSBsaWtlbGlob29kICogcHJpb3IKcG9zdGVyaW9yIDwtIHBvc3RlcmlvciAvIHN1bShwb3N0ZXJpb3IpICMgc3RhbmRhcmRpemUKCnBsb3QoIHBvc3RlcmlvciB+IHBfZ3JpZCAsIHR5cGU9ImwiICwgbWFpbiA9ICJMLCBXLCBXLCBXLCBMLCBXLCBXLCBXIikKYGBgCgojIyBDaGFwdGVyIDMKCkFzc3VtZSB3ZSBoYXZlIHRoZSBmb2xsb3dpbmcgbW9kZWw6CgpgYGB7cn0KcF9ncmlkIDwtIHNlcShmcm9tID0gMCwgdG8gPSAxLCBsZW5ndGgub3V0ID0gMTAwMCkKcHJpb3IgPC0gcmVwKDEsIDEwMDApCmxpa2VsaWhvb2QgPC0gZGJpbm9tKDYsIHNpemUgPSA5LCBwcm9iID0gcF9ncmlkKQpwb3N0ZXJpb3IgPC0gbGlrZWxpaG9vZCAqIHByaW9yCnBvc3RlcmlvciA8LSBwb3N0ZXJpb3IgLyBzdW0ocG9zdGVyaW9yKQoKc2V0LnNlZWQoMTAwKSAjIHZlcnkgaW1wb3J0YW50IHdoZW4gdXNpbmcgcmFuZG9taXplZCBmdW5jdGlvbnMgKGUuZy4sIHNhbXBsZSkKc2FtcGxlcyA8LSBzYW1wbGUocF9ncmlkLCBwcm9iID0gcG9zdGVyaW9yLCBzaXplID0gMWU0LCByZXBsYWNlID0gVFJVRSkKYGBgCgojIyMgRGVtbyBQcm9ibGVtcwoKTGV0J3MgZm9sbG93IHdvcmsgaW4gc2VjdGlvbiAzLjIgdG8gdW5kZXJzdGFuZCBob3cgdG8gc3VtbWFyaXplIGluZm9ybWF0aW9uIGZyb20gdGhlIHBvc3Rlcmlvci4KCjNFMTogSG93IG11Y2ggcG9zdGVyaW9yIHByb2JhYmlsaXR5IGxpZXMgKipiZWxvdyoqIHAgPSAwLjI/CgpgYGB7cn0KbWVhbihzYW1wbGVzIDwgMC4yKQpgYGAKCjNFMjogSG93IG11Y2ggcG9zdGVyaW9yIHByb2JhYmlsaXR5IGxpZXMgKiphYm92ZSoqIHAgPSAwLjg/CgpgYGB7cn0KbWVhbihzYW1wbGVzID4gMC44KQpgYGAKCjNFMzogSG93IG11Y2ggcG9zdGVyaW9yIHByb2JhYmlsaXR5IGxpZXMgKipiZXR3ZWVuKiogcCA9IDAuMiBhbmQgcCA9IDAuOD8KCmBgYHtyfQpzdW0oIHNhbXBsZXMgPiAwLjIgJiBzYW1wbGVzIDwgMC44ICkgLyAxZTQKYGBgCgozRTQ6IDIwJSBvZiB0aGUgcG9zdGVyaW9yIHByb2JhYmlsaXR5IGxpZXMgKipiZWxvdyoqIHdoaWNoIHZhbHVlIG9mIHA/CgpgYGB7cn0KcXVhbnRpbGUoc2FtcGxlcywgcHJvYnMgPSAwLjIpCmBgYAoKM0U1OiAyMCUgb2YgdGhlIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBsaWVzICoqYWJvdmUqKiB3aGljaCB2YWx1ZSBvZiBwPwoKYGBge3J9CnF1YW50aWxlKHNhbXBsZXMsIHByb2JzID0gMC44KQpgYGAKCjNFNjogV2hpY2ggdmFsdWVzIG9mIHAgY29udGFpbiB0aGUgbmFycm93ZXN0IGludGVydmFsIGVxdWFsIHRvIDY2JSBvZiB0aGUgcG9zdGVyaW9yIHByb2JhYmlsaXR5PwoKYGBge3J9CkhQREkoc2FtcGxlcywgcHJvYiA9IDAuNjYpCmBgYAoKM0U3OiBXaGljaCB2YWx1ZXMgb2YgcCBjb250YWluIDY2JSBvZiB0aGUgcG9zdGVyaW9yIHByb2JhYmlsaXR5LCBhc3N1bWluZyBlcXVhbCBwb3N0ZXJpb3IgcHJvYmFiaWxpdHkgYm90aCBiZWxvdyBhbmQgYWJvdmUgdGhlIGludGVydmFsPwoKYGBge3J9ClBJKHNhbXBsZXMsIHByb2IgPSAwLjY2KQpgYGAKCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpDb21wcmVoZW5zaW9uIHF1ZXN0aW9uOiB1bmRlciB3aGF0IGNpcmN1bXN0YW5jZXMgd291bGQgdGhlIFBJIGRpZmZlciBmcm9tIHRoZSBIUERJIChob2xkaW5nIGludGVydmFsIHZhbHVlIHRoZSBzYW1lKT8KCnt7JSAvY2FsbG91dCAlfX0KCjNNMTogU3VwcG9zZSB0aGUgZ2xvYmUgdG9zc2luZyBkYXRhIGhhZCB0dXJuZWQgb3V0IHRvIGJlIDggd2F0ZXIgaW4gMTUgdG9zc2VzLiBDb25zdHJ1Y3RlIHRoZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uLCB1c2luZyBncmlkIGFwcHJveGltYXRpb24uIFVzZSB0aGUgc2FtZSBmbGF0IHByaW9yIGFzIGJlZm9yZS4KCmBgYHtyfQpwX2dyaWQgPC0gc2VxKGZyb20gPSAwLCB0byA9IDEsIGxlbmd0aC5vdXQgPSAxMDAwKQpwcmlvciA8LSByZXAoMSwgMTAwMCkKbGlrZWxpaG9vZCA8LSBkYmlub20oOCwgc2l6ZSA9IDE1LCBwcm9iID0gcF9ncmlkKQpwb3N0ZXJpb3IgPC0gbGlrZWxpaG9vZCAqIHByaW9yCnBvc3RlcmlvciA8LSBwb3N0ZXJpb3IgLyBzdW0ocG9zdGVyaW9yKQoKcGxvdChwb3N0ZXJpb3IpCmBgYAoKM00yLiBEcmF3IDEwLDAwMCBzYW1wbGVzIGZyb20gdGhlIGdyaWQgYXBwcm94aW1hdGlvbiBmcm9tIGFib3ZlLiBUaGVuIHVzZSB0aGUgc2FtcGxlIHRvIGNhbGN1bGF0ZSB0aGUgOTAlIEhQREkgZm9yIHAuCgpgYGB7cn0Kc2FtcGxlcyA8LSBzYW1wbGUocF9ncmlkLCBwcm9iID0gcG9zdGVyaW9yLCBzaXplID0gMWU0LCByZXBsYWNlID0gVFJVRSkKCkhQREkoc2FtcGxlcywgcHJvYiA9IDAuOSkKYGBgCgozTTMuIENvbnN0cnVjdCBhICoqcG9zdGVyaW9yIHByZWRpY3RpdmUgY2hlY2sqKiBmb3IgdGhpcyBtb2RlbCBhbmQgZGF0YS4gVGhlIG1lYW5zIHNpbXVsYXRlIHRoZSBkaXN0cmlidXRpb24gb2Ygc2FtcGxlcywgYXZlcmFnaW5nIG92ZXIgdGhlIHBvc3RlcmlvciB1bmNlcnRhaW50eSBpbiBwLiBXaGF0IGlzIHRoZSBwcm9iYWJpbGl0eSBvZiBvYnNlcnZpbmcgOCB3YXRlciBpbiAxNSB0b3NzZXM/CgpgYGB7cn0KdyA8LSByYmlub20oMWU0LCBzaXplID0gMTUsIHByb2IgPSBzYW1wbGVzKQptZWFuKHcgPT0gOCkKYGBgCgozTTQ6IFVzaW5nIHRoZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uIGNvbnN0cnVjdGVkIGZyb20gdGhlIG5ldyAoOC8xNSkgZGF0YSwgbm93IGNhbGN1bGF0ZSB0aGUgcHJvYmFiaWxpdHkgb2Ygb2JzZXJ2aW5nIDYgd2F0ZXIgaW4gOSB0b3NzZXMuCgpgYGB7cn0KdyA8LSByYmlub20oMWU0LCBzaXplID0gOSwgcHJvYiA9IHNhbXBsZXMpCm1lYW4odyA9PSA2KQpgYGAKCnt7JSBjYWxsb3V0IG5vdGUgJX19CgpNb2RpZnkgdGhlIHZhbHVlcyB3ICgwIHRvIDkpIGZvciB0aGUgc2l6ZSA9IDkgZXhhbXBsZSBpbiAzTTQuIENvbXBhcmUgdGhlc2UgdmFsdWVzIHRvIEZpZ3VyZSAzLjYuCgp7eyUgL2NhbGxvdXQgJX19CgojIyBBcHBlbmRpeDogYHRpZHl2ZXJzZWAgY29udmVyc2lvbgoKU3RhdGlzdGljYWwgUmV0aGlua2luZyB1c2VzIGJhc2UgUiBmdW5jdGlvbnMuIE1vcmUgcmVjZW50bHksIFNvbG9tYW4gS3VyeiBoYXMgY3JlYXRlZCBhIFt0cmFuc2xhdGlvbiBvZiB0aGUgYm9vaydzIGZ1bmN0aW9uc10oaHR0cHM6Ly9ib29rZG93bi5vcmcvY29udGVudC80ODU3LykgaW50byBgdGlkeXZlcnNlYCAoYW5kIGxhdGVyIGBicm1zYCkgY29kZS4gVGhpcyBpcyBub3QgbmVjZXNzYXJ5IGJ1dCBjb3VsZCBiZSBleHRyZW1lbHkgaGVscGZ1bCB0byBjbGFzc21hdGVzIHdobyBhcmUgZmFtaWxpYXIgd2l0aCBgdGlkeXZlcnNlYCBhbHJlYWR5LgoKRmlyc3QsIHdlJ2xsIG5lZWQgdG8gY2FsbCBgdGlkeXZlcnNlYC4gSWYgeW91IGRvIG5vdCBoYXZlIGB0aWR5dmVyc2VgLCB5b3UnbGwgbmVlZCB0byBpbnN0YWxsIGl0LgoKYGBge3J9CmxpYnJhcnkodGlkeXZlcnNlKQpgYGAKCkZvciBleGFtcGxlLCB3ZSBjYW4gdHJhbnNsYXRlIDIuMyBjb2RlIHVzaW5nIHBpcGVzIChgJT4lYCkKCmBgYHtyfQpkIDwtIHRpYmJsZShwX2dyaWQgPSBzZXEoZnJvbSA9IDAsIHRvID0gMSwgbGVuZ3RoLm91dCA9IDIwKSwgICAgICAjIGRlZmluZSBncmlkCiAgICAgICAgICAgcHJpb3IgID0gMSkgJT4lICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIyBkZWZpbmUgcHJpb3IKICAgIG11dGF0ZShsaWtlbGlob29kID0gZGJpbm9tKDYsIHNpemUgPSA5LCBwcm9iID0gcF9ncmlkKSkgJT4lICAjIGNvbXB1dGUgbGlrZWxpaG9vZCBhdCBlYWNoIHZhbHVlIGluIGdyaWQKICAgIG11dGF0ZSh1bnN0ZF9wb3N0ZXJpb3IgPSBsaWtlbGlob29kICogcHJpb3IpICU+JSAgICAgICAgICAgICAjIGNvbXB1dGUgcHJvZHVjdCBvZiBsaWtlbGlob29kIGFuZCBwcmlvcgogICAgbXV0YXRlKHBvc3RlcmlvciA9IHVuc3RkX3Bvc3RlcmlvciAvIHN1bSh1bnN0ZF9wb3N0ZXJpb3IpKSAKCmQKYGBgCldpdGggdGhpcyBjYWxjdWxhdGVkLCB3ZSBjYW4gdGhlbiB1c2UgYGdncGxvdDJgLCB0aGUgc3RhcGxlIGBnZ3Bsb3QyYCBkYXRhIHZpc3VhbGl6YXRpb24gcGFja2FnZSwgdG8gcGxvdCBvdXIgcG9zdGVyaW9yLgoKYGBge3J9CmQgJT4lIAogIGdncGxvdChhZXMoeCA9IHBfZ3JpZCwgeSA9IHBvc3RlcmlvcikpICsKICBnZW9tX3BvaW50KCkgKwogIGdlb21fbGluZSgpICsKICBsYWJzKHN1YnRpdGxlID0gIjIwIHBvaW50cyIsCiAgICAgICB4ID0gInByb2JhYmlsaXR5IG9mIHdhdGVyIiwKICAgICAgIHkgPSAicG9zdGVyaW9yIHByb2JhYmlsaXR5IikgKwogIHRoZW1lKHBhbmVsLmdyaWQgPSBlbGVtZW50X2JsYW5rKCkpCmBgYAoKRm9yIHRoaXMgY2xhc3MsIHdlJ2xsIG9jY2FzaW9uYWxseSByZWZlciB0byBTb2xvbWFuJ3MgZ3VpZGUuIAoKIyMjIERlbW8gUHJvYmxlbQoKMk0xOiBSZWNhbGwgdGhlIGdsb2JlIHRvc3NpbmcgbW9kZWwgZnJvbSB0aGUgY2hhcHRlci4gQ29tcHV0ZSBhbmQgcGxvdCB0aGUgZ3JpZCBhcHByb3hpbWF0ZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uIGZvciBlYWNoIG9mIHRoZSBmb2xsb3dpbmcgc2V0cyBvZiBvYnNlcnZhdGlvbnMuIEluIGVhY2ggY2FzZSwgYXNzdW1lIGEgdW5pZm9ybSBwcmlvciBmb3IgcC4KCmBgYHtyfQojIyBiZSBzdXJlIHRvIGhhdmUgdGlkeXZlcnNlIGluc3RhbGxlZCwgaS5lLiwgaW5zdGFsbC5wYWNrYWdlcygndGlkeXZlcnNlJykKbGlicmFyeSh0aWR5dmVyc2UpCgpkaXN0IDwtIHRpYmJsZShwX2dyaWQgPSBzZXEoZnJvbSA9IDAsIHRvID0gMSwgbGVuZ3RoLm91dCA9IDIwKSwKICAgICAgICAgICAgICAgcHJpb3IgPSByZXAoMSwgdGltZXMgPSAyMCkpICU+JQogIG11dGF0ZShsaWtlbGlob29kXzEgPSBkYmlub20oMywgc2l6ZSA9IDMsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzIgPSBkYmlub20oMywgc2l6ZSA9IDQsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzMgPSBkYmlub20oNSwgc2l6ZSA9IDcsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBhY3Jvc3Moc3RhcnRzX3dpdGgoImxpa2VsaWhvb2QiKSwgfiAueCAqIHByaW9yKSwKICAgICAgICAgYWNyb3NzKHN0YXJ0c193aXRoKCJsaWtlbGlob29kIiksIH4gLnggLyBzdW0oLngpKSkgJT4lCiAgcGl2b3RfbG9uZ2VyKGNvbHMgPSBzdGFydHNfd2l0aCgibGlrZWxpaG9vZCIpLCBuYW1lc190byA9ICJwYXR0ZXJuIiwKICAgICAgICAgICAgICAgdmFsdWVzX3RvID0gInBvc3RlcmlvciIpICU+JQogIHNlcGFyYXRlKHBhdHRlcm4sIGMoTkEsICJwYXR0ZXJuIiksIHNlcCA9ICJfIiwgY29udmVydCA9IFRSVUUpICU+JQogIG11dGF0ZShvYnMgPSBjYXNlX3doZW4ocGF0dGVybiA9PSAxTCB+ICJXLCBXLCBXIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gMkwgfiAiVywgVywgVywgTCIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDNMIH4gIkwsIFcsIFcsIEwsIFcsIFcsIFciKSkKCmdncGxvdChkaXN0LCBhZXMoeCA9IHBfZ3JpZCwgeSA9IHBvc3RlcmlvcikpICsKICBmYWNldF93cmFwKHZhcnMoZmN0X2lub3JkZXIob2JzKSksIG5yb3cgPSAxKSArCiAgZ2VvbV9saW5lKCkgKwogIGdlb21fcG9pbnQoKSArCiAgbGFicyh4ID0gIlByb3BvcnRpb24gV2F0ZXIgKHApIiwgeSA9ICJQb3N0ZXJpb3IgRGVuc2l0eSIpCmBgYAoKYGBge3J9CiMgVywgVywgVywgTCwgVywgVywgVwoKIyBjaGFsbGVuZ2U6IGZ1bmN0aW9uYWxpemUgdGhpcyB0byBnZW5lcmFsaXplIHRoaXMgZm9yIGFueSByZWFkIGluIHRvc3Mgc3RyaW5nIAoKZDJtMSA8LSB0aWJibGUocF9ncmlkID0gc2VxKGZyb20gPSAwLCB0byA9IDEsIGxlbmd0aC5vdXQgPSAyMCksCiAgICAgICAgICAgICAgIHByaW9yID0gcmVwKDEsIHRpbWVzID0gMjApKSAlPiUKICBtdXRhdGUoCiAgICAgICAgIGxpa2VsaWhvb2RfMSA9IGRiaW5vbSgxLCBzaXplID0gMSwgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfMiA9IGRiaW5vbSgyLCBzaXplID0gMiwgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfMyA9IGRiaW5vbSgzLCBzaXplID0gMywgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfNCA9IGRiaW5vbSgzLCBzaXplID0gNCwgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfNSA9IGRiaW5vbSg0LCBzaXplID0gNSwgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfNiA9IGRiaW5vbSg1LCBzaXplID0gNiwgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGxpa2VsaWhvb2RfNyA9IGRiaW5vbSg2LCBzaXplID0gNywgcHJvYiA9IHBfZ3JpZCksCiAgICAgICAgIGFjcm9zcyhzdGFydHNfd2l0aCgibGlrZWxpaG9vZCIpLCB+IC54ICogcHJpb3IpLAogICAgICAgICBhY3Jvc3Moc3RhcnRzX3dpdGgoImxpa2VsaWhvb2QiKSwgfiAueCAvIHN1bSgueCkpKSAlPiUKICBwaXZvdF9sb25nZXIoY29scyA9IHN0YXJ0c193aXRoKCJsaWtlbGlob29kIiksIG5hbWVzX3RvID0gInBhdHRlcm4iLAogICAgICAgICAgICAgICB2YWx1ZXNfdG8gPSAicG9zdGVyaW9yIikgJT4lCiAgc2VwYXJhdGUocGF0dGVybiwgYyhOQSwgInBhdHRlcm4iKSwgc2VwID0gIl8iLCBjb252ZXJ0ID0gVFJVRSkgJT4lCiAgbXV0YXRlKG9icyA9IGNhc2Vfd2hlbihwYXR0ZXJuID09IDFMIH4gIlciLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSAyTCB+ICJXLCBXIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gM0wgfiAiVywgVywgVywiLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSA0TCB+ICJXLCBXLCBXLCBMIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gNUwgfiAiVywgVywgVywgTCwgVyIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDZMIH4gIlcsIFcsIFcsIEwsIFcsIFciLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSA3TCB+ICJXLCBXLCBXLCBMLCBXLCBXLCBXIikpCgpkMm0xCmBgYAoKYGBge3J9CiMgYmUgc3VyZSB0byBpbnN0YWxsIGdnYW5pbWF0ZSwgaS5lLiwgcnVuIGluc3RhbGwucGFja2FnZXMoJ2dnYW5pbWF0ZScpCmxpYnJhcnkoZ2dhbmltYXRlKQoKYW5pbSA8LSBnZ3Bsb3QoZDJtMSwgYWVzKHggPSBwX2dyaWQsIHkgPSBwb3N0ZXJpb3IsIGdyb3VwID0gb2JzKSkgKyAKICBnZW9tX3BvaW50KCkgKwogIGdlb21fbGluZSgpICsgCiAgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gIm5vbmUiKSArCiAgdHJhbnNpdGlvbl9zdGF0ZXMob2JzLAogICAgICAgICAgICAgICAgICAgIHRyYW5zaXRpb25fbGVuZ3RoID0gMiwKICAgICAgICAgICAgICAgICAgICBzdGF0ZV9sZW5ndGggPSAxKSArCiAgbGFicyh4ID0gIlByb3BvcnRpb24gV2F0ZXIgKHApIiwgeSA9ICJQb3N0ZXJpb3IgUHJvYmFiaWxpdHkiKSArCiAgZ2d0aXRsZSgnVG9zcyBSZXN1bHQ6IHtjbG9zZXN0X3N0YXRlfScpICsgCiAgZW50ZXJfZmFkZSgpICsKICBleGl0X2ZhZGUoKQoKYW5pbWF0ZShhbmltLCBoZWlnaHQgPSA1MDAsIHdpZHRoID0gNjAwKQojYW5pbV9zYXZlKCIuLi8uLi9zdGF0aWMvaW1nL2V4YW1wbGUvV29ybGQtdG9zc2luZy1iYXllc2lhbi1jaGFwdGVyMi5naWYiKQpgYGAKCiFbXSguLi8uLi9pbWcvZXhhbXBsZS9Xb3JsZC10b3NzaW5nLWJheWVzaWFuLWNoYXB0ZXIyLmdpZikKCiMjIFBhY2thZ2UgdmVyc2lvbnMKCmBgYHtyfQpzZXNzaW9uSW5mbygpCmBgYA==" download="02-class.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010_examples" target="_blank"><img src="https://gitpod.io/button/open-in-gitpod.svg" style="display: block; margin: auto auto auto 0;" /></a>

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

<img src="/lab/02-class_files/figure-html/unnamed-chunk-6-1.png" width="672" />

``` r
# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )

plot(p_grid, likelihood, type="b", ylim=c(0,0.3))
```

<img src="/lab/02-class_files/figure-html/unnamed-chunk-7-1.png" width="672" />

``` r
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

plot(p_grid, unstd.posterior, type="b", ylim=c(0,0.3))
```

<img src="/lab/02-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

``` r
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## R code 2.4
plot( p_grid , posterior , type="b" ,
    xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )
```

<img src="/lab/02-class_files/figure-html/unnamed-chunk-9-1.png" width="672" />

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
## 0.6666663 
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
## p 0.6666663 0.1571339 0.4155361 0.9177966
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

<img src="/lab/02-class_files/figure-html/unnamed-chunk-14-1.png" width="672" />

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

<img src="/lab/02-class_files/figure-html/unnamed-chunk-15-1.png" width="672" />

``` r
# likelihood of 3 water in 4 tosses
likelihood <- dbinom( 3 , size=4 , prob=p_grid )

posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize

plot( posterior ~ p_grid , type="l" , main = "W, W, W, L")
```

<img src="/lab/02-class_files/figure-html/unnamed-chunk-16-1.png" width="672" />

``` r
# likelihood of 5 water in 7 tosses
likelihood <- dbinom( 5 , size=7 , prob=p_grid )

posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize

plot( posterior ~ p_grid , type="l" , main = "L, W, W, W, L, W, W, W")
```

<img src="/lab/02-class_files/figure-html/unnamed-chunk-17-1.png" width="672" />

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

<img src="/lab/02-class_files/figure-html/unnamed-chunk-26-1.png" width="672" />

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

<img src="/lab/02-class_files/figure-html/unnamed-chunk-32-1.png" width="672" />

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

<img src="/lab/02-class_files/figure-html/unnamed-chunk-33-1.png" width="672" />

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
