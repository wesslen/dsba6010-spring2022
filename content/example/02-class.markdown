---
title: "Class 2"
date: "2022-01-24"
menu:
  example:
    parent: Examples
weight: 2
type: docs
toc: yes
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:application/octet-stream;base64,LS0tCnRpdGxlOiAiQ2xhc3MgMiIKZGF0ZTogImByIFN5cy5EYXRlKClgIgptZW51OgogIGV4YW1wbGU6CiAgICBwYXJlbnQ6IEV4YW1wbGVzCndlaWdodDogMgp0eXBlOiBkb2NzCnRvYzogeWVzCi0tLQoKYGBge3Igc2V0dXAsIGluY2x1ZGU9RkFMU0V9CmtuaXRyOjpvcHRzX2NodW5rJHNldChlY2hvID0gVFJVRSwgY2xhc3Muc291cmNlPSJsYW5ndWFnZS1yIiwgY2xhc3Mub3V0cHV0PSJsYW5ndWFnZS1yIiwgbWVzc2FnZSA9IEZBTFNFLCB3YXJuaW5nID0gRkFMU0UpCnhhcmluZ2FuRXh0cmE6OnVzZV9jbGlwYm9hcmQoKQpsaWJyYXJ5KHJldGhpbmtpbmcpCmBgYAoKYGBge3IgZWNobz1GQUxTRX0KZG93bmxvYWR0aGlzOjpkb3dubG9hZF9maWxlKAogIHBhdGggPSAiMDItY2xhc3MuUm1hcmtkb3duIiwKICBvdXRwdXRfbmFtZSA9ICIwMi1jbGFzcyIsCiAgYnV0dG9uX2xhYmVsID0gIkRvd25sb2FkIHRoaXMgY29kZSIsCiAgYnV0dG9uX3R5cGUgPSAiZGFuZ2VyIiwKICBoYXNfaWNvbiA9IFRSVUUsCiAgaWNvbiA9ICJmYSBmYS1zYXZlIiwKICBzZWxmX2NvbnRhaW5lZCA9IEZBTFNFCikKYGBgCmBgYHtyIGVjaG89RkFMU0UsIGZpZy5hbGlnbj0ibGVmdCIsIGZpZy5saW5rPSdodHRwczovL2dpdHBvZC5pby8jaHR0cHM6Ly9naXRodWIuY29tL3dlc3NsZW4vZHNiYTYwMTBfZXhhbXBsZXMnfQprbml0cjo6aW5jbHVkZV9ncmFwaGljcyhwYXRoPSJodHRwczovL2dpdHBvZC5pby9idXR0b24vb3Blbi1pbi1naXRwb2Quc3ZnIikKYGBgCgojIyBJbnRyb2R1Y3Rpb24KCkZvciB0aGlzIGNsYXNzLCB3ZSdsbCByZXZpZXcgY29kZSBleGFtcGxlcyBmb3VuZCBpbiBDaGFwdGVyIDIuCgpUaGlzIGFzc3VtZXMgdGhhdCB5b3UgaGF2ZSBhbHJlYWR5IGluc3RhbGxlZCB0aGUgYHJldGhpbmtpbmdgIHBhY2thZ2UuCgpJZiB5b3UgbmVlZCBoZWxwLCBiZSBzdXJlIHRvIHJlbWVtYmVyIHRoZSByZWZlcmVuY2VzIGluIHRoZSBbUmVzb3VyY2VzXSgvcmVzb3VyY2UvKToKCiogW0luc3RhbGxpbmcgUi9SU3R1ZGlvXSgvcmVzb3VyY2UvaW5zdGFsbC8pCiogW0luc3RhbGxpbmcgYHJldGhpbmtpbmdgIHBhY2thZ2VdKC9yZXNvdXJjZS9pbnN0YWxsLXJldGhpbmtpbmcvKQoqIFtSbWFya2Rvd25dKC9yZXNvdXJjZS9ybWFya2Rvd24vKQoqIFtSIFN0eWxlIGd1aWRlXSgvcmVzb3VyY2Uvc3R5bGUvKQoKIyMgQ2hhcHRlciAyCgojIyMgQmF5ZXNpYW4gVXBkYXRpbmc6IEdyaWQgQXBwcm94aW1hdGlvbgoKTGV0J3MgYXNzdW1lIHdlIGhhdmUgdGhlIHRhYmxlIGluIDIuMQoKYGBge3J9CiMjIFIgY29kZSAyLjEKd2F5cyA8LSBjKCAwICwgMyAsIDggLCA5ICwgMCApCndheXMvc3VtKHdheXMpCmBgYAoKTGV0J3MgY29tcHV0ZSB0aGUgbGlrZWxpaG9vZCBvZiBzaXggVydzIGluIG5pbmUgdG9zc2VzIChhc3N1bWluZyBhIDUwJSBwcm9iYWJpbGl0eSk6CgpgYGB7cn0KIyMgUiBjb2RlIDIuMgpkYmlub20oIDYgLCBzaXplPTkgLCBwcm9iPTAuNSApCmBgYAoKV2UgY2FuIHNlZSBpdCdzIDE2LjQlLgoKQmUgc3VyZSB0byBleGFtaW5lIHRoZSBgZGJpbm9tYCBmdW5jdGlvbiBieSB0eXBpbmcgaW4gYD9kYmlub21gIGFuZCBleHBsb3JpbmcgdGhlIGRvY3VtZW50YXRpb24uIFdlJ2xsIHVzZSB0aGlzIGZ1bmN0aW9uIGEgbG90IGluIHRoaXMgY2xhc3MuCgpOZXh0LCBsZXQncyBkZWZpbmUgYSBncmlkLiBUaGlzIGlzIHJlcXVpcmVkIHdoZW4gd2UgYXJlIHVzaW5nIEdyaWQgQXBwcm94aW1hdGlvbiBmb3Igb3VyIEJheWVzaWFuIGNhbGN1bGF0aW9ucyAoaS5lLiwgdG8gZXN0aW1hdGUgdGhlIHBvc3RlcmlvcikuCgpgYGB7cn0KIyMgUiBjb2RlIDIuMwojIGRlZmluZSBncmlkCnBfZ3JpZCA8LSBzZXEoIGZyb209MCAsIHRvPTEgLCBsZW5ndGgub3V0PTIwICkKCnBfZ3JpZApgYGAKCk5vdGljZSB0aGF0IHRoaXMgZnVuY3Rpb24gY3JlYXRlcyBhIHZlY3RvciB3aXRoIGxlbmd0aCAyMCBhbmQgdGhhdCByYW5nZXMgZnJvbSAwIHRvIDEuIE5vdGUgYXMgd2VsbCB0aGF0IGVhY2ggdmVjdG9yIGVsZW1lbnQgaXMgZXZlbmx5IHNwYWNlZCBpbiBpbmNyZW1lbnRzIG9mIGAodG8gLSBmcm9tKS8obGVuZ3RoLm91dCAtIDEpYC4KClRoaW5rIGFib3V0IHRoZSB0cmFkZS1vZmYgYmV0d2VlbiBoYXZpbmcgYSBzbWFsbGVyIG9yIGxhcmdlciBgbGVuZ3RoLm91dGAuCgpOZXh0LCBsZXQncyBkZWZpbmUgb3VyIHByaW9yLiBXZSdsbCBhc3N1bWUgYSAiZmxhdCIgcHJpb3IuIAoKYGBge3J9CiMgZGVmaW5lIHByaW9yCnByaW9yIDwtIHJlcCggMSAsIDIwICkKCnBsb3QocF9ncmlkLCBwcmlvciwgdHlwZT0iYiIsIHlsaW09YygwLDUpKQpgYGAKCmBgYHtyfQojIGNvbXB1dGUgbGlrZWxpaG9vZCBhdCBlYWNoIHZhbHVlIGluIGdyaWQKbGlrZWxpaG9vZCA8LSBkYmlub20oIDYgLCBzaXplPTkgLCBwcm9iPXBfZ3JpZCApCgpwbG90KHBfZ3JpZCwgbGlrZWxpaG9vZCwgdHlwZT0iYiIsIHlsaW09YygwLDAuMykpCmBgYAoKYGBge3J9CiMgY29tcHV0ZSBwcm9kdWN0IG9mIGxpa2VsaWhvb2QgYW5kIHByaW9yCnVuc3RkLnBvc3RlcmlvciA8LSBsaWtlbGlob29kICogcHJpb3IKCnBsb3QocF9ncmlkLCB1bnN0ZC5wb3N0ZXJpb3IsIHR5cGU9ImIiLCB5bGltPWMoMCwwLjMpKQpgYGAKCmBgYHtyfQojIHN0YW5kYXJkaXplIHRoZSBwb3N0ZXJpb3IsIHNvIGl0IHN1bXMgdG8gMQpwb3N0ZXJpb3IgPC0gdW5zdGQucG9zdGVyaW9yIC8gc3VtKHVuc3RkLnBvc3RlcmlvcikKCiMjIFIgY29kZSAyLjQKcGxvdCggcF9ncmlkICwgcG9zdGVyaW9yICwgdHlwZT0iYiIgLAogICAgeGxhYj0icHJvYmFiaWxpdHkgb2Ygd2F0ZXIiICwgeWxhYj0icG9zdGVyaW9yIHByb2JhYmlsaXR5IiApCm10ZXh0KCAiMjAgcG9pbnRzIiApCmBgYAoKCgp7eyUgY2FsbG91dCBub3RlICV9fQoKUHJhY3RpY2U6IFdoYXQgaGFwcGVucyBpZiB3ZSBhbHRlciB0aGUgcHJpb3JzPyBXaGF0IHdpbGwgYmUgdGhlIG5ldyBwb3N0ZXJpb3JzPwoKQXNzdW1lIDYgVydzIGFuZCAzIEwncyAoOSB0b3NzZXMpLiBQbG90IHRoZSBwb3N0ZXJpb3IgYW5kIGNvbXBhcmUgdGhlbSB0byB1c2luZyBhIHVuaWZvcm0gcHJpb3IuCgpgYGB7cn0KIyBwcmlvciAxCnByaW9yIDwtIGlmZWxzZSggcF9ncmlkIDwgMC41ICwgMCAsIDEgKQpgYGAKCmBgYHtyfQojIHByaW9yIDIKcHJpb3IgPC0gZXhwKCAtNSphYnMoIHBfZ3JpZCAtIDAuNSApICkKYGBgCgp7eyUgL2NhbGxvdXQgJX19CgojIyMgQmF5ZXNpYW4gVXBkYXRpbmc6IFF1YWRyYXRpYyBBcHByb3hpbWF0aW9uCgpXZSBjYW4gYWxzbyB1c2UgcXVhZHJhdGljIGFwcHJveGltYXRpb24sIHdoaWNoIGlzIGRpc2N1c3NlZCBvbiBwYWdlIDQyIG9mIENoYXB0ZXIyLiBXZSdsbCB1c2UgcXVhZHJhdGljIGFwcHJveGltYXRpb24gYXBwcm9hY2ggb3ZlciB0aGUgbmV4dCBmZXcgd2Vla3MgYmVmb3JlIG1vdmluZyB0byBNQ01DIG1ldGhvZHMgdmlhIFN0YW4uCgpgYGB7cn0KIyMgUiBjb2RlIDIuNgpsaWJyYXJ5KHJldGhpbmtpbmcpCmdsb2JlLnFhIDwtIHF1YXAoCiAgICBhbGlzdCgKICAgICAgICBXIH4gZGJpbm9tKCBXK0wgLHApICwgICMgYmlub21pYWwgbGlrZWxpaG9vZAogICAgICAgIHAgfiBkdW5pZigwLDEpICAgICAjIHVuaWZvcm0gcHJpb3IKICAgICkgLAogICAgZGF0YT1saXN0KFc9NixMPTMpICkKCmdsb2JlLnFhCmBgYAoKV2UgY2FuIGFsc28gdXNlIHRoZSBgcHJlY2lzYCBmdW5jdGlvbiB0byBzdW1tYXJpemUgcGFyYW1ldGVyIGVzdGltYXRlcy4gSSByZWNvbW1lbmQgcnVubmluZyBgP3ByZWNpc2AgdG8gbG9vayB1cCBwYXJhbWV0ZXJzIGFzc29jaWF0ZWQgd2l0aCB0aGlzIGZ1bmN0aW9uLgoKYGBge3J9CiMgZGlzcGxheSBzdW1tYXJ5IG9mIHF1YWRyYXRpYyBhcHByb3hpbWF0aW9uCnByZWNpcyggZ2xvYmUucWEgKQpgYGAKCiMjIyBIb3cgZG9lcyBncmlkIGFwcHJveGltYXRpb24gY29tcGFyZSB0byBhbmFseXRpY2FsIHBvc3RlcmlvciBjYWxjdWxhdGlvbj8KCmBgYHtyfQojIyBSIGNvZGUgMi43CiMgYW5hbHl0aWNhbCBjYWxjdWxhdGlvbgpXIDwtIDYKTCA8LSAzCmN1cnZlKCBkYmV0YSggeCAsIFcrMSAsIEwrMSApICwgZnJvbT0wICwgdG89MSAsIGNvbCA9IDEpICMgc29saWQgbGluZQojIHF1YWRyYXRpYyBhcHByb3hpbWF0aW9uCmN1cnZlKCBkbm9ybSggeCAsIDAuNjcgLCAwLjE2ICkgLCBsdHk9MiAsIGFkZD1UUlVFICwgY29sID0gMikgIyBkb3R0ZWQgbGluZQpgYGAKCgojIyMgRGVtbyBQcm9ibGVtcwoKMk0xOiBSZWNhbGwgdGhlIGdsb2JlIHRvc3NpbmcgbW9kZWwgZnJvbSB0aGUgY2hhcHRlci4gQ29tcHV0ZSBhbmQgcGxvdCB0aGUgZ3JpZCBhcHByb3hpbWF0ZSBwb3N0ZXJpb3IgZGlzdHJpYnV0aW9uIGZvciBlYWNoIG9mIHRoZSBmb2xsb3dpbmcgc2V0cyBvZiBvYnNlcnZhdGlvbnMuIEluIGVhY2ggY2FzZSwgYXNzdW1lIGEgdW5pZm9ybSBwcmlvciBmb3IgcC4KCmBgYHtyfQpwX2dyaWQgPC0gc2VxKCBmcm9tPTAgLCB0bz0xICwgbGVuZ3RoLm91dD0xMDAgKSAjIGdyaWQgZnJvbSAwIHRvIDEgd2l0aCBsZW5ndGggMTAwCnByaW9yIDwtIHJlcCgxLDEwMCkgIyB1bmlmb3JtIHByaW9yCgojIGxpa2VsaWhvb2Qgb2YgMyB3YXRlciBpbiAzIHRvc3NlcwpsaWtlbGlob29kIDwtIGRiaW5vbSggMyAsIHNpemU9MyAsIHByb2I9cF9ncmlkICkKCnBvc3RlcmlvciA8LSBsaWtlbGlob29kICogcHJpb3IKcG9zdGVyaW9yIDwtIHBvc3RlcmlvciAvIHN1bShwb3N0ZXJpb3IpICMgc3RhbmRhcmRpemUKCnBsb3QoIHBvc3RlcmlvciB+IHBfZ3JpZCAsIHR5cGU9ImwiLCBtYWluID0gIlcsIFcsIFciKQpgYGAKCmBgYHtyfQojIGxpa2VsaWhvb2Qgb2YgMyB3YXRlciBpbiA0IHRvc3NlcwpsaWtlbGlob29kIDwtIGRiaW5vbSggMyAsIHNpemU9NCAsIHByb2I9cF9ncmlkICkKCnBvc3RlcmlvciA8LSBsaWtlbGlob29kICogcHJpb3IKcG9zdGVyaW9yIDwtIHBvc3RlcmlvciAvIHN1bShwb3N0ZXJpb3IpICMgc3RhbmRhcmRpemUKCnBsb3QoIHBvc3RlcmlvciB+IHBfZ3JpZCAsIHR5cGU9ImwiICwgbWFpbiA9ICJXLCBXLCBXLCBMIikKYGBgCgpgYGB7cn0KIyBsaWtlbGlob29kIG9mIDUgd2F0ZXIgaW4gNyB0b3NzZXMKbGlrZWxpaG9vZCA8LSBkYmlub20oIDUgLCBzaXplPTcgLCBwcm9iPXBfZ3JpZCApCgpwb3N0ZXJpb3IgPC0gbGlrZWxpaG9vZCAqIHByaW9yCnBvc3RlcmlvciA8LSBwb3N0ZXJpb3IgLyBzdW0ocG9zdGVyaW9yKSAjIHN0YW5kYXJkaXplCgpwbG90KCBwb3N0ZXJpb3IgfiBwX2dyaWQgLCB0eXBlPSJsIiAsIG1haW4gPSAiTCwgVywgVywgVywgTCwgVywgVywgVyIpCmBgYAoKIyMgQ2hhcHRlciAzCgpBc3N1bWUgd2UgaGF2ZSB0aGUgZm9sbG93aW5nIG1vZGVsOgoKYGBge3J9CnBfZ3JpZCA8LSBzZXEoZnJvbSA9IDAsIHRvID0gMSwgbGVuZ3RoLm91dCA9IDEwMDApCnByaW9yIDwtIHJlcCgxLCAxMDAwKQpsaWtlbGlob29kIDwtIGRiaW5vbSg2LCBzaXplID0gOSwgcHJvYiA9IHBfZ3JpZCkKcG9zdGVyaW9yIDwtIGxpa2VsaWhvb2QgKiBwcmlvcgpwb3N0ZXJpb3IgPC0gcG9zdGVyaW9yIC8gc3VtKHBvc3RlcmlvcikKCnNldC5zZWVkKDEwMCkgIyB2ZXJ5IGltcG9ydGFudCB3aGVuIHVzaW5nIHJhbmRvbWl6ZWQgZnVuY3Rpb25zIChlLmcuLCBzYW1wbGUpCnNhbXBsZXMgPC0gc2FtcGxlKHBfZ3JpZCwgcHJvYiA9IHBvc3Rlcmlvciwgc2l6ZSA9IDFlNCwgcmVwbGFjZSA9IFRSVUUpCmBgYAoKIyMjIERlbW8gUHJvYmxlbXMKCkxldCdzIGZvbGxvdyB3b3JrIGluIHNlY3Rpb24gMy4yIHRvIHVuZGVyc3RhbmQgaG93IHRvIHN1bW1hcml6ZSBpbmZvcm1hdGlvbiBmcm9tIHRoZSBwb3N0ZXJpb3IuCgozRTE6IEhvdyBtdWNoIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBsaWVzICoqYmVsb3cqKiBwID0gMC4yPwoKYGBge3J9Cm1lYW4oc2FtcGxlcyA8IDAuMikKYGBgCgozRTI6IEhvdyBtdWNoIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBsaWVzICoqYWJvdmUqKiBwID0gMC44PwoKYGBge3J9Cm1lYW4oc2FtcGxlcyA+IDAuOCkKYGBgCgozRTM6IEhvdyBtdWNoIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBsaWVzICoqYmV0d2VlbioqIHAgPSAwLjIgYW5kIHAgPSAwLjg/CgpgYGB7cn0Kc3VtKCBzYW1wbGVzID4gMC4yICYgc2FtcGxlcyA8IDAuOCApIC8gMWU0CmBgYAoKM0U0OiAyMCUgb2YgdGhlIHBvc3RlcmlvciBwcm9iYWJpbGl0eSBsaWVzICoqYmVsb3cqKiB3aGljaCB2YWx1ZSBvZiBwPwoKYGBge3J9CnF1YW50aWxlKHNhbXBsZXMsIHByb2JzID0gMC4yKQpgYGAKCjNFNTogMjAlIG9mIHRoZSBwb3N0ZXJpb3IgcHJvYmFiaWxpdHkgbGllcyAqKmFib3ZlKiogd2hpY2ggdmFsdWUgb2YgcD8KCmBgYHtyfQpxdWFudGlsZShzYW1wbGVzLCBwcm9icyA9IDAuOCkKYGBgCgozRTY6IFdoaWNoIHZhbHVlcyBvZiBwIGNvbnRhaW4gdGhlIG5hcnJvd2VzdCBpbnRlcnZhbCBlcXVhbCB0byA2NiUgb2YgdGhlIHBvc3RlcmlvciBwcm9iYWJpbGl0eT8KCmBgYHtyfQpIUERJKHNhbXBsZXMsIHByb2IgPSAwLjY2KQpgYGAKCjNFNzogV2hpY2ggdmFsdWVzIG9mIHAgY29udGFpbiA2NiUgb2YgdGhlIHBvc3RlcmlvciBwcm9iYWJpbGl0eSwgYXNzdW1pbmcgZXF1YWwgcG9zdGVyaW9yIHByb2JhYmlsaXR5IGJvdGggYmVsb3cgYW5kIGFib3ZlIHRoZSBpbnRlcnZhbD8KCmBgYHtyfQpQSShzYW1wbGVzLCBwcm9iID0gMC42NikKYGBgCgp7eyUgY2FsbG91dCBub3RlICV9fQoKQ29tcHJlaGVuc2lvbiBxdWVzdGlvbjogdW5kZXIgd2hhdCBjaXJjdW1zdGFuY2VzIHdvdWxkIHRoZSBQSSBkaWZmZXIgZnJvbSB0aGUgSFBESSAoaG9sZGluZyBpbnRlcnZhbCB2YWx1ZSB0aGUgc2FtZSk/Cgp7eyUgL2NhbGxvdXQgJX19CgozTTE6IFN1cHBvc2UgdGhlIGdsb2JlIHRvc3NpbmcgZGF0YSBoYWQgdHVybmVkIG91dCB0byBiZSA4IHdhdGVyIGluIDE1IHRvc3Nlcy4gQ29uc3RydWN0ZSB0aGUgcG9zdGVyaW9yIGRpc3RyaWJ1dGlvbiwgdXNpbmcgZ3JpZCBhcHByb3hpbWF0aW9uLiBVc2UgdGhlIHNhbWUgZmxhdCBwcmlvciBhcyBiZWZvcmUuCgpgYGB7cn0KcF9ncmlkIDwtIHNlcShmcm9tID0gMCwgdG8gPSAxLCBsZW5ndGgub3V0ID0gMTAwMCkKcHJpb3IgPC0gcmVwKDEsIDEwMDApCmxpa2VsaWhvb2QgPC0gZGJpbm9tKDgsIHNpemUgPSAxNSwgcHJvYiA9IHBfZ3JpZCkKcG9zdGVyaW9yIDwtIGxpa2VsaWhvb2QgKiBwcmlvcgpwb3N0ZXJpb3IgPC0gcG9zdGVyaW9yIC8gc3VtKHBvc3RlcmlvcikKCnBsb3QocG9zdGVyaW9yKQpgYGAKCjNNMi4gRHJhdyAxMCwwMDAgc2FtcGxlcyBmcm9tIHRoZSBncmlkIGFwcHJveGltYXRpb24gZnJvbSBhYm92ZS4gVGhlbiB1c2UgdGhlIHNhbXBsZSB0byBjYWxjdWxhdGUgdGhlIDkwJSBIUERJIGZvciBwLgoKYGBge3J9CnNhbXBsZXMgPC0gc2FtcGxlKHBfZ3JpZCwgcHJvYiA9IHBvc3Rlcmlvciwgc2l6ZSA9IDFlNCwgcmVwbGFjZSA9IFRSVUUpCgpIUERJKHNhbXBsZXMsIHByb2IgPSAwLjkpCmBgYAoKM00zLiBDb25zdHJ1Y3QgYSAqKnBvc3RlcmlvciBwcmVkaWN0aXZlIGNoZWNrKiogZm9yIHRoaXMgbW9kZWwgYW5kIGRhdGEuIFRoZSBtZWFucyBzaW11bGF0ZSB0aGUgZGlzdHJpYnV0aW9uIG9mIHNhbXBsZXMsIGF2ZXJhZ2luZyBvdmVyIHRoZSBwb3N0ZXJpb3IgdW5jZXJ0YWludHkgaW4gcC4gV2hhdCBpcyB0aGUgcHJvYmFiaWxpdHkgb2Ygb2JzZXJ2aW5nIDggd2F0ZXIgaW4gMTUgdG9zc2VzPwoKYGBge3J9CncgPC0gcmJpbm9tKDFlNCwgc2l6ZSA9IDE1LCBwcm9iID0gc2FtcGxlcykKbWVhbih3ID09IDgpCmBgYAoKM000OiBVc2luZyB0aGUgcG9zdGVyaW9yIGRpc3RyaWJ1dGlvbiBjb25zdHJ1Y3RlZCBmcm9tIHRoZSBuZXcgKDgvMTUpIGRhdGEsIG5vdyBjYWxjdWxhdGUgdGhlIHByb2JhYmlsaXR5IG9mIG9ic2VydmluZyA2IHdhdGVyIGluIDkgdG9zc2VzLgoKYGBge3J9CncgPC0gcmJpbm9tKDFlNCwgc2l6ZSA9IDksIHByb2IgPSBzYW1wbGVzKQptZWFuKHcgPT0gNikKYGBgCgp7eyUgY2FsbG91dCBub3RlICV9fQoKTW9kaWZ5IHRoZSB2YWx1ZXMgdyAoMCB0byA5KSBmb3IgdGhlIHNpemUgPSA5IGV4YW1wbGUgaW4gM000LiBDb21wYXJlIHRoZXNlIHZhbHVlcyB0byBGaWd1cmUgMy42LgoKe3slIC9jYWxsb3V0ICV9fQoKIyMgQXBwZW5kaXg6IGB0aWR5dmVyc2VgIGNvbnZlcnNpb24KClN0YXRpc3RpY2FsIFJldGhpbmtpbmcgdXNlcyBiYXNlIFIgZnVuY3Rpb25zLiBNb3JlIHJlY2VudGx5LCBTb2xvbWFuIEt1cnogaGFzIGNyZWF0ZWQgYSBbdHJhbnNsYXRpb24gb2YgdGhlIGJvb2sncyBmdW5jdGlvbnNdKGh0dHBzOi8vYm9va2Rvd24ub3JnL2NvbnRlbnQvNDg1Ny8pIGludG8gYHRpZHl2ZXJzZWAgKGFuZCBsYXRlciBgYnJtc2ApIGNvZGUuIFRoaXMgaXMgbm90IG5lY2Vzc2FyeSBidXQgY291bGQgYmUgZXh0cmVtZWx5IGhlbHBmdWwgdG8gY2xhc3NtYXRlcyB3aG8gYXJlIGZhbWlsaWFyIHdpdGggYHRpZHl2ZXJzZWAgYWxyZWFkeS4KCkZpcnN0LCB3ZSdsbCBuZWVkIHRvIGNhbGwgYHRpZHl2ZXJzZWAuIElmIHlvdSBkbyBub3QgaGF2ZSBgdGlkeXZlcnNlYCwgeW91J2xsIG5lZWQgdG8gaW5zdGFsbCBpdC4KCmBgYHtyfQpsaWJyYXJ5KHRpZHl2ZXJzZSkKYGBgCgpGb3IgZXhhbXBsZSwgd2UgY2FuIHRyYW5zbGF0ZSAyLjMgY29kZSB1c2luZyBwaXBlcyAoYCU+JWApCgpgYGB7cn0KZCA8LSB0aWJibGUocF9ncmlkID0gc2VxKGZyb20gPSAwLCB0byA9IDEsIGxlbmd0aC5vdXQgPSAyMCksICAgICAgIyBkZWZpbmUgZ3JpZAogICAgICAgICAgIHByaW9yICA9IDEpICU+JSAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICMgZGVmaW5lIHByaW9yCiAgICBtdXRhdGUobGlrZWxpaG9vZCA9IGRiaW5vbSg2LCBzaXplID0gOSwgcHJvYiA9IHBfZ3JpZCkpICU+JSAgIyBjb21wdXRlIGxpa2VsaWhvb2QgYXQgZWFjaCB2YWx1ZSBpbiBncmlkCiAgICBtdXRhdGUodW5zdGRfcG9zdGVyaW9yID0gbGlrZWxpaG9vZCAqIHByaW9yKSAlPiUgICAgICAgICAgICAgIyBjb21wdXRlIHByb2R1Y3Qgb2YgbGlrZWxpaG9vZCBhbmQgcHJpb3IKICAgIG11dGF0ZShwb3N0ZXJpb3IgPSB1bnN0ZF9wb3N0ZXJpb3IgLyBzdW0odW5zdGRfcG9zdGVyaW9yKSkgCgpkCmBgYApXaXRoIHRoaXMgY2FsY3VsYXRlZCwgd2UgY2FuIHRoZW4gdXNlIGBnZ3Bsb3QyYCwgdGhlIHN0YXBsZSBgZ2dwbG90MmAgZGF0YSB2aXN1YWxpemF0aW9uIHBhY2thZ2UsIHRvIHBsb3Qgb3VyIHBvc3Rlcmlvci4KCmBgYHtyfQpkICU+JSAKICBnZ3Bsb3QoYWVzKHggPSBwX2dyaWQsIHkgPSBwb3N0ZXJpb3IpKSArCiAgZ2VvbV9wb2ludCgpICsKICBnZW9tX2xpbmUoKSArCiAgbGFicyhzdWJ0aXRsZSA9ICIyMCBwb2ludHMiLAogICAgICAgeCA9ICJwcm9iYWJpbGl0eSBvZiB3YXRlciIsCiAgICAgICB5ID0gInBvc3RlcmlvciBwcm9iYWJpbGl0eSIpICsKICB0aGVtZShwYW5lbC5ncmlkID0gZWxlbWVudF9ibGFuaygpKQpgYGAKCkZvciB0aGlzIGNsYXNzLCB3ZSdsbCBvY2Nhc2lvbmFsbHkgcmVmZXIgdG8gU29sb21hbidzIGd1aWRlLiAKCiMjIyBEZW1vIFByb2JsZW0KCjJNMTogUmVjYWxsIHRoZSBnbG9iZSB0b3NzaW5nIG1vZGVsIGZyb20gdGhlIGNoYXB0ZXIuIENvbXB1dGUgYW5kIHBsb3QgdGhlIGdyaWQgYXBwcm94aW1hdGUgcG9zdGVyaW9yIGRpc3RyaWJ1dGlvbiBmb3IgZWFjaCBvZiB0aGUgZm9sbG93aW5nIHNldHMgb2Ygb2JzZXJ2YXRpb25zLiBJbiBlYWNoIGNhc2UsIGFzc3VtZSBhIHVuaWZvcm0gcHJpb3IgZm9yIHAuCgpgYGB7cn0KIyMgYmUgc3VyZSB0byBoYXZlIHRpZHl2ZXJzZSBpbnN0YWxsZWQsIGkuZS4sIGluc3RhbGwucGFja2FnZXMoJ3RpZHl2ZXJzZScpCmxpYnJhcnkodGlkeXZlcnNlKQoKZGlzdCA8LSB0aWJibGUocF9ncmlkID0gc2VxKGZyb20gPSAwLCB0byA9IDEsIGxlbmd0aC5vdXQgPSAyMCksCiAgICAgICAgICAgICAgIHByaW9yID0gcmVwKDEsIHRpbWVzID0gMjApKSAlPiUKICBtdXRhdGUobGlrZWxpaG9vZF8xID0gZGJpbm9tKDMsIHNpemUgPSAzLCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF8yID0gZGJpbm9tKDMsIHNpemUgPSA0LCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgbGlrZWxpaG9vZF8zID0gZGJpbm9tKDUsIHNpemUgPSA3LCBwcm9iID0gcF9ncmlkKSwKICAgICAgICAgYWNyb3NzKHN0YXJ0c193aXRoKCJsaWtlbGlob29kIiksIH4gLnggKiBwcmlvciksCiAgICAgICAgIGFjcm9zcyhzdGFydHNfd2l0aCgibGlrZWxpaG9vZCIpLCB+IC54IC8gc3VtKC54KSkpICU+JQogIHBpdm90X2xvbmdlcihjb2xzID0gc3RhcnRzX3dpdGgoImxpa2VsaWhvb2QiKSwgbmFtZXNfdG8gPSAicGF0dGVybiIsCiAgICAgICAgICAgICAgIHZhbHVlc190byA9ICJwb3N0ZXJpb3IiKSAlPiUKICBzZXBhcmF0ZShwYXR0ZXJuLCBjKE5BLCAicGF0dGVybiIpLCBzZXAgPSAiXyIsIGNvbnZlcnQgPSBUUlVFKSAlPiUKICBtdXRhdGUob2JzID0gY2FzZV93aGVuKHBhdHRlcm4gPT0gMUwgfiAiVywgVywgVyIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDJMIH4gIlcsIFcsIFcsIEwiLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSAzTCB+ICJMLCBXLCBXLCBMLCBXLCBXLCBXIikpCgpnZ3Bsb3QoZGlzdCwgYWVzKHggPSBwX2dyaWQsIHkgPSBwb3N0ZXJpb3IpKSArCiAgZmFjZXRfd3JhcCh2YXJzKGZjdF9pbm9yZGVyKG9icykpLCBucm93ID0gMSkgKwogIGdlb21fbGluZSgpICsKICBnZW9tX3BvaW50KCkgKwogIGxhYnMoeCA9ICJQcm9wb3J0aW9uIFdhdGVyIChwKSIsIHkgPSAiUG9zdGVyaW9yIERlbnNpdHkiKQpgYGAKCmBgYHtyfQojIFcsIFcsIFcsIEwsIFcsIFcsIFcKCiMgY2hhbGxlbmdlOiBmdW5jdGlvbmFsaXplIHRoaXMgdG8gZ2VuZXJhbGl6ZSB0aGlzIGZvciBhbnkgcmVhZCBpbiB0b3NzIHN0cmluZyAKCmQybTEgPC0gdGliYmxlKHBfZ3JpZCA9IHNlcShmcm9tID0gMCwgdG8gPSAxLCBsZW5ndGgub3V0ID0gMjApLAogICAgICAgICAgICAgICBwcmlvciA9IHJlcCgxLCB0aW1lcyA9IDIwKSkgJT4lCiAgbXV0YXRlKAogICAgICAgICBsaWtlbGlob29kXzEgPSBkYmlub20oMSwgc2l6ZSA9IDEsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzIgPSBkYmlub20oMiwgc2l6ZSA9IDIsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzMgPSBkYmlub20oMywgc2l6ZSA9IDMsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzQgPSBkYmlub20oMywgc2l6ZSA9IDQsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzUgPSBkYmlub20oNCwgc2l6ZSA9IDUsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzYgPSBkYmlub20oNSwgc2l6ZSA9IDYsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBsaWtlbGlob29kXzcgPSBkYmlub20oNiwgc2l6ZSA9IDcsIHByb2IgPSBwX2dyaWQpLAogICAgICAgICBhY3Jvc3Moc3RhcnRzX3dpdGgoImxpa2VsaWhvb2QiKSwgfiAueCAqIHByaW9yKSwKICAgICAgICAgYWNyb3NzKHN0YXJ0c193aXRoKCJsaWtlbGlob29kIiksIH4gLnggLyBzdW0oLngpKSkgJT4lCiAgcGl2b3RfbG9uZ2VyKGNvbHMgPSBzdGFydHNfd2l0aCgibGlrZWxpaG9vZCIpLCBuYW1lc190byA9ICJwYXR0ZXJuIiwKICAgICAgICAgICAgICAgdmFsdWVzX3RvID0gInBvc3RlcmlvciIpICU+JQogIHNlcGFyYXRlKHBhdHRlcm4sIGMoTkEsICJwYXR0ZXJuIiksIHNlcCA9ICJfIiwgY29udmVydCA9IFRSVUUpICU+JQogIG11dGF0ZShvYnMgPSBjYXNlX3doZW4ocGF0dGVybiA9PSAxTCB+ICJXIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gMkwgfiAiVywgVyIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDNMIH4gIlcsIFcsIFcsIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gNEwgfiAiVywgVywgVywgTCIsCiAgICAgICAgICAgICAgICAgICAgICAgICBwYXR0ZXJuID09IDVMIH4gIlcsIFcsIFcsIEwsIFciLAogICAgICAgICAgICAgICAgICAgICAgICAgcGF0dGVybiA9PSA2TCB+ICJXLCBXLCBXLCBMLCBXLCBXIiwKICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm4gPT0gN0wgfiAiVywgVywgVywgTCwgVywgVywgVyIpKQoKZDJtMQpgYGAKCmBgYHtyfQojIGJlIHN1cmUgdG8gaW5zdGFsbCBnZ2FuaW1hdGUsIGkuZS4sIHJ1biBpbnN0YWxsLnBhY2thZ2VzKCdnZ2FuaW1hdGUnKQpsaWJyYXJ5KGdnYW5pbWF0ZSkKCmFuaW0gPC0gZ2dwbG90KGQybTEsIGFlcyh4ID0gcF9ncmlkLCB5ID0gcG9zdGVyaW9yLCBncm91cCA9IG9icykpICsgCiAgZ2VvbV9wb2ludCgpICsKICBnZW9tX2xpbmUoKSArIAogIHRoZW1lKGxlZ2VuZC5wb3NpdGlvbiA9ICJub25lIikgKwogIHRyYW5zaXRpb25fc3RhdGVzKG9icywKICAgICAgICAgICAgICAgICAgICB0cmFuc2l0aW9uX2xlbmd0aCA9IDIsCiAgICAgICAgICAgICAgICAgICAgc3RhdGVfbGVuZ3RoID0gMSkgKwogIGxhYnMoeCA9ICJQcm9wb3J0aW9uIFdhdGVyIChwKSIsIHkgPSAiUG9zdGVyaW9yIFByb2JhYmlsaXR5IikgKwogIGdndGl0bGUoJ1Rvc3MgUmVzdWx0OiB7Y2xvc2VzdF9zdGF0ZX0nKSArIAogIGVudGVyX2ZhZGUoKSArCiAgZXhpdF9mYWRlKCkKCmFuaW1hdGUoYW5pbSwgaGVpZ2h0ID0gNTAwLCB3aWR0aCA9IDYwMCkKI2FuaW1fc2F2ZSgiLi4vLi4vc3RhdGljL2ltZy9leGFtcGxlL1dvcmxkLXRvc3NpbmctYmF5ZXNpYW4tY2hhcHRlcjIuZ2lmIikKYGBgCgohW10oLi4vLi4vaW1nL2V4YW1wbGUvV29ybGQtdG9zc2luZy1iYXllc2lhbi1jaGFwdGVyMi5naWYpCgojIyBQYWNrYWdlIHZlcnNpb25zCgpgYGB7cn0Kc2Vzc2lvbkluZm8oKQpgYGA=" download="02-class.Rmarkdown">
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
