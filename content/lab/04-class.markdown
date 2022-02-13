---
date: "2022-02-13"
title: "Class 4"
menu:
  example:
    parent: Labs
weight: 4
toc: true
type: docs
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:text/x-markdown;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyA0IgotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQpgYGAKCgojIyBJbnRyb2R1Y3Rpb24KCkZvciB0aGlzIGNsYXNzLCB3ZSdsbCByZXZpZXcgY29kZSBleGFtcGxlcyBmb3VuZCBpbiBDaGFwdGVyIDQgYW5kIHNvbWUgb2YgQ2hhcHRlciA1LgoKIyMgQ2hhcHRlciA0OiBUaGUgY2F1c2VzIGFyZW4ndCBpbiB0aGUgZGF0YSAKClRoaXMgY2hhcHRlciBleHBhbmRlZCByZWdyZXNzaW9uIHRvIG11bHRpcGxlIHJlZ3Jlc3Npb24uIFRoaXMgaXMgd2hlbiB3ZSBjb25zaWRlciBtb3JlIHRoYW4gMSBwcmVkaWN0b3IgKGluZGVwZW5kZW50KSB2YXJpYWJsZS4gV2UnbGwgY29uc2lkZXIgYSBuZXcgY2F0ZWdvcmljYWwgdmFyaWFibGUgb2Ygc2V4LiAKCiFbXSguLi8uLi9pbWcvZXhhbXBsZS8wNC1jbGFzcy5wbmcpCgojIyBUb3RhbCBjYXVzYWwgZWZmZWN0IG9mIFMgb24gVwoKYGBge3J9CnNldC5zZWVkKDEwMCkgIyBmeWksIGluIGNvZGUgc2VlZCB3YXNuJ3Qgc2V0IHNvIG1heSBiZSBzbGlnaHRseSBkaWZmZXJlbnQKbGlicmFyeShyZXRoaW5raW5nKQpkYXRhKCJIb3dlbGwxIikKZCA8LSBIb3dlbGwxW0hvd2VsbDEkYWdlPj0xOCxdCmBgYAoKTGV0J3MgYWdhaW4gY29uc2lkZXIgdGhlIDE4KyB5ZWFyIG9sZCBmcm9tIHRoZSBIb3dlbGwgZGF0YXNldCwgYnV0IG5vdyBsb29rIGF0IHRoZSByb2xlIG9mIGEgdGhpcmQgdmFyaWFibGU6IHNleC4KCmBgYHtyfQpwbG90KGQkaGVpZ2h0LCBkJHdlaWdodCwgY29sID0gaWZlbHNlKGQkbWFsZSw0LDIpLCAgeGxhYiA9ICJoZWlnaHQgKGNtKSIsIHlsYWIgPSAid2VpZ2h0IChrZykiLCAgbHdkPTMpCmxlZ2VuZCgxMzgsIDYzLCBsZWdlbmQ9YygiRmVtYWxlIiwgIk1hbGUiKSwKICAgICAgIGNvbD1jKDIsNCksIGx0eT0xOjEsIGNleD0wLjgpCmBgYAoKYGBge3J9CmhlYWQoZCkKYGBgCgoKV2UgYXJlIGludGVyZXN0ZWQgaW4gdGhlICoqdG90YWwqKiBjYXVzYWwgZWZmZWN0IG9mIFMgb24gVyAoaS5lLiwgdGhyb3VnaCBib3RoIHBhdGhzIGRpcmVjdCBhbmQgdmlhIEgpLgoKYGBge3IgZmlnLmhlaWdodD00LCBmaWcud2lkdGg9NH0KbGlicmFyeShkYWdpdHR5KQoKZyA8LSBkYWdpdHR5KCdkYWcgewpiYj0iMCwwLDEsMSIKUyBbcG9zPSIwLjI1MSwwLjQ4MSJdCkggW3Bvcz0iMC4yNTEsMC4zNTIiXQpXIFtwb3M9IjAuNDgxLDAuMzUyIl0KUyAtPiBIClMgLT4gVwpIIC0+IFcKfQonKQpwbG90KGcpCmBgYAoKTGV0J3MgZmlyc3QgbG9vayBhdCB0aGUgZGlzdHJpYnV0aW9ucyBvZiBoZWlnaHRzIGFuZCB3ZWlnaHRzIGJ5IHNleC4KCmBgYHtyfQojIG5ldyBoZWlnaHQsIHdlaWdodCwgc2V4IGNhdGVnb3JpY2FsIHZhcmlhYmxlIGV4YW1wbGUKZGVucyhkJGhlaWdodFtkJG1hbGU9PTFdLGx3ZD0zLGNvbD00LHhsYWI9ImhlaWdodCAoY20pIikKZGVucyhkJGhlaWdodFtkJG1hbGU9PTBdLGx3ZD0zLGNvbD0yLGFkZD1UUlVFKQpgYGAKCmBgYHtyfQpkZW5zKGQkd2VpZ2h0W2QkbWFsZT09MV0sbHdkPTMsY29sPTQseGxhYj0id2VpZ2h0IChrZykiKQpkZW5zKGQkd2VpZ2h0W2QkbWFsZT09MF0sbHdkPTMsY29sPTIsYWRkPVRSVUUpCmBgYAoKIyMjIENhdXNhbCBlZmZlY3Qgb2YgUyBvbiBXPwoKU2VlIG1vZGVsIGluIFtMZWN0dXJlIDRdKGh0dHBzOi8veW91dHUuYmUvUWlIS2R2QWJZSUk/dD0xNDMyKS4KCmBgYHtyfQojIFcgfiBTCmRhdCA8LSBsaXN0KAogICAgVyA9IGQkd2VpZ2h0LAogICAgUyA9IGQkbWFsZSArIDEgKSAjIFM9MSBmZW1hbGUsIFM9MiBtYWxlCgptX1NXIDwtIHF1YXAoCiAgICBhbGlzdCgKICAgICAgICBXIH4gZG5vcm0obXUsc2lnbWEpLAogICAgICAgIG11IDwtIGFbU10sCiAgICAgICAgYVtTXSB+IGRub3JtKDYwLDEwKSwgIyBhc3N1bWUgYm90aCBzZXhlcyBoYXZlIHNhbWUgcHJpb3JzCiAgICAgICAgc2lnbWEgfiBkdW5pZigwLDEwKQogICAgKSwgZGF0YT1kYXQgKQpgYGAKCkZpcnN0IHRoaW5nIHdlJ2xsIGRvIGlzIGNhbGN1bGF0ZSB0aGUgcG9zdGVyaW9yIG1lYW4gd2VpZ2h0cyAoW0xlY3R1cmUgNF0oaHR0cHM6Ly95b3V0dS5iZS9RaUhLZHZBYllJST90PTE1MzUpKS4gCgpgYGB7cn0KTl9zaW0gPSAxMDAwICMgbnVtYmVyIG9mIHNhbXBsZXMKCiMgcG9zdGVyaW9yIG1lYW5zCnBvc3QgPC0gZXh0cmFjdC5zYW1wbGVzKG1fU1csIG4gPSBOX3NpbSkKZGVucyggcG9zdCRhWywxXSAsIHhsaW09YygzOSw1MCkgLCBsd2Q9MyAsIGNvbD0yICwgeGxhYj0icG9zdGVyaW9yIG1lYW4gd2VpZ2h0IChrZykiICkKZGVucyggcG9zdCRhWywyXSAsIGx3ZD0zICwgY29sPTQgLCBhZGQ9VFJVRSApCmBgYAoKV2UgY2FuIHRoZW4gY2FsY3VsYXRlIHRoZSBwb3N0ZXJpb3IgbWVhbiB3ZWlnaHQgY29udHJhc3QuCgpgYGB7cn0KIyBjYXVzYWwgY29udHJhc3QgKGluIG1lYW5zKQptdV9jb250cmFzdCA8LSBwb3N0JGFbLDJdIC0gcG9zdCRhWywxXQpkZW5zKCBtdV9jb250cmFzdCAsIHhsaW09YygzLDEwKSAsIGx3ZD0zICwgY29sPTEgLCB4bGFiPSJwb3N0ZXJpb3IgbWVhbiB3ZWlnaHQgY29udHJhc3QgKGtnKSIgKQpgYGAKCgpPbiBhdmVyYWdlLCBtZW4gYXJlIGhlYXZpbHkgKipvbiBhdmVyYWdlKiogYnV0IG5vdCBuZWNlc3NhcmlseSAqKnJlbGlhYmx5IGhlYXZpZXIqKi4KCk5leHQsIHdlJ2xsIGdlbmVyYXRlIHByZWRpY3RpdmUgd2VpZ2h0cy4gVGhpcyBhbGxvd3MgdXMgdG8gY2FsY3VsYXRlIHRoZSBjb250cmFzdCBvbiB0aGUgaW5kaXZpZHVhbCBsZXZlbC4KCmBgYHtyfQpOX3NpbSA9IDEwMDAgIyBsZXQncyBkbyAxLDAwMCBzYW1wbGVzCgojIHBvc3RlcmlvciBXIGRpc3RyaWJ1dGlvbnMKVzEgPC0gcm5vcm0oIE5fc2ltICwgcG9zdCRhWywxXSAsIHBvc3Qkc2lnbWEgKQpXMiA8LSBybm9ybSggTl9zaW0gLCBwb3N0JGFbLDJdICwgcG9zdCRzaWdtYSApCmRlbnMoIFcxICwgeGxpbT1jKDIwLDcwKSAsIHlsaW09YygwLDAuMDg1KSAsIGx3ZD0zICwgY29sPTIgLCB4bGFiPSJwb3N0ZXJpb3IgcHJlZGljdGVkIHdlaWdodCAoa2cpIiApCmRlbnMoIFcyICwgbHdkPTMgLCBjb2w9NCAsIGFkZD1UUlVFICkKYGBgCgoKCgpgYGB7cn0KIyBXIGNvbnRyYXN0CldfY29udHJhc3QgPC0gVzIgLSBXMQpkZW5zKCBXX2NvbnRyYXN0ICwgeGxpbT1jKC0yNSwzNSkgLCBsd2Q9MyAsIGNvbD0xICwgeGxhYj0icG9zdGVyaW9yIHdlaWdodCBjb250cmFzdCAoa2cpIiApCgpXZGVucyA8LSBkZW5zaXR5KFdfY29udHJhc3QsYWRqPTAuNSkKcG9seWdvbihjKFdkZW5zJHhbV2RlbnMkeD4wXSwgbWF4KFdkZW5zJHgpLCAwKSwgYyhXZGVucyR5W1dkZW5zJHg+MF0sIDAsIDApLCBjb2wgPSA0LCBib3JkZXIgPSBOQSApCnBvbHlnb24oYyhXZGVucyR4W1dkZW5zJHg8MF0sIDAsIG1pbihXZGVucyR4KSksIGMoV2RlbnMkeVtXZGVucyR4PDBdLCAwLCAwKSwgY29sID0gMiwgYm9yZGVyID0gTkEgKQpgYGAKClRoaXMgYWxsb3dzIHVzIHRvIGNhbGN1bGF0ZSBob3cgbWFueSBjYXNlcyB3aGVyZSBtZW4gYXJlIHRhbGxlciAoYWJvdmUgemVybykgYW5kIGFyZSBzaG9ydGVyIChiZWxvdyB6ZXJvKS4KCmBgYHtyfQojIHByb3BvcnRpb24gYWJvdmUgemVybwpzdW0oIFdfY29udHJhc3QgPiAwICkgLyBOX3NpbQojIHByb3BvcnRpb24gYmVsb3cgemVybwpzdW0oIFdfY29udHJhc3QgPCAwICkgLyBOX3NpbQpgYGAKCkFib3V0IDgyJSB0aGUgbWFsZXMgYXJlIHRhbGxlciB0aGFuIHRoZSB3b21lbiAtLSBidXQgYWJvdXQgMjAlIG9mIGNhc2VzIHdvbWVuIGFyZSB0YWxsZXIuCgp7eyUgY2FsbG91dCBub3RlICV9fQoKV2h5IGlzIHRoZSBwb3N0ZXJpb3IgY29udHJhc3RzIG5vdCBleGFjdGx5IGVxdWFsIHRvIHRoZSBsZWN0dXJlPwoKV2hhdCB3b3VsZCBoYXBwZW4gaWYgeW91IGluY3JlYXNlIG51bWJlciBvZiBzYW1wbGVzIHRvIDEwMCwwMDA/IHdoYXQgYWJvdXQgMSwwMDAsMDAwIChvbmx5IHJ1biBpZiB5b3UgaGF2ZSBzdWZmaWNpZW50IFJBTSk/Cgp7eyUgL2NhbGxvdXQgICV9fQoKIyMjIEFsdGVybmF0aXZlIEFwcHJvYWNoCgpBbiBhbHRlcm5hdGl2ZSBhcHByb2FjaCBmb3IgY2FsY3VsYXRpbmcgdGhlIGNvbnRyYXN0IHVzaW5nIHRoZSBgbGluaygpYCBhbmQgYHNpbSgpYCBmdW5jdGlvbnMuCgoqIHdlIGNhbiB1c2UgYGV4dHJhY3Quc2FtcGxlc2Agc2ltaWxhcmx5IGFzIGBsaW5rKClgCgoqIGNhbiBhbHNvIHVzZSBleHRyYWN0ZWQgc2FtcGxlcyB0byBjYWxjdWxhdGUgc2ltaWxhciBmdW5jdGlvbiB0byBgc2ltKClgCgpGaXJzdCwgc2luY2Ugd2UncmUgY29uc2lkZXJpbmcgdGhlIG1lYW4gKG11KSB2YWx1ZSwgd2UgY2FuIHVzZSB0aGUgYGxpbmsoKWAgZnVuY3Rpb24uCgpgYGB7cn0KIyBhcHByb2FjaCAyOiB1c2UgbGluayBmdW5jdGlvbgpOX3NpbSA9IDEwMDAKCm11RiA8LSBsaW5rKG1fU1csZGF0YT1saXN0KFM9MSksIE5fc2ltKQptdU0gPC0gbGluayhtX1NXLGRhdGE9bGlzdChTPTIpLCBOX3NpbSkKCmRlbnMoIG11RiAsIHhsaW09Yyg0MCw1MCkgLCBsd2Q9MyAsIGNvbD0yICwgeGxhYj0icG9zdGVyaW9yIG1lYW4gd2VpZ2h0IChrZykiICkKZGVucyggbXVNICwgbHdkPTMgLCBjb2w9NCAsIGFkZD1UUlVFICkKYGBgCgpgYGB7cn0KIyBjYXVzYWwgY29udHJhc3QgKGluIG1lYW5zKQpsaW5rX2NvbnRyYXN0IDwtIG11TSAtIG11RgpkZW5zKCBtdV9jb250cmFzdCAsIHhsaW09YygzLDEwKSAsIGx3ZD0zICwgY29sPTEgLCB4bGFiPSJwb3N0ZXJpb3IgbWVhbiB3ZWlnaHQgY29udHJhc3QgKGtnKSIpICMgcmVkIHVzaW5nIGV4dHJhY3Quc2FtcGxlcwpsaW5lcyggZGVuc2l0eShsaW5rX2NvbnRyYXN0KSAsIGx3ZD0zICwgY29sPTIgKSAjIGJsYWNrIGlzIGxpbmsoKQpgYGAKCgpOb3RpY2UgdGhpcyBpcyBuZWFybHkgaWRlbnRpY2FsIHRvIHRoZSBlYXJsaWVyIGNoYXJ0cyB3aXRoIHVzaW5nIGBleHRyYWN0LnNhbXBsZXNgLiBXZSBjYW4gZm9sbG93IGEgc2ltaWxhciBsb2dpYyBidXQgbG9vayBhdCB0aGUgcG9zdGVyaW9yIHByZWRpY3RpdmVzLgoKYGBge3J9Cm11RiA8LSBzaW0obV9TVyxkYXRhPWxpc3QoUz0xKSwgTl9zaW0pCm11TSA8LSBzaW0obV9TVyxkYXRhPWxpc3QoUz0yKSwgTl9zaW0pCgpkZW5zKCBtdUYgLCB4bGltPWMoMjAsNzApICwgbHdkPTMgLCBjb2w9MiAsIHhsYWI9InBvc3RlcmlvciBtZWFuIHdlaWdodCAoa2cpIiApCmRlbnMoIG11TSAsIGx3ZD0zICwgY29sPTQgLCBhZGQ9VFJVRSApCmBgYAoKYGBge3J9CmNvbnRyYXN0IDwtIG11TSAtIG11RgoKZGVucyggY29udHJhc3QgLCB4bGltPWMoLTI1LDM1KSAsIGx3ZD0zICwgY29sPTEgLCB4bGFiPSJwb3N0ZXJpb3Igd2VpZ2h0IGNvbnRyYXN0IChrZykiICkKCldkZW5zIDwtIGRlbnNpdHkoY29udHJhc3QsYWRqPTAuNSkKcG9seWdvbihjKFdkZW5zJHhbV2RlbnMkeD4wXSwgbWF4KFdkZW5zJHgpLCAwKSwgYyhXZGVucyR5W1dkZW5zJHg+MF0sIDAsIDApLCBjb2wgPSA0LCBib3JkZXIgPSBOQSApCnBvbHlnb24oYyhXZGVucyR4W1dkZW5zJHg8MF0sIDAsIG1pbihXZGVucyR4KSksIGMoV2RlbnMkeVtXZGVucyR4PDBdLCAwLCAwKSwgY29sID0gMiwgYm9yZGVyID0gTkEgKQpgYGAKCmBgYHtyfQojIHByb3BvcnRpb24gYWJvdmUgemVybwpzdW0oIGNvbnRyYXN0ID4gMCApIC8gTl9zaW0KIyBwcm9wb3J0aW9uIGJlbG93IHplcm8Kc3VtKCBjb250cmFzdCA8IDAgKSAvIE5fc2ltCmBgYAoKe3slIGNhbGxvdXQgbm90ZSAlfX0KCldoYXQgd291bGQgaGFwcGVuIGlmIHlvdSBpbmNyZWFzZSBudW1iZXIgb2Ygc2FtcGxlcyBmb3IgYGxpbmsoKWAgYW5kIGBzaW0oKWA/CgpIb3cgZG9lcyB0aGUgY29tcHV0YXRpb25hbCBzcGVlZCBjb21wYXJlIGZvciB1c2luZyBgZXh0cmFjdC5zYW1wbGVzKClgIHZzLiBgbGluaygpYCAvIGBzaW0oKWA/Cgp7eyUgL2NhbGxvdXQgICV9fQoKIyMjIERpcmVjdCBjYXVzYWwgZWZmZWN0IG9mIFMgb24gVz8KCldlIGNhbiBhbHNvIHRyeSB0byBtZWFzdXJlIHRoZSBzb2xlIGVmZmVjdCBvZiBTIG9uIFcuIEZyb20gW2Fib3V0IG1pbnV0ZSAzNyBpbiBMZWN0dXJlIDRdKGh0dHBzOi8veW91dHUuYmUvUWlIS2R2QWJZSUk/dD0yMjA4KS4KCkluIHRoaXMgd2UgKipzdHJhdGlmeSoqIGJ5IHNleCB3aGVyZSB3ZSB3aWxsIGdldCBhbiBpbnRlcmNlcHQgYW5kIGEgc2xvcGUgdW5pcXVlIGZvciBlYWNoIG9mIHRoZSBzZXhlcy4KCmBgYHtyfQojIFcgfiBTICsgSApkYXQgPC0gbGlzdCgKICAgIFcgPSBkJHdlaWdodCwKICAgIEggPSBkJGhlaWdodCwKICAgIEhiYXIgPSBtZWFuKGQkaGVpZ2h0KSwKICAgIFMgPSBkJG1hbGUgKyAxICkgIyBTPTEgZmVtYWxlLCBTPTIgbWFsZQoKbV9TSFcgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkbm9ybShtdSxzaWdtYSksCiAgICAgICAgbXUgPC0gYVtTXSArIGJbU10qKEgtSGJhciksCiAgICAgICAgYVtTXSB+IGRub3JtKDYwLDEwKSwKICAgICAgICBiW1NdIH4gZGxub3JtKDAsMSksCiAgICAgICAgc2lnbWEgfiBkdW5pZigwLDEwKQogICAgKSwgZGF0YT1kYXQgKQpgYGAgICAgCgpMZXQncyBub3cgZ2V0IHRoZSBwb3N0ZXJpb3IgcHJlZGljdGl2ZXMgZm9yIHRoZSBjb250cmFzdHMuCgpgYGB7cn0KeHNlcSA8LSBzZXEoZnJvbT0xMzAsdG89MTkwLGxlbj01MCkKCm11RiA8LSBsaW5rKG1fU0hXLGRhdGE9bGlzdChTPXJlcCgxLDUwKSxIPXhzZXEsSGJhcj1tZWFuKGQkaGVpZ2h0KSkpCm11TSA8LSBsaW5rKG1fU0hXLGRhdGE9bGlzdChTPXJlcCgyLDUwKSxIPXhzZXEsSGJhcj1tZWFuKGQkaGVpZ2h0KSkpCm11X2NvbnRyYXN0IDwtIG11RiAtIG11TQpwbG90KCBOVUxMLCB4bGltPXJhbmdlKHhzZXEpICwgeWxpbT1jKC02LDgpICwgeGxhYiA9ICJoZWlnaHQgKGNtKSIsIHlsYWIgPSAid2VpZ2h0IGNvbnRyYXN0IChGLU0pIikKZm9yICggcCBpbiBjKDAuNSwwLjYsMC43LDAuOCwwLjksMC45OSkpCiAgc2hhZGUoIGFwcGx5KG11X2NvbnRyYXN0LDIsUEkscHJvYj1wKSwgeHNlcSkKYWJsaW5lKGg9MCxsdHk9MikKYGBgCgp7eyUgY2FsbG91dCBub3RlICV9fQoKVGhvdWdodCBxdWVzdGlvbjogV2h5IHdhcyBpbmNsdWRpbmcgaGVpZ2h0IGludG8gdGhlIHJlZ3Jlc3Npb24gZW5hYmxpbmcgZXN0aW1hdGlvbiBvZiB0aGUgKipkaXJlY3QqKiBlZmZlY3Qgb2Ygc2V4IG9uIHdlaWdodD8gV2h5IHdhcyByZW1vdmluZyBoZWlnaHQgZW5hYmxpbmcgYSAqKnRvdGFsKiogZWZmZWN0IG9mIHNleCBvbiB3ZWlnaHQ/Cgp7eyUgL2NhbGxvdXQgJX19CgoKIyMjIEZ1bGwgTHV4dXJ5IEJheWVzIChPcHRpb25hbCkKClRoaXMgcGFydCBpcyBjb3ZlcmVkIGluIExlY3R1cmUgNCBicmllZmx5IC0tIG5vdGUsIHdlIHdvbid0IGdvIGluIGRlcHRoIHNvIG9ubHkgcmV2aWV3IHRoaXMgZXhhbXBsZSBpZiB5b3Ugd2FudCB0byB1bmRlcnN0YW5kIG1vcmUuCgpJbiB0aGlzIHNldHVwLCB3ZSdsbCBydW4gaGVpZ2h0IGFuZCB3ZWlnaHQgc2ltdWx0YW5lb3VzbHkuIAoKYGBge3J9CiMgZnVsbCBzeXN0ZW0gYXMgU0NNCmRhdCA8LSBsaXN0KAogICAgVyA9IGQkd2VpZ2h0LAogICAgSCA9IGQkaGVpZ2h0LAogICAgSGJhciA9IG1lYW4oZCRoZWlnaHQpLAogICAgUyA9IGQkbWFsZSArIDEgKSAjIFM9MSBmZW1hbGUsIFM9MiBtYWxlCgptX1NIV19mdWxsIDwtIHF1YXAoCiAgICBhbGlzdCgKCiAgICAgICAgIyB3ZWlnaHQKICAgICAgICBXIH4gZG5vcm0obXUsc2lnbWEpLAogICAgICAgIG11IDwtIGFbU10gKyBiW1NdKihILUhiYXIpLAogICAgICAgIGFbU10gfiBkbm9ybSg2MCwxMCksCiAgICAgICAgYltTXSB+IGRsbm9ybSgwLDEpLAogICAgICAgIHNpZ21hIH4gZHVuaWYoMCwxMCksCgogICAgICAgICMgaGVpZ2h0CiAgICAgICAgSCB+IGRub3JtKG51LHRhdSksCiAgICAgICAgbnUgPC0gaFtTXSwKICAgICAgICBoW1NdIH4gZG5vcm0oMTYwLDEwKSwKICAgICAgICB0YXUgfiBkdW5pZigwLDEwKQoKICAgICksIGRhdGE9ZGF0ICkKYGBgCgpXZSdsbCBzaW11bGF0ZSAxMDAwIHN5bnRoZXRpYyB3b21lbiBpbiBvcmRlci4gV2UgZm9jdXMgb24gaGVpZ2h0IGZpcnN0IHNpbmNlIGl0J3MgYSBmdW5jdGlvbiBvZiB3ZWlnaHQuIFRoZW4gc2ltdWxhdGUgd2VpZ2h0cyBieSB1c2luZyB0aGUgc2ltdWxhdGlvbiBoZWlnaHRzLiBUaGVuIHJlcGVhdCBmb3IgMTAwMCBzeW50aGV0aWMgbWVuIGluIHRoZSBzaW1pbGFyIG9yZGVyLgoKYGBge3J9CiMgY29tcHV0ZSB0b3RhbCBjYXVzYWwgZWZmZWN0IG9mIFMgb24gVwpwb3N0IDwtIGV4dHJhY3Quc2FtcGxlcyhtX1NIV19mdWxsKQpIYmFyIDwtIGRhdCRIYmFyCm4gPC0gMWU0Cgp3aXRoKCBwb3N0ICwgewojIHNpbXVsYXRlIFcgZm9yIFM9MQogIEhfUzEgPC0gcm5vcm0obiwgaFssMV0gLCB0YXUgKQogIFdfUzEgPC0gcm5vcm0obiwgYVssMV0gKyBiWywxXSooSF9TMS1IYmFyKSAsIHNpZ21hKQojIHNpbXVsYXRlIFcgZm9yIFM9MgogIEhfUzIgPC0gcm5vcm0obiwgaFssMl0gLCB0YXUpCiAgV19TMiA8LSBybm9ybShuLCBhWywyXSArIGJbLDJdKihIX1MyLUhiYXIpICwgc2lnbWEpCiMgY29tcHV0ZSBjb250cmFzdCAoZG8gb3BlcmF0b3IpOyBzaG91bGQgaG9sZCByZXN1bHRzIGZyb20gaW50ZXJ2ZW5pbmcgaW4gc2V4CiAgV19kb19TIDw8LSBXX1MyIC0gV19TMQojIDw8LSAoc2NvcGluZyBhc3NpZ25tZW50KQojaHR0cHM6Ly9zdGFja292ZXJmbG93LmNvbS9xdWVzdGlvbnMvMjYyODYyMS9ob3ctZG8teW91LXVzZS1zY29waW5nLWFzc2lnbm1lbnQtaW4tcgp9KQpgYGAKCmBgYHtyfQpkZW5zKCBXX2RvX1MgLCB4bGltPWMoLTI1LDM1KSAsIGx3ZD0zICwgY29sPTEgLCB4bGFiPSJwb3N0ZXJpb3Igd2VpZ2h0IGNvbnRyYXN0IChrZykiICkKCldkZW5zIDwtIGRlbnNpdHkoV19kb19TLGFkaj0wLjUpCnBvbHlnb24oYyhXZGVucyR4W1dkZW5zJHg+MF0sIG1heChXZGVucyR4KSwgMCksIGMoV2RlbnMkeVtXZGVucyR4PjBdLCAwLCAwKSwgY29sID0gNCwgYm9yZGVyID0gTkEgKQpwb2x5Z29uKGMoV2RlbnMkeFtXZGVucyR4PDBdLCAwLCBtaW4oV2RlbnMkeCkpLCBjKFdkZW5zJHlbV2RlbnMkeDwwXSwgMCwgMCksIGNvbCA9IDIsIGJvcmRlciA9IE5BICkKYGBgCgoKYGBge3J9CiMgYXV0b21hdGVkIHdheQpIV3NpbSA8LSBzaW0obV9TSFdfZnVsbCwKICAgICAgICAgICAgIGRhdGE9bGlzdChTPWMoMSwyKSksCiAgICAgICAgICAgICB2YXJzPWMoIkgiLCJXIikpCldfZG9fU19hdXRvIDwtIEhXc2ltJFdbLDJdIC0gSFdzaW0kV1ssMV0KYGBgCgoKIyMgUGFja2FnZSB2ZXJzaW9ucwoKYGBge3J9CnNlc3Npb25JbmZvKCkKYGBg" download="04-class.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010_examples" target="_blank"><img src="https://gitpod.io/button/open-in-gitpod.svg" style="display: block; margin: auto auto auto 0;" /></a>

## Introduction

For this class, we’ll review code examples found in Chapter 4 and some of Chapter 5.

## Chapter 4: The causes aren’t in the data

This chapter expanded regression to multiple regression. This is when we consider more than 1 predictor (independent) variable. We’ll consider a new categorical variable of sex.

![](../../img/example/04-class.png)

## Total causal effect of S on W

``` r
set.seed(100) # fyi, in code seed wasn't set so may be slightly different
library(rethinking)
data("Howell1")
d <- Howell1[Howell1$age>=18,]
```

Let’s again consider the 18+ year old from the Howell dataset, but now look at the role of a third variable: sex.

``` r
plot(d$height, d$weight, col = ifelse(d$male,4,2),  xlab = "height (cm)", ylab = "weight (kg)",  lwd=3)
legend(138, 63, legend=c("Female", "Male"),
       col=c(2,4), lty=1:1, cex=0.8)
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-4-1.png" width="672" />

``` r
head(d)
```

``` language-r
##    height   weight age male
## 1 151.765 47.82561  63    1
## 2 139.700 36.48581  63    0
## 3 136.525 31.86484  65    0
## 4 156.845 53.04191  41    1
## 5 145.415 41.27687  51    0
## 6 163.830 62.99259  35    1
```

We are interested in the **total** causal effect of S on W (i.e., through both paths direct and via H).

``` r
library(dagitty)

g <- dagitty('dag {
bb="0,0,1,1"
S [pos="0.251,0.481"]
H [pos="0.251,0.352"]
W [pos="0.481,0.352"]
S -> H
S -> W
H -> W
}
')
plot(g)
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-6-1.png" width="384" />

Let’s first look at the distributions of heights and weights by sex.

``` r
# new height, weight, sex categorical variable example
dens(d$height[d$male==1],lwd=3,col=4,xlab="height (cm)")
dens(d$height[d$male==0],lwd=3,col=2,add=TRUE)
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-7-1.png" width="672" />

``` r
dens(d$weight[d$male==1],lwd=3,col=4,xlab="weight (kg)")
dens(d$weight[d$male==0],lwd=3,col=2,add=TRUE)
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-8-1.png" width="672" />

### Causal effect of S on W?

See model in [Lecture 4](https://youtu.be/QiHKdvAbYII?t=1432).

``` r
# W ~ S
dat <- list(
    W = d$weight,
    S = d$male + 1 ) # S=1 female, S=2 male

m_SW <- quap(
    alist(
        W ~ dnorm(mu,sigma),
        mu <- a[S],
        a[S] ~ dnorm(60,10), # assume both sexes have same priors
        sigma ~ dunif(0,10)
    ), data=dat )
```

First thing we’ll do is calculate the posterior mean weights ([Lecture 4](https://youtu.be/QiHKdvAbYII?t=1535)).

``` r
N_sim = 1000 # number of samples

# posterior means
post <- extract.samples(m_SW, n = N_sim)
dens( post$a[,1] , xlim=c(39,50) , lwd=3 , col=2 , xlab="posterior mean weight (kg)" )
dens( post$a[,2] , lwd=3 , col=4 , add=TRUE )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-10-1.png" width="672" />

We can then calculate the posterior mean weight contrast.

``` r
# causal contrast (in means)
mu_contrast <- post$a[,2] - post$a[,1]
dens( mu_contrast , xlim=c(3,10) , lwd=3 , col=1 , xlab="posterior mean weight contrast (kg)" )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-11-1.png" width="672" />

On average, men are heavily **on average** but not necessarily **reliably heavier**.

Next, we’ll generate predictive weights. This allows us to calculate the contrast on the individual level.

``` r
N_sim = 1000 # let's do 1,000 samples

# posterior W distributions
W1 <- rnorm( N_sim , post$a[,1] , post$sigma )
W2 <- rnorm( N_sim , post$a[,2] , post$sigma )
dens( W1 , xlim=c(20,70) , ylim=c(0,0.085) , lwd=3 , col=2 , xlab="posterior predicted weight (kg)" )
dens( W2 , lwd=3 , col=4 , add=TRUE )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-12-1.png" width="672" />

``` r
# W contrast
W_contrast <- W2 - W1
dens( W_contrast , xlim=c(-25,35) , lwd=3 , col=1 , xlab="posterior weight contrast (kg)" )

Wdens <- density(W_contrast,adj=0.5)
polygon(c(Wdens$x[Wdens$x>0], max(Wdens$x), 0), c(Wdens$y[Wdens$x>0], 0, 0), col = 4, border = NA )
polygon(c(Wdens$x[Wdens$x<0], 0, min(Wdens$x)), c(Wdens$y[Wdens$x<0], 0, 0), col = 2, border = NA )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-13-1.png" width="672" />

This allows us to calculate how many cases where men are heavier (above zero) and are lighter (below zero).

``` r
# proportion above zero
sum( W_contrast > 0 ) / N_sim
```

``` language-r
## [1] 0.808
```

``` r
# proportion below zero
sum( W_contrast < 0 ) / N_sim
```

``` language-r
## [1] 0.192
```

About 82% the males are heavier than the women – but about 20% of cases women are lighter

{{% callout note %}}

Why is the posterior contrasts not exactly equal to the lecture?

What would happen if you increase number of samples to 100,000? what about 1,000,000 (only run if you have sufficient RAM)?

{{% /callout %}}

### Alternative Approach

An alternative approach for calculating the contrast using the `link()` and `sim()` functions.

-   we can use `extract.samples` similarly as `link()`

-   can also use extracted samples to calculate similar function to `sim()`

First, since we’re considering the mean (mu) value, we can use the `link()` function.

``` r
# approach 2: use link function
N_sim = 1000

muF <- link(m_SW,data=list(S=1), N_sim)
muM <- link(m_SW,data=list(S=2), N_sim)

dens( muF , xlim=c(40,50) , lwd=3 , col=2 , xlab="posterior mean weight (kg)" )
dens( muM , lwd=3 , col=4 , add=TRUE )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-15-1.png" width="672" />

``` r
# causal contrast (in means)
link_contrast <- muM - muF
dens( mu_contrast , xlim=c(3,10) , lwd=3 , col=1 , xlab="posterior mean weight contrast (kg)") # red using extract.samples
lines( density(link_contrast) , lwd=3 , col=2 ) # black is link()
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-16-1.png" width="672" />

Notice this is nearly identical to the earlier charts with using `extract.samples`. We can follow a similar logic but look at the posterior predictives.

``` r
muF <- sim(m_SW,data=list(S=1), N_sim)
muM <- sim(m_SW,data=list(S=2), N_sim)

dens( muF , xlim=c(20,70) , lwd=3 , col=2 , xlab="posterior mean weight (kg)" )
dens( muM , lwd=3 , col=4 , add=TRUE )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-17-1.png" width="672" />

``` r
contrast <- muM - muF

dens( contrast , xlim=c(-25,35) , lwd=3 , col=1 , xlab="posterior weight contrast (kg)" )

Wdens <- density(contrast,adj=0.5)
polygon(c(Wdens$x[Wdens$x>0], max(Wdens$x), 0), c(Wdens$y[Wdens$x>0], 0, 0), col = 4, border = NA )
polygon(c(Wdens$x[Wdens$x<0], 0, min(Wdens$x)), c(Wdens$y[Wdens$x<0], 0, 0), col = 2, border = NA )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-18-1.png" width="672" />

``` r
# proportion above zero
sum( contrast > 0 ) / N_sim
```

``` language-r
## [1] 0.839
```

``` r
# proportion below zero
sum( contrast < 0 ) / N_sim
```

``` language-r
## [1] 0.161
```

{{% callout note %}}

What would happen if you increase number of samples for `link()` and `sim()`?

How does the computational speed compare for using `extract.samples()` vs. `link()` / `sim()`?

{{% /callout %}}

### Direct causal effect of S on W?

We can also try to measure the sole effect of S on W. From [about minute 37 in Lecture 4](https://youtu.be/QiHKdvAbYII?t=2208).

In this we **stratify** by sex where we will get an intercept and a slope unique for each of the sexes.

``` r
# W ~ S + H
dat <- list(
    W = d$weight,
    H = d$height,
    Hbar = mean(d$height),
    S = d$male + 1 ) # S=1 female, S=2 male

m_SHW <- quap(
    alist(
        W ~ dnorm(mu,sigma),
        mu <- a[S] + b[S]*(H-Hbar),
        a[S] ~ dnorm(60,10),
        b[S] ~ dlnorm(0,1),
        sigma ~ dunif(0,10)
    ), data=dat )
```

Let’s now get the posterior predictives for the contrasts.

``` r
xseq <- seq(from=130,to=190,len=50)

muF <- link(m_SHW,data=list(S=rep(1,50),H=xseq,Hbar=mean(d$height)))
muM <- link(m_SHW,data=list(S=rep(2,50),H=xseq,Hbar=mean(d$height)))
mu_contrast <- muF - muM
plot( NULL, xlim=range(xseq) , ylim=c(-6,8) , xlab = "height (cm)", ylab = "weight contrast (F-M)")
for ( p in c(0.5,0.6,0.7,0.8,0.9,0.99))
  shade( apply(mu_contrast,2,PI,prob=p), xseq)
abline(h=0,lty=2)
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-21-1.png" width="672" />

{{% callout note %}}

Thought question: Why was including height into the regression enabling estimation of the **direct** effect of sex on weight? Why was removing height enabling a **total** effect of sex on weight?

{{% /callout %}}

### Full Luxury Bayes (Optional)

This part is covered in Lecture 4 briefly – note, we won’t go in depth so only review this example if you want to understand more.

In this setup, we’ll run height and weight simultaneously.

``` r
# full system as SCM
dat <- list(
    W = d$weight,
    H = d$height,
    Hbar = mean(d$height),
    S = d$male + 1 ) # S=1 female, S=2 male

m_SHW_full <- quap(
    alist(

        # weight
        W ~ dnorm(mu,sigma),
        mu <- a[S] + b[S]*(H-Hbar),
        a[S] ~ dnorm(60,10),
        b[S] ~ dlnorm(0,1),
        sigma ~ dunif(0,10),

        # height
        H ~ dnorm(nu,tau),
        nu <- h[S],
        h[S] ~ dnorm(160,10),
        tau ~ dunif(0,10)

    ), data=dat )
```

We’ll simulate 1000 synthetic women in order. We focus on height first since it’s a function of weight. Then simulate weights by using the simulation heights. Then repeat for 1000 synthetic men in the similar order.

``` r
# compute total causal effect of S on W
post <- extract.samples(m_SHW_full)
Hbar <- dat$Hbar
n <- 1e4

with( post , {
# simulate W for S=1
  H_S1 <- rnorm(n, h[,1] , tau )
  W_S1 <- rnorm(n, a[,1] + b[,1]*(H_S1-Hbar) , sigma)
# simulate W for S=2
  H_S2 <- rnorm(n, h[,2] , tau)
  W_S2 <- rnorm(n, a[,2] + b[,2]*(H_S2-Hbar) , sigma)
# compute contrast (do operator); should hold results from intervening in sex
  W_do_S <<- W_S2 - W_S1
# <<- (scoping assignment)
#https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r
})
```

``` r
dens( W_do_S , xlim=c(-25,35) , lwd=3 , col=1 , xlab="posterior weight contrast (kg)" )

Wdens <- density(W_do_S,adj=0.5)
polygon(c(Wdens$x[Wdens$x>0], max(Wdens$x), 0), c(Wdens$y[Wdens$x>0], 0, 0), col = 4, border = NA )
polygon(c(Wdens$x[Wdens$x<0], 0, min(Wdens$x)), c(Wdens$y[Wdens$x<0], 0, 0), col = 2, border = NA )
```

<img src="/lab/04-class_files/figure-html/unnamed-chunk-24-1.png" width="672" />

``` r
# automated way
HWsim <- sim(m_SHW_full,
             data=list(S=c(1,2)),
             vars=c("H","W"))
W_do_S_auto <- HWsim$W[,2] - HWsim$W[,1]
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
## [76] bsplus_0.1.3         shape_1.4.6          stringi_1.7.6       
## [79] Rcpp_1.0.7           vctrs_0.3.8          tidyselect_1.1.1    
## [82] xfun_0.28            coda_0.19-4
```
