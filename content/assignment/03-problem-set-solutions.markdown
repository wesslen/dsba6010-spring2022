---
title: Problem Set 3 Solutions
date: "2022-02-20"
menu:
  assignment:
    parent: Problem sets
    weight: 3
type: docs
toc: true
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>

This problem set is due on February 21, 2022 at 11:59am.

<a href="data:text/x-markdown;base64,LS0tCnRpdGxlOiBQcm9ibGVtIFNldCAzCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKLS0tCgpUaGlzIHByb2JsZW0gc2V0IGlzIGR1ZSBvbiBGZWJydWFyeSAyMSwgMjAyMiBhdCAxMTo1OWFtLgoKIyMgUXVlc3Rpb24gMQoKRnJvbSB0aGUgSG93ZWxsMSBkYXRhc2V0LCBjb25zaWRlciBvbmx5IHRoZSBwZW9wbGUgeW91bmdlciB0aGFuIDEzIHllYXJzIG9sZC4gRXN0aW1hdGUgdGhlIGNhdXNhbCBhc3NvY2lhdGlvbiBiZXR3ZWVuIGFnZSBhbmQgd2VpZ2h0LiBBc3N1bWUgdGhhdCBhZ2UgaW5mbHVlbmNlcyB3ZWlnaHQgdGhyb3VnaCB0d28gcGF0aHMuIEZpcnN0LCBhZ2UgaW5mbHVlbmNlcyBoZWlnaHQsIGFuZCBoZWlnaHQgaW5mbHVlbmNlcyB3ZWlnaHQuIFNlY29uZCwgYWdlIGRpcmVjdGx5IGluZmx1ZW5jZXMgd2VpZ2h0IHRocm91Z2ggYWdlIHJlbGF0ZWQgY2hhbmdlcyBpbiBtdXNjbGUgZ3Jvd3RoIGFuZCBib2R5IHByb3BvcnRpb25zLiBBbGwgb2YgdGhpcyBpbXBsaWVzIHRoaXMgY2F1c2FsIG1vZGVsIChEQUcpOgoKYGBge3IgZmlnLmhlaWdodD0yLCBmaWcud2lkdGg9Mn0KbGlicmFyeShkYWdpdHR5KQoKZyA8LSBkYWdpdHR5KCdkYWcgewpiYj0iMCwwLDEsMSIKQSBbcG9zPSIwLjI1MSwwLjQ4MSJdCkggW3Bvcz0iMC4zNTAsMC4zMTIiXQpXIFtwb3M9IjAuNDUyLDAuNDg0Il0KQSAtPiBICkEgLT4gVwpIIC0+IFcKfQonKQpwbG90KGcpCmBgYAoKVXNlIGEgbGluZWFyIHJlZ3Jlc3Npb24gdG8gZXN0aW1hdGUgdGhlIHRvdGFsIChub3QganVzdCBkaXJlY3QpIGNhdXNhbCBlZmZlY3Qgb2YKZWFjaCB5ZWFyIG9mIGdyb3d0aCBvbiB3ZWlnaHQuIEJlIHN1cmUgdG8gY2FyZWZ1bGx5IGNvbnNpZGVyIHRoZSBwcmlvcnMuIFRyeQp1c2luZyBwcmlvciBwcmVkaWN0aXZlIHNpbXVsYXRpb24gdG8gYXNzZXNzIHdoYXQgdGhleSBpbXBseS4KCmBgYHtyIG1lc3NhZ2U9RkFMU0Usd2FybmluZz1GQUxTRX0KbGlicmFyeShyZXRoaW5raW5nKQpkYXRhKEhvd2VsbDEpCmQgPC0gSG93ZWxsMQpkIDwtIGRbIGQkYWdlIDwgMTMgLCBdCgojIHNpbSBmcm9tIHByaW9ycwpuIDwtIDEwCmEgPC0gcm5vcm0obiw1LDEpCmIgPC0gcmxub3JtKG4sMCwxKQojIGJsYW5rKGJ0eT0ibiIpCnBsb3QoIE5VTEwgLCB4bGltPXJhbmdlKGQkYWdlKSAsIHlsaW09cmFuZ2UoZCR3ZWlnaHQpLCB4bGFiPSJhZ2UiLCB5bGFiPSJ3ZWlnaHQiICkKZm9yICggaSBpbiAxOm4gKSBhYmxpbmUoIGFbaV0gLCBiW2ldICwgbHdkPTMgLCBjb2w9MiApCmBgYAoKVGhlc2UgYXJlIGd1ZXNzZXMgdGhhdCBpbmNsdWRlIHRoYXQgdGhlIHJlbGF0aW9uc2hpcCBtdXN0IGJlIHBvc2l0aXZlIGFuZCB0aGF0IHdlaWdodCBhdCBhZ2UgemVybyBpcyBiaXJ0aCB3ZWlnaHQsIGFuIGF2ZXJhZ2UgYXJvdW5kIDUga2cuCgpgYGB7cn0KbTIgPC0gcXVhcCgKICAgIGFsaXN0KAogICAgICAgIFcgfiBkbm9ybSggbXUgLCBzaWdtYSApLAogICAgICAgIG11IDwtIGEgKyBiKkEsCiAgICAgICAgYSB+IGRub3JtKDUsMSksCiAgICAgICAgYiB+IGRsbm9ybSgwLDEpLAogICAgICAgIHNpZ21hIH4gZGV4cCgxKQogICAgKSwgZGF0YT1saXN0KFc9ZCR3ZWlnaHQsQT1kJGFnZSkgKQoKcHJlY2lzKG0yKQpgYGAKClRoZSBjYXVzYWwgZWZmZWN0IG9mIGVhY2ggeWVhciBvZiBncm93dGggaXMgZ2l2ZW4gYnkgdGhlIHBhcmFtZXRlciBgYmAsIHNvIGl0cyA4OSUgaW50ZXJ2YWwgaXMgMS4yOSB0byAxLjQ2IGtnIC8geWVhci4KCkxldCdzIGZpcnN0IGxvb2sgYXQgdGhlIG1lYW5zLgoKYGBge3J9CiMgYXBwcm9hY2ggMTogdXNlIGV4dHJhY3Quc2FtcGxlcwpwbG90KCBkJGFnZSAsIGQkd2VpZ2h0ICwgbHdkPTMsIGNvbD0yICkKcG9zdCA8LSBleHRyYWN0LnNhbXBsZXMobTIpCmZvciAoIGkgaW4gMToxMCApIGFibGluZSggcG9zdCRhW2ldICwgcG9zdCRiW2ldICwgbHdkPTMgLCBjb2w9MSApCmBgYAoKYGBge3J9CiMgYXBwcm9hY2ggMjogdXNlIGxpbmsgZnVuY3Rpb24Kc2ltX249MTAwCgp4c2VxIDwtIHNlcShmcm9tPTAsdG89MTIsYnk9MSkKbXUgPC0gbGluayhtMixkYXRhPWxpc3QoIEE9eHNlcSksIHNpbV9uKQptdS5tZWFuIDwtIGFwcGx5KCBtdSAsIDIgLCBtZWFuICkKCnBsb3QoZCRhZ2UsIGQkd2VpZ2h0LCBjb2w9MiAsIGx3ZD0zLAogICAgIGNleD0xLjIsIHhsYWI9ImhlaWdodCAoY20pIiwgeWxhYj0id2VpZ2h0IChrZykiKQpmb3IgKCBpIGluIDE6c2ltX24gKQogICAgbGluZXMoIHhzZXEgLCBtdVtpLF0gLCBwY2g9MTYsIGNvbD0xICkKYGBgCgoKIyMgUXVlc3Rpb24gMgoKTm93IHN1cHBvc2UgdGhlIGNhdXNhbCBhc3NvY2lhdGlvbiBiZXR3ZWVuIGFnZSBhbmQgd2VpZ2h0IG1pZ2h0IGJlIGRpZmZlcmVudCBmb3IgYm95cyBhbmQgZ2lybHMuIFVzZSBhIHNpbmdsZSBsaW5lYXIgcmVncmVzc2lvbiwgd2l0aCBhIGNhdGVnb3JpY2FsIHZhcmlhYmxlIGZvciBzZXgsIHRvIGVzdGltYXRlIHRoZSB0b3RhbCBjYXVzYWwgZWZmZWN0IG9mIGFnZSBvbiB3ZWlnaHQgc2VwYXJhdGVseSBmb3IgYm95cyBhbmQgZ2lybHMuIEhvdyBkbyBnaXJscyBhbmQgYm95cyBkaWZmZXI/IFByb3ZpZGUgb25lIG9yIG1vcmUgcG9zdGVyaW9yIGNvbnRyYXN0cyBhcyBhIHN1bW1hcnkuCgpgYGB7cn0KbGlicmFyeShyZXRoaW5raW5nKQpkYXRhKEhvd2VsbDEpCmQgPC0gSG93ZWxsMQpkIDwtIGRbIGQkYWdlIDwgMTMgLCBdCgpkYXQgPC0gbGlzdChXPWQkd2VpZ2h0LEE9ZCRhZ2UsUz1kJG1hbGUrMSkKCm0zIDwtIHF1YXAoCiAgICBhbGlzdCgKICAgICAgICBXIH4gZG5vcm0oIG11ICwgc2lnbWEgKSwKICAgICAgICBtdSA8LSBhW1NdICsgYltTXSpBLAogICAgICAgIGFbU10gfiBkbm9ybSg1LDEpLAogICAgICAgIGJbU10gfiBkbG5vcm0oMCwxKSwKICAgICAgICBzaWdtYSB+IGRleHAoMSkKICAgICksIGRhdGE9ZGF0ICkKCiMgYmxhbmsoYnR5PSJuIikKcGxvdCggZCRhZ2UgLCBkJHdlaWdodCAsIGx3ZD0zLCBjb2w9aWZlbHNlKGQkbWFsZT09MSw0LDIpICwgeGxhYj0iYWdlICh5ZWFycykiICwgeWxhYj0id2VpZ2h0IChrZykiICkKQXNlcSA8LSAwOjEyCgojIGdpcmxzCm11RiA8LSBsaW5rKG0zLGRhdGE9bGlzdChBPUFzZXEsUz1yZXAoMSwxMykpKQpzaGFkZSggYXBwbHkobXVGLDIsUEksMC45OSkgLCBBc2VxICwgY29sPWNvbC5hbHBoYSgyLDAuNSkgKQpsaW5lcyggQXNlcSAsIGFwcGx5KG11RiwyLG1lYW4pICwgbHdkPTMgLCBjb2w9MiApCgojIGJveXMKbXVNIDwtIGxpbmsobTMsZGF0YT1saXN0KEE9QXNlcSxTPXJlcCgyLDEzKSkpCnNoYWRlKCBhcHBseShtdU0sMixQSSwwLjk5KSAsIEFzZXEgLCBjb2w9Y29sLmFscGhhKDQsMC41KSApCmxpbmVzKCBBc2VxICwgYXBwbHkobXVNLDIsbWVhbikgLCBsd2Q9MyAsIGNvbD00ICkKYGBgCgpTbyBib3lzIGxvb2sgYSBsaXR0bGUgaGVhdmllciB0aGFuIGdpcmxzIGF0IGFsbCBhZ2VzIGFuZCBzZWVtIHRvIGluY3JlYXNlCnNsaWdodGx5IGZhc3RlciBhcyB3ZWxsLiBMZXTigJlzIGRvIGEgcG9zdGVyaW9yIGNvbnRyYXN0IGFjcm9zcyBhZ2VzIHRob3VnaCwgc28Kd2UgY2FuIGdldCBtYWtlIHN1cmUuCgoKYGBge3J9Ck5fc2ltID0gMTAwCgpBc2VxIDwtIDA6MTIKbXUxIDwtIHNpbShtMyxkYXRhPWxpc3QoQT1Bc2VxLFM9cmVwKDEsMTMpKSwgTl9zaW0pCm11MiA8LSBzaW0obTMsZGF0YT1saXN0KEE9QXNlcSxTPXJlcCgyLDEzKSksIE5fc2ltKQptdV9jb250cmFzdCA8LSBtdTEKZm9yICggaSBpbiAxOjEzICkgbXVfY29udHJhc3RbLGldIDwtIG11MlssaV0gLSBtdTFbLGldCnBsb3QoIE5VTEwgLCB4bGltPWMoMCwxMykgLCB5bGltPWMoLTE1LDE1KSAsIHhsYWI9ImFnZSIgLCB5bGFiPSJ3ZWlnaHQgZGlmZmVyZW5jZSAoYm95cy1naXJscykiICkKCmZvciAoIHAgaW4gYygwLjUsMC42NywwLjg5LDAuOTkpICkKc2hhZGUoIGFwcGx5KG11X2NvbnRyYXN0LDIsUEkscHJvYj1wKSAsIEFzZXEgKQoKYWJsaW5lKGg9MCxsdHk9Mixsd2Q9MikKCmZvciAoIGkgaW4gMToxMyApIHBvaW50cyggcmVwKGktMSxOX3NpbSksIG11X2NvbnRyYXN0WzE6Tl9zaW0saV0gLCBjb2w9aWZlbHNlKG11X2NvbnRyYXN0WzE6Tl9zaW0saV0+MCw0LDIpICwgbHdkPTMgKQpgYGAKClRoZXNlIGNvbnRyYXN0cyB1c2UgdGhlIGVudGlyZSB3ZWlnaHQgZGlzdHJpYnV0aW9uLCBub3QganVzdCB0aGUgZXhwZWN0YXRpb25zLiBCb3lzIGRvIHRlbmQgdG8gYmUgaGVhdmllciB0aGFuIGdpcmxzIGF0IGFsbCBhZ2VzLCBidXQgdGhlIGRpc3RyaWJ1dGlvbnMgb3ZlcmxhcCBhIGxvdC4gVGhlIGRpZmZlcmVuY2UgaW5jcmVhc2VzIHdpdGggYWdlLiBUaGlzIGlzIGdvb2QgbW9tZW50IHRvIHJlcGVhdCBSaWNoYXJkJ3Mgc2VybW9uIG9uIHplcm8uIFRoZSBmYWN0IHRoYXQgdGhlc2UgY29udHJhc3RzIGFsbCBvdmVybGFwIHplcm8gaXMgbm8gcmVhc29uIHRvIGFzc2VydCB0aGF0IHRoZXJlIGlzIG5vIGRpZmZlcmVuY2UgaW4gd2VpZ2h0IGJldHdlZW4gYm95cyBhbmQgZ2lybHMuIFRoYXQgd291bGQgYmUgc2lsbHkuIEJ1dCB0aGF0IGlzIGV4YWN0bHkgd2hhdCByZXNlYXJjaGVycyBkbyBldmVyeSB0aW1lIHRoZXkgbG9vayBpZiBhbiBpbnRlcnZhbCBvdmVybGFwcyB6ZXJvIGFuZCB0aGVuIGFjdCBhcyBpZiB0aGUgZXN0aW1hdGUgd2FzIGV4YWN0bHkgemVyby4KCiMjIFF1ZXN0aW9uIDMKCkZvciB0aGlzIHByb2JsZW0sIHdlIHdhbnQgdG8gY29tcGFyZSB0aGUgZGlmZmVyZW5jZSBiZXR3ZWVuIEZyZXF1ZW50aXN0IGFuZCBCYXllc2lhbiBsaW5lYXIgcmVncmVzc2lvbnMuIFdlJ3JlIGdvaW5nIHRvIHVzZSB0aGUgc2ltaWxhciBmdW5jdGlvbnMgZnJvbSBzZWN0aW9uIDQuNS4KClRvIGJlZ2luLCBydW4gdGhlIHNhbWUgY29kZSB0byBnZXQgdGhlIG1vZGVsIGBtNC41YCAoaS5lLiwgcnVuIFIgY29kZSA0LjY1KS4gCgpgYGB7ciB3YXJuaW5nPUZBTFNFLCBtZXNzYWdlPUZBTFNFfQojIFIgY29kZSA0LjY1ICsgNC42NiAocHJlY2lzKQpsaWJyYXJ5KHJldGhpbmtpbmcpCmRhdGEoSG93ZWxsMSkKZCA8LSBIb3dlbGwxCmQkd2VpZ2h0X3MgPC0gKCBkJHdlaWdodCAtIG1lYW4oZCR3ZWlnaHQpICkvc2QoZCR3ZWlnaHQpCmQkd2VpZ2h0X3MyIDwtIGQkd2VpZ2h0X3NeMgoKbTQuNSA8LSBxdWFwKAogIGFsaXN0KAogICAgaGVpZ2h0IH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICBtdSA8LSBhICsgYjEqKHdlaWdodF9zKSArIGIyICogd2VpZ2h0X3MyLAogICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgIGIxIH4gZGxub3JtKCAwICwgMSApICwKICAgIGIyIH4gZG5vcm0oIDAsIDEgKSAsCiAgICBzaWdtYSB+IGR1bmlmKCAwICwgNTAgKQopICwgZGF0YT1kICkKCnByZWNpcyggbTQuNSApCmBgYAoKTm93IG1vZGlmeSBgbTQuNWAgbW9kZWwgYnkgcmVsYXhpbmcgb3VyICJwb3NpdGl2ZSByZWxhdGlvbnNoaXAiIChha2EgbG9nbm9ybWFsKSBhc3N1bXB0aW9uIGZvciB0aGUgYGIxYCB2YXJpYWJsZSBieSBtb2RpZnlpbmcgaXQncyBwcmlvciBhcyBgZG5vcm0oIDAgLCAxIClgIGFuZCBjcmVhdGUgYSBuZXcgbW9kZWwgY2FsbGVkIGBtNC41YmAuIFJ1biBgcHJlY2lzKG00LjViKWAuCgpgYGB7cn0KbTQuNWIgPC0gcXVhcCgKICBhbGlzdCgKICAgIGhlaWdodCB+IGRub3JtKCBtdSAsIHNpZ21hICkgLAogICAgbXUgPC0gYSArIGIxKih3ZWlnaHRfcykgKyBiMiAqIHdlaWdodF9zMiwKICAgIGEgfiBkbm9ybSggMTc4ICwgMjAgKSAsCiAgICBiMSB+IGRub3JtKCAwICwgMSApICwKICAgIGIyIH4gZG5vcm0oIDAsIDEgKSAsCiAgICBzaWdtYSB+IGR1bmlmKCAwICwgNTAgKQopICwgZGF0YT1kICkKCnByZWNpcyggbTQuNWIgKQpgYGAKCk5vdywgcnVuIGEgZnJlcXVlbnRpc3QgcmVncmVzc2lvbiBvZiBtNC41YiBieSB1c2luZyB0aGUgYGxtYCBmdW5jdGlvbi4gSSBoYXZlIHByb3ZpZGVkIHRoaXMgY29kZS4gCgpgYGB7cn0KIyBoaW50OiB5b3UgbmVlZCB0byBvbmx5IHJlbW92ZSB0aGUgZXZhbD1GQUxTRSBzbyB0aGlzIGNvZGUgcnVucwpmbSA8LSBsbShoZWlnaHQgfiB3ZWlnaHRfcyArIHdlaWdodF9zMiwgZGF0YSA9IGQpCm5hbWVzKGZtJGNvZWZmaWNpZW50cykgPC0gYygnYScsJ2IxJywnYjInKSAjIHJlbmFtZSBjb2VmIGZvciBjb25zaXN0ZW5jeQpmbQpgYGAKTm93IGNvbXBhcmUgYWxsIHRocmVlIG1vZGVscyBieSB1c2luZyB0aGUgYGNvZWZ0YWIoKWAgYW5kIHB1dHRpbmcgYWxsIHRocmVlIG9mIHRoZSBtb2RlbHMgYXMgcGFyYW1ldGVycy4gWW91IGNhbiBhbHNvIHJ1biBhIGBwbG90KClgIG9uIHRoZSBgY29lZnRhYigpYCBmdW5jdGlvbiB0byBydW4gYSBwbG90IG9mIHRoZSBlZmZlY3RzLgoKYGBge3J9CnBsb3QoY29lZnRhYihtNC41LCBtNC41YiwgZm0pKQpgYGAKCkhvdyBkaWZmZXJlbnQgYXJlIHRoZSBtb2RlbHM/CgoKCiMjIFF1ZXN0aW9uIDQKCkZvciB0aGlzIHByb2JsZW0sIHdlJ3JlIGdvaW5nIHRvIHJldXNlIHRoZSBzYW1lIG1vZGVsIChgbTQuNWApIGZyb20gUXVlc3Rpb24gMyBhbmQgcnVuIHByaW9yIHByZWRpY3RpdmUgc2ltdWxhdGlvbnMgdG8gdW5kZXJzdGFuZCB0aGUgcm9sZSBvZiBkaWZmZXJlbnQgcHJpb3JzLiBGb3IgaGVscCwgc2VlIDUuNC01LjUgY29kZSBpbiB0aGUgYm9vay4KCmBgYHtyfQptNC41IDwtIHF1YXAoCiAgYWxpc3QoCiAgICBoZWlnaHQgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgIG11IDwtIGEgKyBiMSood2VpZ2h0X3MpICsgYjIgKiB3ZWlnaHRfczIsCiAgICBhIH4gZG5vcm0oIDE3OCAsIDIwICkgLAogICAgYjEgfiBkbG5vcm0oIDAgLCAxICkgLAogICAgYjIgfiBkbm9ybSggMCwgMSApICwKICAgIHNpZ21hIH4gZHVuaWYoIDAgLCA1MCApCikgLCBkYXRhPWQgKQpzZXQuc2VlZCg0NSkKcHJpb3IgPC0gZXh0cmFjdC5wcmlvcihtNC41KQpwcmVjaXMocHJpb3IpCmBgYAoKYGBge3J9Cndfc2VxIDwtIHNlcSggZnJvbT1taW4oZCR3ZWlnaHRfcykgLCB0bz1tYXgoZCR3ZWlnaHRfcykgLCBsZW5ndGgub3V0PTUwICkKdzJfc2VxIDwtIHdfc2VxXjIKbXUgPC0gbGluayggbTQuNSAsIHBvc3Q9cHJpb3IgLCBkYXRhPWxpc3QoIHdlaWdodF9zPXdfc2VxICwgd2VpZ2h0X3MyPXcyX3NlcSApICkKCnBsb3QoIE5VTEwgLCB4bGltPXJhbmdlKHdfc2VxKSAsIHlsaW09Yyg1NSwyNzApICwKeGxhYj0id2VpZ2h0IChzdGQpIiAsIHlsYWI9ImhlaWdodCIgKQpmb3IgKCBpIGluIDE6NTAgKSBsaW5lcyggd19zZXEgLCBtdVtpLF0gLCBjb2w9Y29sLmFscGhhKCJibGFjayIsMC41KSApCmBgYAoKQ2hhbmdlIHRoZSBwcmlvcnMgb24gdGhlIGBiMmAgY29lZmZpY2llbnQgdG8gYGIyIH4gZG5vcm0oMCwgMTApYCBhbmQgcmVydW4gdGhlIHByaW9yIHByZWRpY3RpdmUgc2ltdWxhdGlvbi4gCgpgYGB7cn0KbTQuNV9hbHRlciA8LSBxdWFwKAogIGFsaXN0KAogICAgaGVpZ2h0IH4gZG5vcm0oIG11ICwgc2lnbWEgKSAsCiAgICBtdSA8LSBhICsgYjEqKHdlaWdodF9zKSArIGIyICogd2VpZ2h0X3MyLAogICAgYSB+IGRub3JtKCAxNzggLCAyMCApICwKICAgIGIxIH4gZGxub3JtKDAsIDEpICwKICAgIGIyIH4gZG5vcm0oMCwgMTApICwgIyB1cGRhdGVkIHByaW9yCiAgICBzaWdtYSB+IGR1bmlmKCAwICwgNTAgKQopICwgZGF0YT1kICkKCnByaW9yIDwtIGV4dHJhY3QucHJpb3IobTQuNV9hbHRlcikKCm11IDwtIGxpbmsoIG00LjVfYWx0ZXIgLCBwb3N0PXByaW9yICwgZGF0YT1saXN0KCB3ZWlnaHRfcz13X3NlcSAsIHdlaWdodF9zMj13Ml9zZXEgKSApCgpwbG90KCBOVUxMICwgeGxpbT1yYW5nZSh3X3NlcSkgLCB5bGltPWMoNTUsMjcwKSAsCnhsYWI9IndlaWdodCAoc3RkKSIgLCB5bGFiPSJoZWlnaHQiICkKZm9yICggaSBpbiAxOjUwICkgbGluZXMoIHdfc2VxICwgbXVbaSxdICwgY29sPWNvbC5hbHBoYSgiYmxhY2siLDAuNSkgKQpgYGAKCk5vdywgY2hhbmdlIHRoZSBwcmlvcnMgb24gdGhlIGJldGEgY29lZmZpY2llbnRzIHRvIG1vcmUgImZsYXQsIHZlcnkgdW5pbmZvcm1hdGl2ZSIgcHJpb3JzLCBgZG5vcm0oMCwgMTAwKWAgZm9yIGBiMWAgYW5kIGBiMmAuIFJlcnVuIGEgc2ltaWxhciBwcmlvciBwcmVkaWN0aXZlIHNpbXVsYXRpb24uCgpgYGB7cn0KbTQuNV9mbGF0IDwtIHF1YXAoCiAgYWxpc3QoCiAgICBoZWlnaHQgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgIG11IDwtIGEgKyBiMSood2VpZ2h0X3MpICsgYjIgKiB3ZWlnaHRfczIsCiAgICBhIH4gZG5vcm0oIDE3OCAsIDIwICkgLAogICAgYjEgfiBkbm9ybSgwLCAxMDApICwgIyB2ZXJ5IGZsYXQgcHJpb3JzCiAgICBiMiB+IGRub3JtKDAsIDEwMCkgLCAjIHZlcnkgZmxhdCBwcmlvcnMKICAgIHNpZ21hIH4gZHVuaWYoIDAgLCA1MCApCikgLCBkYXRhPWQgKQoKcHJpb3IgPC0gZXh0cmFjdC5wcmlvcihtNC41X2ZsYXQpCgptdSA8LSBsaW5rKCBtNC41X2ZsYXQgLCBwb3N0PXByaW9yICwgZGF0YT1saXN0KCB3ZWlnaHRfcz13X3NlcSAsIHdlaWdodF9zMj13Ml9zZXEgKSApCgpwbG90KCBOVUxMICwgeGxpbT1yYW5nZSh3X3NlcSkgLCB5bGltPWMoNTUsMjcwKSAsCnhsYWI9IndlaWdodCAoc3RkKSIgLCB5bGFiPSJoZWlnaHQiICkKZm9yICggaSBpbiAxOjUwICkgbGluZXMoIHdfc2VxICwgbXVbaSxdICwgY29sPWNvbC5hbHBoYSgiYmxhY2siLDAuNSkgKQpgYGAKCldoYXQgYXJlIHRoZSBjb25zZXF1ZW5jZXMgZm9yIHVzaW5nIGEgZmxhdHRlciBwcmlvcj8gRXhwbGFpbiB3aGF0IHlvdSBzdXNwZWN0IGlzIG9jY3VyaW5nLgoKCmBgYHtyIGV2YWw9RkFMU0UsIGluY2x1ZGU9RkFMU0V9CiMgdHlwZSBpbiB5b3VyIGNvZGUgaGVyZQoKYGBgCgojIyBPcHRpb25hbCBDaGFsbGVuZ2UgKE5vdCBncmFkZWQpCgpSZXR1cm4gdG8gYGRhdGEoY2hlcnJ5X2Jsb3Nzb21zKWAgYW5kIG1vZGVsIHRoZSBhc3NvY2lhdGlvbiBiZXR3ZWVuIGJsb3Nzb20gZGF0ZSAoYGRheWApIGFuZCBNYXJjaCB0ZW1wZXJhdHVyZSAoYHRlbXBgKS4gTm90ZSB0aGF0IHRoZXJlIGFyZSBtYW55IG1pc3NpbmcgdmFsdWVzIGluIGJvdGggdmFyaWFibGVzLiBZb3UgbWF5IGNvbnNpZGVyIGEgbGluZWFyIG1vZGVsLCBhIHBvbHlub21pYWwsIG9yIGEgc3BsaW5lIG9uIHRlbXBlcmF0dXJlLiBIb3cgd2VsbCBkb2VzIHRlbXBlcmF0dXJlIHRyZW5kIHByZWRpY3QgdGhlIGJsb3Nzb20gdHJlbmQ/CgpGaXJzdCwgbGV0J3MgbG9vayBmb3IgbWlzc2luZyB2YWx1ZXMuCgpgYGB7cn0KbGlicmFyeShyZXRoaW5raW5nKQpkYXRhKGNoZXJyeV9ibG9zc29tcykKY29sU3VtcyggaXMubmEoY2hlcnJ5X2Jsb3Nzb21zKSApCmBgYAoKTGV0J3MganVzdCBzZWxlY3QgYGRveWAgYW5kIGB0ZW1wYC4KCmBgYHtyfQpkIDwtIGNoZXJyeV9ibG9zc29tcwpkMiA8LSBkWyBjb21wbGV0ZS5jYXNlcyggZCRkb3kgLCBkJHRlbXAgKSAsIGMoImRveSIsInRlbXAiKSBdCiMgb3RoZXIgd2F5cyB0byB3cml0ZSB0aGlzIHVzaW5nIHRpZHl2ZXJzZQojIGQyIDwtIHRpZHlyOjpkcm9wX25hKGQsIGMoImRveSIsInRlbXAiKSkKIyBkMiA8LSBkcGx5cjo6ZmlsdGVyKGQsICFpcy5uYSgiZG95IikgfCAhaXMubmEoInRlbXAiKSkKYGBgCgpgYGB7cn0KbnVtX2tub3RzIDwtIDMwCmtub3RfbGlzdCA8LSBxdWFudGlsZSggZDIkdGVtcCAsIHByb2JzPXNlcSgwLDEsbGVuZ3RoLm91dD1udW1fa25vdHMpICkKbGlicmFyeShzcGxpbmVzKQpCIDwtIGJzKGQyJHRlbXAsCmtub3RzPWtub3RfbGlzdFstYygxLG51bV9rbm90cyldICwKZGVncmVlPTMgLCBpbnRlcmNlcHQ9VFJVRSApCmBgYAoKYGBge3J9Cm00SDUgPC0gcXVhcCgKICBhbGlzdCgKICAgIEQgfiBkbm9ybSggbXUgLCBzaWdtYSApICwKICAgIG11IDwtIGEgKyBCICUqJSB3ICwKICAgIGEgfiBkbm9ybSgxMDAsMTApLAogICAgdyB+IGRub3JtKDAsMTApLAogICAgc2lnbWEgfiBkZXhwKDEpCiAgICApLCAKICBkYXRhPWxpc3QoIEQ9ZDIkZG95ICwgQj1CICkgLAogIHN0YXJ0PWxpc3QoIHc9cmVwKCAwICwgbmNvbChCKSApICkgCiAgKQpgYGAKCllvdSBjYW4gaW5zcGVjdCB0aGUgcHJlY2lzIG91dHB1dCwgaWYgeW91IGxpa2UuIFRoZSB3ZWlnaHRzIGFyZW7igJl0IGdvaW5nIHRvIGJlIG1lYW5pbmdmdWwgdG8geW91LiBMZXTigJlzIHBsb3QuIFRoZSBvbmx5IHRyaWNrIGhlcmUgaXMgdG8gZ2V0IHRoZSBvcmRlciBvZiB0aGUgdGVtcGVyYXR1cmUgdmFsdWVzIHJpZ2h0IHdoZW4gd2UgcGxvdCwgc2luY2UgdGhleSBhcmUgbm90IG9yZGVyZWQgaW4gdGhlIGRhdGEgb3IgaW4gdGhlIGJhc2lzIGZ1bmN0aW9ucy4gV2UgY2FuIGRvIHRoaXMgd2l0aCBvcmRlciB0byBnZXQgdGhlIGluZGV4IHZhbHVlcyBmb3IgdGhlIHByb3BlciBvcmRlciBhbmQgdGhlbiBpbmRleCBldmVyeXRoaW5nIGVsc2UgYnkgdGhpczoKCmBgYHtyfQptdSA8LSBsaW5rKCBtNEg1ICkKbXVfbWVhbiA8LSBhcHBseSggbXUgLCAyICwgbWVhbiApCm11X1BJIDwtIGFwcGx5KCBtdSAsIDIgLCBQSSwgMC45NyApCnBsb3QoIGQyJHRlbXAgLCBkMiRkb3kgLCBjb2w9Y29sLmFscGhhKHJhbmdpMiwwLjMpICwgcGNoPTE2ICwKeGxhYj0idGVtcCIgLCB5bGFiPSJkb3kiICkKbyA8LSBvcmRlciggZDIkdGVtcCApCmxpbmVzKCBkMiR0ZW1wW29dICwgbXVfbWVhbltvXSAsIGx3ZD0zICkKc2hhZGUoIG11X1BJWyxvXSAsIGQyJHRlbXBbb10gLCBjb2w9Z3JhdSgwLjMpICkKYGBgCgpUaGVyZSBpcyBhIHNpbGx5IGFtb3VudCBvZiB3aWdnbGUgaW4gdGhpcyBzcGxpbmUuIEkgdXNlZCAzMCBrbm90cyBhbmQgcXVpdGUgbG9vc2UgcHJpb3Igd2VpZ2h0cywgc28gdGhpcyB3aWdnbGUgaXNu4oCZdCB1bmV4cGVjdGVkLiBJdCBhbHNvIHByb2JhYmx5IGlzbuKAmXQgdGVsbGluZyB1cyBhbnl0aGluZyBjYXVzYWwuIE92ZXJhbGwgdGhlIHRyZW5kIGlzIHF1aXRlIGxpbmVhciwgYXNpZGUgZnJvbSB0aGUgb2RkIGRyb3AganVzdCBiZWZvcmUgNiBkZWdyZWVzLiBUaGlzIGNvdWxkIGJlIHJlYWwsIG9yIGl0IGNvdWxkIGJlIGFuIGFydGlmYWN0IG9mIGNoYW5nZXMgaW4gdGhlIHJlY29yZCBrZWVwaW5nLiBUaGUgY29sZGVyIGRhdGVzIGFyZSBhbHNvIG9sZGVyIGFuZCB0aGUgdGVtcGVyYXR1cmVzIGZvciBvbGRlciBkYXRlcyB3ZXJlIGVzdGltYXRlZCBkaWZmZXJlbnRseS4KCg==" download="03-problem-set-solutions.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this file</button>
</a>

## Question 1

From the Howell1 dataset, consider only the people younger than 13 years old. Estimate the causal association between age and weight. Assume that age influences weight through two paths. First, age influences height, and height influences weight. Second, age directly influences weight through age related changes in muscle growth and body proportions. All of this implies this causal model (DAG):

``` r
library(dagitty)

g <- dagitty('dag {
bb="0,0,1,1"
A [pos="0.251,0.481"]
H [pos="0.350,0.312"]
W [pos="0.452,0.484"]
A -> H
A -> W
H -> W
}
')
plot(g)
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-2-1.png" width="192" />

Use a linear regression to estimate the total (not just direct) causal effect of
each year of growth on weight. Be sure to carefully consider the priors. Try
using prior predictive simulation to assess what they imply.

``` r
library(rethinking)
data(Howell1)
d <- Howell1
d <- d[ d$age < 13 , ]

# sim from priors
n <- 10
a <- rnorm(n,5,1)
b <- rlnorm(n,0,1)
# blank(bty="n")
plot( NULL , xlim=range(d$age) , ylim=range(d$weight), xlab="age", ylab="weight" )
for ( i in 1:n ) abline( a[i] , b[i] , lwd=3 , col=2 )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-3-1.png" width="672" />

These are guesses that include that the relationship must be positive and that weight at age zero is birth weight, an average around 5 kg.

``` r
m2 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a + b*A,
        a ~ dnorm(5,1),
        b ~ dlnorm(0,1),
        sigma ~ dexp(1)
    ), data=list(W=d$weight,A=d$age) )

precis(m2)
```

``` language-r
##           mean         sd     5.5%    94.5%
## a     7.179132 0.33980549 6.636057 7.722206
## b     1.373792 0.05243257 1.289995 1.457589
## sigma 2.507394 0.14535422 2.275090 2.739698
```

The causal effect of each year of growth is given by the parameter `b`, so its 89% interval is 1.29 to 1.46 kg / year.

Let’s first look at the means.

``` r
# approach 1: use extract.samples
plot( d$age , d$weight , lwd=3, col=2 )
post <- extract.samples(m2)
for ( i in 1:10 ) abline( post$a[i] , post$b[i] , lwd=3 , col=1 )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-5-1.png" width="672" />

``` r
# approach 2: use link function
sim_n=100

xseq <- seq(from=0,to=12,by=1)
mu <- link(m2,data=list( A=xseq), sim_n)
mu.mean <- apply( mu , 2 , mean )

plot(d$age, d$weight, col=2 , lwd=3,
     cex=1.2, xlab="height (cm)", ylab="weight (kg)")
for ( i in 1:sim_n )
    lines( xseq , mu[i,] , pch=16, col=1 )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-6-1.png" width="672" />

## Question 2

Now suppose the causal association between age and weight might be different for boys and girls. Use a single linear regression, with a categorical variable for sex, to estimate the total causal effect of age on weight separately for boys and girls. How do girls and boys differ? Provide one or more posterior contrasts as a summary.

``` r
library(rethinking)
data(Howell1)
d <- Howell1
d <- d[ d$age < 13 , ]

dat <- list(W=d$weight,A=d$age,S=d$male+1)

m3 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a[S] + b[S]*A,
        a[S] ~ dnorm(5,1),
        b[S] ~ dlnorm(0,1),
        sigma ~ dexp(1)
    ), data=dat )

# blank(bty="n")
plot( d$age , d$weight , lwd=3, col=ifelse(d$male==1,4,2) , xlab="age (years)" , ylab="weight (kg)" )
Aseq <- 0:12

# girls
muF <- link(m3,data=list(A=Aseq,S=rep(1,13)))
shade( apply(muF,2,PI,0.99) , Aseq , col=col.alpha(2,0.5) )
lines( Aseq , apply(muF,2,mean) , lwd=3 , col=2 )

# boys
muM <- link(m3,data=list(A=Aseq,S=rep(2,13)))
shade( apply(muM,2,PI,0.99) , Aseq , col=col.alpha(4,0.5) )
lines( Aseq , apply(muM,2,mean) , lwd=3 , col=4 )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-7-1.png" width="672" />

So boys look a little heavier than girls at all ages and seem to increase
slightly faster as well. Let’s do a posterior contrast across ages though, so
we can get make sure.

``` r
N_sim = 100

Aseq <- 0:12
mu1 <- sim(m3,data=list(A=Aseq,S=rep(1,13)), N_sim)
mu2 <- sim(m3,data=list(A=Aseq,S=rep(2,13)), N_sim)
mu_contrast <- mu1
for ( i in 1:13 ) mu_contrast[,i] <- mu2[,i] - mu1[,i]
plot( NULL , xlim=c(0,13) , ylim=c(-15,15) , xlab="age" , ylab="weight difference (boys-girls)" )

for ( p in c(0.5,0.67,0.89,0.99) )
shade( apply(mu_contrast,2,PI,prob=p) , Aseq )

abline(h=0,lty=2,lwd=2)

for ( i in 1:13 ) points( rep(i-1,N_sim), mu_contrast[1:N_sim,i] , col=ifelse(mu_contrast[1:N_sim,i]>0,4,2) , lwd=3 )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-8-1.png" width="672" />

These contrasts use the entire weight distribution, not just the expectations. Boys do tend to be heavier than girls at all ages, but the distributions overlap a lot. The difference increases with age. This is good moment to repeat Richard’s sermon on zero. The fact that these contrasts all overlap zero is no reason to assert that there is no difference in weight between boys and girls. That would be silly. But that is exactly what researchers do every time they look if an interval overlaps zero and then act as if the estimate was exactly zero.

## Question 3

For this problem, we want to compare the difference between Frequentist and Bayesian linear regressions. We’re going to use the similar functions from section 4.5.

To begin, run the same code to get the model `m4.5` (i.e., run R code 4.65).

``` r
# R code 4.65 + 4.66 (precis)
library(rethinking)
data(Howell1)
d <- Howell1
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2

m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*(weight_s) + b2 * weight_s2,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0, 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d )

precis( m4.5 )
```

``` language-r
##             mean        sd       5.5%      94.5%
## a     146.057480 0.3689762 145.467785 146.647175
## b1     21.732942 0.2888893  21.271241  22.194643
## b2     -7.803271 0.2741845  -8.241471  -7.365071
## sigma   5.774489 0.1764663   5.492462   6.056516
```

Now modify `m4.5` model by relaxing our “positive relationship” (aka lognormal) assumption for the `b1` variable by modifying it’s prior as `dnorm( 0 , 1 )` and create a new model called `m4.5b`. Run `precis(m4.5b)`.

``` r
m4.5b <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*(weight_s) + b2 * weight_s2,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0, 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d )

precis( m4.5b )
```

``` language-r
##             mean        sd       5.5%      94.5%
## a     146.857010 0.3725669 146.261576 147.452444
## b1     20.030280 0.2933640  19.561427  20.499132
## b2     -8.604422 0.2743681  -9.042915  -8.165929
## sigma   5.892846 0.1872169   5.593637   6.192055
```

Now, run a frequentist regression of m4.5b by using the `lm` function. I have provided this code.

``` r
# hint: you need to only remove the eval=FALSE so this code runs
fm <- lm(height ~ weight_s + weight_s2, data = d)
names(fm$coefficients) <- c('a','b1','b2') # rename coef for consistency
fm
```

``` language-r
## 
## Call:
## lm(formula = height ~ weight_s + weight_s2, data = d)
## 
## Coefficients:
##       a       b1       b2  
## 146.660   21.415   -8.412
```

Now compare all three models by using the `coeftab()` and putting all three of the models as parameters. You can also run a `plot()` on the `coeftab()` function to run a plot of the effects.

``` r
plot(coeftab(m4.5, m4.5b, fm))
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-12-1.png" width="672" />

How different are the models?

## Question 4

For this problem, we’re going to reuse the same model (`m4.5`) from Question 3 and run prior predictive simulations to understand the role of different priors. For help, see 5.4-5.5 code in the book.

``` r
m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*(weight_s) + b2 * weight_s2,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0, 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d )
set.seed(45)
prior <- extract.prior(m4.5)
precis(prior)
```

``` language-r
##               mean         sd        5.5%      94.5%     histogram
## a     177.60867247 20.7249835 144.1785056 211.421234      ▁▁▃▇▇▂▁▁
## b1      1.61236478  1.8754510   0.1867143   4.433442    ▇▂▁▁▁▁▁▁▁▁
## b2     -0.05036862  0.9688063  -1.6422983   1.447121 ▁▁▁▂▃▅▇▇▅▃▁▁▁
## sigma  25.14432248 14.5856372   2.5161320  47.371388    ▇▇▅▇▇▇▇▇▇▇
```

``` r
w_seq <- seq( from=min(d$weight_s) , to=max(d$weight_s) , length.out=50 )
w2_seq <- w_seq^2
mu <- link( m4.5 , post=prior , data=list( weight_s=w_seq , weight_s2=w2_seq ) )

plot( NULL , xlim=range(w_seq) , ylim=c(55,270) ,
xlab="weight (std)" , ylab="height" )
for ( i in 1:50 ) lines( w_seq , mu[i,] , col=col.alpha("black",0.5) )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-14-1.png" width="672" />

Change the priors on the `b2` coefficient to `b2 ~ dnorm(0, 10)` and rerun the prior predictive simulation.

``` r
m4.5_alter <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*(weight_s) + b2 * weight_s2,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm(0, 1) ,
    b2 ~ dnorm(0, 10) , # updated prior
    sigma ~ dunif( 0 , 50 )
) , data=d )

prior <- extract.prior(m4.5_alter)

mu <- link( m4.5_alter , post=prior , data=list( weight_s=w_seq , weight_s2=w2_seq ) )

plot( NULL , xlim=range(w_seq) , ylim=c(55,270) ,
xlab="weight (std)" , ylab="height" )
for ( i in 1:50 ) lines( w_seq , mu[i,] , col=col.alpha("black",0.5) )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-15-1.png" width="672" />

Now, change the priors on the beta coefficients to more “flat, very uninformative” priors, `dnorm(0, 100)` for `b1` and `b2`. Rerun a similar prior predictive simulation.

``` r
m4.5_flat <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*(weight_s) + b2 * weight_s2,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dnorm(0, 100) , # very flat priors
    b2 ~ dnorm(0, 100) , # very flat priors
    sigma ~ dunif( 0 , 50 )
) , data=d )

prior <- extract.prior(m4.5_flat)

mu <- link( m4.5_flat , post=prior , data=list( weight_s=w_seq , weight_s2=w2_seq ) )

plot( NULL , xlim=range(w_seq) , ylim=c(55,270) ,
xlab="weight (std)" , ylab="height" )
for ( i in 1:50 ) lines( w_seq , mu[i,] , col=col.alpha("black",0.5) )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-16-1.png" width="672" />

What are the consequences for using a flatter prior? Explain what you suspect is occuring.

## Optional Challenge (Not graded)

Return to `data(cherry_blossoms)` and model the association between blossom date (`day`) and March temperature (`temp`). Note that there are many missing values in both variables. You may consider a linear model, a polynomial, or a spline on temperature. How well does temperature trend predict the blossom trend?

First, let’s look for missing values.

``` r
library(rethinking)
data(cherry_blossoms)
colSums( is.na(cherry_blossoms) )
```

``` language-r
##       year        doy       temp temp_upper temp_lower 
##          0        388         91         91         91
```

Let’s just select `doy` and `temp`.

``` r
d <- cherry_blossoms
d2 <- d[ complete.cases( d$doy , d$temp ) , c("doy","temp") ]
# other ways to write this using tidyverse
# d2 <- tidyr::drop_na(d, c("doy","temp"))
# d2 <- dplyr::filter(d, !is.na("doy") | !is.na("temp"))
```

``` r
num_knots <- 30
knot_list <- quantile( d2$temp , probs=seq(0,1,length.out=num_knots) )
library(splines)
B <- bs(d2$temp,
knots=knot_list[-c(1,num_knots)] ,
degree=3 , intercept=TRUE )
```

``` r
m4H5 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(100,10),
    w ~ dnorm(0,10),
    sigma ~ dexp(1)
    ), 
  data=list( D=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) 
  )
```

You can inspect the precis output, if you like. The weights aren’t going to be meaningful to you. Let’s plot. The only trick here is to get the order of the temperature values right when we plot, since they are not ordered in the data or in the basis functions. We can do this with order to get the index values for the proper order and then index everything else by this:

``` r
mu <- link( m4H5 )
mu_mean <- apply( mu , 2 , mean )
mu_PI <- apply( mu , 2 , PI, 0.97 )
plot( d2$temp , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 ,
xlab="temp" , ylab="doy" )
o <- order( d2$temp )
lines( d2$temp[o] , mu_mean[o] , lwd=3 )
shade( mu_PI[,o] , d2$temp[o] , col=grau(0.3) )
```

<img src="/assignment/03-problem-set-solutions_files/figure-html/unnamed-chunk-22-1.png" width="672" />

There is a silly amount of wiggle in this spline. I used 30 knots and quite loose prior weights, so this wiggle isn’t unexpected. It also probably isn’t telling us anything causal. Overall the trend is quite linear, aside from the odd drop just before 6 degrees. This could be real, or it could be an artifact of changes in the record keeping. The colder dates are also older and the temperatures for older dates were estimated differently.
