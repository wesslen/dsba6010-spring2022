---
date: "2022-02-20"
title: "Class 6"
menu:
  example:
    parent: Labs
weight: 6
toc: true
type: docs
---

<script src="/rmarkdown-libs/clipboard/clipboard.min.js"></script>
<link href="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.css" rel="stylesheet" />
<script src="/rmarkdown-libs/xaringanExtra-clipboard/xaringanExtra-clipboard.js"></script>
<script>window.xaringanExtraClipboard(null, {"button":"Copy Code","success":"Copied!","error":"Press Ctrl+C to Copy"})</script>
<script src="/rmarkdown-libs/font-awesome/js/script.js"></script>
<a href="data:text/x-markdown;base64,LS0tCmRhdGU6ICJgciBTeXMuRGF0ZSgpYCIKdGl0bGU6ICJDbGFzcyA2IgotLS0KCmBgYHtyIHNldHVwLCBpbmNsdWRlPUZBTFNFfQprbml0cjo6b3B0c19jaHVuayRzZXQoZWNobyA9IFRSVUUsIG1lc3NhZ2UgPSBGQUxTRSwgd2FybmluZyA9IEZBTFNFKQpgYGAKCiMjIEludHJvZHVjdGlvbgoKIyMgSW4tQ2xhc3MgUHJhY3RpY2UKCkZvciBpbi1jbGFzcyBwcmFjdGljZSwgd2UncmUgZ29pbmcgdG8gdXNlIFtkYWdpdHR5Lm5ldF0oaHR0cHM6Ly9kYWdpdHR5Lm5ldCkuIERhZ2l0dHkubmV0IHdhcyBpbnRyb2R1Y2VkIGluIFtMZWN0dXJlIDZdKGh0dHBzOi8vd3d3LnlvdXR1YmUuY29tL3dhdGNoP3Y9TlN1VGFlVzZPcmMmdD0yMzk4cykuIFdlJ3JlIGdvaW5nIHRvIGNvbnNpZGVyIGEgZmV3IGRpZmZlcmVudCBleGFtcGxlcyBvZiBkaWZmZXJlbnQgREFHcyBhbmQgY29uc2lkZXIgdGhlIGltcGxpY2F0aW9ucyBvZiB0aGVpciBzdHJ1Y3R1cmUuCgpNYWxhcmlhIGlzIGEgbGlmZS10aHJlYXRlbmluZyBkaXNlYXNlIGNhdXNlZCBieSBwYXJhc2l0ZXMgdGhhdCBhcmUgdHJhbnNtaXR0ZWQgdG8gcGVvcGxlIHRocm91Z2ggdGhlIGJpdGVzIG9mIGluZmVjdGVkIGZlbWFsZSBBbm9waGVsZXMgbW9zcXVpdG9lcyAoW1dIT10oaHR0cHM6Ly93d3cud2hvLmludC9uZXdzLXJvb20vZmFjdC1zaGVldHMvZGV0YWlsL21hbGFyaWEpKS4gSXQgaXMgcHJldmVudGFibGUgYW5kIGN1cmFibGUuIEluIDIwMjAsIHRoZXJlIHdlcmUgYW4gZXN0aW1hdGVkIDI0MSBtaWxsaW9uIGNhc2VzIG9mIG1hbGFyaWEgd29ybGR3aWRlLiBUaGUgZXN0aW1hdGVkIG51bWJlciBvZiBtYWxhcmlhIGRlYXRocyBzdG9vZCBhdCAqKjYyNywwMDAqKiBpbiAyMDIwLiBUaGUgV0hPIEFmcmljYW4gUmVnaW9uIGNhcnJpZXMgYSBkaXNwcm9wb3J0aW9uYXRlbHkgaGlnaCBzaGFyZSBvZiB0aGUgZ2xvYmFsIG1hbGFyaWEgYnVyZGVuLiBJbiAyMDIwLCB0aGUgcmVnaW9uIHdhcyBob21lIHRvIDk1JSBvZiBtYWxhcmlhIGNhc2VzIGFuZCA5NiUgb2YgbWFsYXJpYSBkZWF0aHMuIENoaWxkcmVuIHVuZGVyIDUgYWNjb3VudGVkIGZvciBhbiBlc3RpbWF0ZWQgODAlIG9mIGFsbCBtYWxhcmlhIGRlYXRocyBpbiB0aGUgUmVnaW9uLgoKV2UncmUgZ29pbmcgdG8gbG9vayBhdCB1bmRlcnN0YW5kaW5nIHRoZSBlZmZlY3Qgb2YgbW9zcXVpdG8gbmV0cyBvbiB0aGUgc3ByZWFkIG9mIE1hbGFyaWEuCgojIyMgUHJlcAoKWW91IHdpbGwgbmVlZCB0byBoYXZlIHRoZXNlIHR3byBhZGRpdGlvbmFsIFIgcGFja2FnZXMgaW5zdGFsbGVkLgoKYGBge3IgZXZhbD1GQUxTRX0KaW5zdGFsbC5wYWNrYWdlcygnZGFnaXR0eScpCmluc3RhbGwucGFja2FnZXMoJ2dnZGFnJykKYGBgCgojIyMgUXVlc3Rpb24gMQoKRmlyc3QsIHVzZSBbZGFnaXR0eS5uZXRdKGh0dHBzOi8vZGFnaXR0eS5uZXQpIHRvIGNyZWF0ZSB0aGlzIGluaXRpYWwgZGFnLgoKYGBge3IgZWNobz1GQUxTRSwgb3V0LndpZHRoID0gJzUwJSd9CiMgcnVuIHRoaXMgY2h1bmsgdG8gdmlldyB0aGUgaW1hZ2UKa25pdHI6OmluY2x1ZGVfZ3JhcGhpY3MoImh0dHBzOi8vcmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbS93ZXNzbGVuL2RzYmE2MDEwLXNwcmluZzIwMjIvbWFzdGVyL3N0YXRpYy9pbWcvYXNzaWdubWVudHMvMDQtcHJvYmxlbS1zZXQvMDQtcHJvYmxlbS1zZXQtMS5wbmciKQpgYGAKCk5vdywgZG8gdGhlIHR3byBmb2xsb3dpbmcgc3RlcHM6CgoxLiBzZXQgTW9zcXVpdG8gTmV0IGFzIHRoZSAiZXhwb3N1cmUiIChha2EgdHJlYXRtZW50IG9yIGluZGVwZW5kZW50IHZhcmlhYmxlKSBpbiBEYWdpdHR5LgoyLiBzZXQgUmlzayBvZiBNYWxhcmlhIGFzIHRoZSAib3V0Y29tZSIgKGFrYSByZXNwb25zZSBvciBkZXBlbmRlbnQgdmFyaWFibGUpIGluIERhZ2l0dHkuCgpZb3VyIERBRyBzaG91bGQgbm93IGxvb2sgbGlrZSB0aGlzOgoKYGBge3IgZWNobz1GQUxTRSwgb3V0LndpZHRoID0gJzUwJSd9CiMgcnVuIHRoaXMgY2h1bmsgdG8gdmlldyB0aGUgaW1hZ2UKa25pdHI6OmluY2x1ZGVfZ3JhcGhpY3MoImh0dHBzOi8vcmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbS93ZXNzbGVuL2RzYmE2MDEwLXNwcmluZzIwMjIvbWFzdGVyL3N0YXRpYy9pbWcvYXNzaWdubWVudHMvMDQtcHJvYmxlbS1zZXQvMDQtcHJvYmxlbS1zZXQtMi5wbmciKQpgYGAKCkFuc3dlciB0aGUgZm9sbG93aW5nIGZvdXIgcXVlc3Rpb25zOgoKKipBLiBHaXZlbiB0aGUgREFHLCB3aGF0IHR5cGUgb2YgY29uZm91bmQgaXMgSW5jb21lPyoqCgoqKkIuIEdpdmVuIEluY29tZSdzIGNvbmZvdW5kIHR5cGUsIGlmIHRoZSBnb2FsIGlzIHRvIGZpbmQgdGhlIHRvdGFsIGVmZmVjdCBvZiBNb3NxdWl0byBOZXRzIG9uIHRoZSBSaXNrIG9mIE1hbGFyaWEsIHNob3VsZCB5b3UgaW5jbHVkZSBJbmNvbWUgYXMgYSBwcmVkaWN0b3IgKGFrYSBpbmRlcGVuZGVudCB2YXJpYWJsZSkgaW50byBhIHJlZ3Jlc3Npb24gd2l0aCBNb3NxdWl0byBOZXRzIHRvIGV4cGxhaW4gdGhlIFJpc2sgb2YgTWFsYXJpYT8gV2h5IG9yIHdoeSBub3Q/KioKCioqQy4gVXNpbmcgeW91ciBEYWdpdHR5IERBRywgZmluZCB0aGUgKm1pbmltYWwgYWRqdXN0bWVudCBzZXQqIGZvciBlc3RpbWF0aW5nIHRoZSB0b3RhbCBlZmZlY3Qgb2YgTW9zcXVpdG8gTmV0IG9uIFJpc2sgb2YgTWFsYXJpYS4gSG93IGRvZXMgdGhpcyBjb21wYXJlIHRvIHBhcnQgQj8qKgoKKipELiBOb3cgY2hhbmdlIHRoZSB2YXJpYWJsZXMgaW4geW91ciAqbWluaW1hbCBhZGp1c3RtZW50IHNldCogaW4gUGFydCBDIHRvICJhZGp1c3RlZCIgb24gRGFnaXR0eS4gWW91IGNhbiBkbyB0aGlzIGJ5IHNlbGVjdGluZyB0aGUgbm9kZXMgYW5kIGNsaWNrIHRoZSAiYWRqdXN0ZWQiIGJveCBvbiB0aGUgbGVmdC4gQWZ0ZXIgeW91IGhhdmUgY29tcGxldGVkIHRoaXMsIGNvcHkgYW5kIHBhc3RlIHlvdXIgREFHIFIgQ29kZSBpbiB0aGUgY2h1bmsgYmVsb3cuIEJlIHN1cmUgdG8gY2hhbmdlIHRoZSBjaHVuayBwYXJhbWV0ZXIgZnJvbSBgZXZhbD1GQUxTRWAgdG8gYGV2YWw9VFJVRWAgdG8gZW5zdXJlIHRoZSBjb2RlIGNodW5rIHJ1bnMuKioKCmBgYHtyIGV2YWw9RkFMU0V9CmcgPC0gZGFnaXR0eTo6ZGFnaXR0eSgnCiMgaW5zZXJ0IHlvdXIgZGFnaXR0eSBjb3B5L3Bhc3RlIGNvZGUgaGVyZSAgICAgICAgICAgICAgICAgICAgICAKJykKZ2dkYWc6OmdnZGFnX3N0YXR1cyhnLCB0ZXh0ID0gRkFMU0UsIHVzZV9sYWJlbHMgPSAibmFtZSIpICsKICBndWlkZXMoY29sb3IgPSAibm9uZSIpICsgICMgVHVybiBvZmYgbGVnZW5kCiAgdGhlbWVfZGFnKCkKYGBgCgpUaGlzIGlzIHdoYXQgeW91ciBEQUcgYWJvdmUgc2hvdWxkIGxvb2sgbGlrZToKCmBgYHtyIGVjaG89RkFMU0UsIG91dC53aWR0aCA9ICc1MCUnfQojIHJ1biB0aGlzIGNodW5rIHRvIHZpZXcgdGhlIGltYWdlCmtuaXRyOjppbmNsdWRlX2dyYXBoaWNzKCJodHRwczovL3Jhdy5naXRodWJ1c2VyY29udGVudC5jb20vd2Vzc2xlbi9kc2JhNjAxMC1zcHJpbmcyMDIyL21hc3Rlci9zdGF0aWMvaW1nL2Fzc2lnbm1lbnRzLzA0LXByb2JsZW0tc2V0LzA0LXByb2JsZW0tc2V0LTMucG5nIikKYGBgCgojIyMgUXVlc3Rpb24gMgoKTGV0J3Mgbm93IGFkZCBpbiBtb3JlIGNvbXBsZXhpdHkgdG8gb3VyIERBRy4KCkZpcnN0LCBnbyBiYWNrIGFuZCAqKnJlbW92ZSAiSW5jb21lIiBhcyBhbiAiQWRqdXN0ZWQiIGNoZWNrIGJveC4qKgoKYGBge3IgZWNobz1GQUxTRSwgb3V0LndpZHRoID0gJzc1JSd9CiMgcnVuIHRoaXMgY2h1bmsgdG8gdmlldyB0aGUgaW1hZ2UKa25pdHI6OmluY2x1ZGVfZ3JhcGhpY3MoImh0dHBzOi8vcmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbS93ZXNzbGVuL2RzYmE2MDEwLXNwcmluZzIwMjIvbWFzdGVyL3N0YXRpYy9pbWcvYXNzaWdubWVudHMvMDQtcHJvYmxlbS1zZXQvMDQtcHJvYmxlbS1zZXQtNC5wbmciKQpgYGAKCk5vdywgYWRkIGluIGFkZGl0aW9uYWwgbm9kZXMgaW4geW91ciBEQUcgdG8gZ2V0IHRoZSBkYWcgYWJvdmUuIAoKTm90ZSB0aGF0IGluIHRoaXMgREFHIG9uICJNb3NxdWl0byBOZXQiIChleHBvc3VyZSkgYW5kICJSaXNrIG9mIE1hbGFyaWEiIChvdXRjb21lKSBoYXZlIGJlZW4gYXNzaWduZWQgKGkuZS4sIG5vIG90aGVyIG5vZGVzIGFyZSBzZWxlY3RlZCBhcyAiYWRqdXN0ZWQiKS4KCkFuc3dlciB0aGUgZm9sbG93aW5nIGZvdXIgcXVlc3Rpb25zOgoKKipBLiBHaXZlbiB0aGUgREFHLCB3aGF0IGlzIHRoZSBtaW5pbWFsIHN1ZmZpY2llbnQgYWRqdXN0bWVudCBzZXRzIGZvciBlc3RpbWF0aW5nIHRoZSB0b3RhbCBlZmZlY3Qgb2YgTW9zcXVpdG8gTmV0IG9uIFJpc2sgb2YgTWFsYXJpYT8qKgoKKipCLiBXaGF0IGRvZXMgdGhpcyBpbXBseSBmb3Igd2hhdCB2YXJpYWJsZXMgc2hvdWxkIGJlIGluY2x1ZGVkIGFzIHByZWRpY3RvciAoaW5kZXBlbmRlbnQgdmFyaWFibGVzKSB3aGVuIHRyeWluZyB0byBpZGVudGlmeSB0aGUgdG90YWwgZWZmZWN0IG9mIE1vc3F1aXRvIE5ldCBvbiBSaXNrIG9mIE1hbGFyaWE/KioKCioqQy4gTm93IGNoYW5nZSB0aGUgYWRqdXN0bWVudCBzZXQgbm9kZXMgdG8gImFkanVzdGVkIiBub2RlcyAobGlrZSBpbiBQYXJ0IEQgb2YgUTEpLiBBZnRlciB5b3UgaGF2ZSBjb21wbGV0ZWQgdGhpcywgY29weSBhbmQgcGFzdGUgeW91ciBEQUcgUiBDb2RlIGluIHRoZSBjaHVuayBiZWxvdy4gQmUgc3VyZSB0byBjaGFuZ2UgdGhlIGNodW5rIHBhcmFtZXRlciBmcm9tIGBldmFsPUZBTFNFYCB0byBgZXZhbD1UUlVFYCB0byBlbnN1cmUgdGhlIGNvZGUgY2h1bmsgcnVucy4qKgoKYGBge3IgZXZhbD1GQUxTRX0KZyA8LSBkYWdpdHR5OjpkYWdpdHR5KCcKIyBpbnNlcnQgeW91ciBkYWdpdHR5IGNvcHkvcGFzdGUgY29kZSBoZXJlICAgICAgICAgICAgICAgICAgICAgIAonKQpnZ2RhZzo6Z2dkYWdfc3RhdHVzKGcsIHRleHQgPSBGQUxTRSwgdXNlX2xhYmVscyA9ICJuYW1lIikgKwogIGd1aWRlcyhjb2xvciA9ICJub25lIikgKyAgIyBUdXJuIG9mZiBsZWdlbmQKICB0aGVtZV9kYWcoKQpgYGAKCioqRC4gV2l0aCB5b3VyIG5ldyBEQUcsIHVzZSBgZGFnaXR0eWAncyBgYWRqdXN0bWVudF9zZXQoKWAgZnVuY3Rpb24gb24geW91ciBEQUcgdG8gaWRlbnRpZnkuIENvbmZpcm0gdGhhdCB5b3UgaGF2ZSB0aGUgc2FtZSBhZGp1c3RtZW50IHNldCBhcyB5b3UgZm91bmQgb24gZGFnaXR0eS5uZXQgZm9yIFBhcnQgQS4qKgoKYGBge3IgZXZhbD1GQUxTRSwgaW5jbHVkZT1GQUxTRX0KIyBpbnNlcnQgY29kZSBoZXJlCmBgYAoKKipFLiBSdW4gYGRhZ2l0dHlgJ3MgYGdnZGFnX2FkanVzdG1lbnRfc2V0KClgIGZ1bmN0aW9uIG9uIHRoZSBgZ2Agb2JqZWN0IHRvIGdldCBhIGJldHRlciBncmFwaCBvZiB0aGUgREFHIHdpdGggbm9kZXMuIEFkZCBpbiB0aGVzZSB0aHJlZSBwYXJhbWV0ZXJzIHRvIHlvdXIgZnVuY3Rpb246IGBzaGFkb3cgPSBUUlVFYCwgYHVzZV9sYWJlbHMgPSAibGFiZWwiYCwgYHRleHQgPSBGQUxTRWAuKioKCiMjIExlY3R1cmUgNiBFeGFtcGxlcwoKSW4gdGhpcyBsYWIgZXhhbXBsZSwgd2UnbGwgcmV2aWV3IGV4YW1wbGVzIGZyb20gW0xlY3R1cmUgNl0oaHR0cHM6Ly93d3cueW91dHViZS5jb20vd2F0Y2g/dj1OU3VUYWVXNk9yYykuCgojIyMgR3JhbmRwYXJlbnQgYW5kIFBhcmVudCBFZmZlY3Qgb24gQ2hpbGQncyBFZHVjYXRpb24KCkxldCdzIHN0YXJ0IGZpcnN0IHdpdGggdGhpcyBEQUcgZXhhbXBsZSB1c2VkIGluIExlY3R1cmUgNi4KCmBgYHtyIGVjaG89RkFMU0UsIG91dC53aWR0aCA9ICc1MCUnfQojIHJ1biB0aGlzIGNodW5rIHRvIHZpZXcgdGhlIGltYWdlCmtuaXRyOjppbmNsdWRlX2dyYXBoaWNzKCJodHRwczovL3Jhdy5naXRodWJ1c2VyY29udGVudC5jb20vd2Vzc2xlbi9kc2JhNjAxMC1zcHJpbmcyMDIyL21hc3Rlci9zdGF0aWMvaW1nL2V4YW1wbGUvMDYtY2xhc3MucG5nIikKYGBgCgpgYGB7cn0KbGlicmFyeShyZXRoaW5raW5nKQoKTiA8LSAyMDAgIyBudW0gZ3JhbmRwYXJlbnQtcGFyZW50LWNoaWxkIHRyaWFkcwpiX0dQIDwtIDEgIyBkaXJlY3QgZWZmZWN0IG9mIEcgb24gUApiX0dDIDwtIDAgIyBkaXJlY3QgZWZmZWN0IG9mIEcgb24gQwpiX1BDIDwtIDEgIyBkaXJlY3QgZWZmZWN0IG9mIFAgb24gQwpiX1UgPC0gMiAjZGlyZWN0IGVmZmVjdCBvZiBVIG9uIFAgYW5kIEMKCnNldC5zZWVkKDEpCiMgZ2VuZXJhdGl2ZSBtb2RlbApVIDwtIDIqcmJlcm4oIE4gLCAwLjUgKSAtIDEKRyA8LSBybm9ybSggTiApClAgPC0gcm5vcm0oIE4gLCBiX0dQKkcgKyBiX1UqVSApCkMgPC0gcm5vcm0oIE4gLCBiX1BDKlAgKyBiX0dDKkcgKyBiX1UqVSApCmQgPC0gZGF0YS5mcmFtZSggQz1DICwgUD1QICwgRz1HICwgVT1VICkKCm02LjExIDwtIHF1YXAoCiBhbGlzdCgKIEMgfiBkbm9ybSggbXUgLCBzaWdtYSApLAogbXUgPC0gYSArIGJfUEMqUCArIGJfR0MqRywKIGEgfiBkbm9ybSggMCAsIDEgKSwKIGMoYl9QQyxiX0dDKSB+IGRub3JtKCAwICwgMSApLAogc2lnbWEgfiBkZXhwKCAxICkKICksIGRhdGE9ZCApCgpjb2VmdGFiX3Bsb3QoY29lZnRhYihtNi4xMSksIHBhcnMgPSBjKCJiX1BDIiwiYl9HQyIpKQp0ZXh0KHggPSBiX1BDLCB5ID0gNCwgIisiLCBjb2w9InJlZCIsIGNleCA9IDIpICAjIGJfUEMKdGV4dCh4ID0gYl9HQywgeSA9IDEsICIrIiwgY29sPSJyZWQiLCBjZXggPSAyKSAgIyBiX0dDCmBgYAoKIyMjIEV4YW1wbGUgCgpMZXQncyBhc3N1bWUgdGhpcyBEQUcuCgpgYGB7ciBlY2hvPUZBTFNFLCBvdXQud2lkdGggPSAnNTAlJ30KIyBydW4gdGhpcyBjaHVuayB0byB2aWV3IHRoZSBpbWFnZQprbml0cjo6aW5jbHVkZV9ncmFwaGljcygiaHR0cHM6Ly9yYXcuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3dlc3NsZW4vZHNiYTYwMTAtc3ByaW5nMjAyMi9tYXN0ZXIvc3RhdGljL2ltZy9leGFtcGxlLzA2LWNsYXNzLTAxLnBuZyIpCmBgYAoKIyMjIEV4YW1wbGUgMQoKTGV0J3MgYXNzdW1lIHRoaXMgREFHIHdlaWdodHM6CgpgYGB7ciBlY2hvPUZBTFNFLCBvdXQud2lkdGggPSAnNTAlJ30KIyBydW4gdGhpcyBjaHVuayB0byB2aWV3IHRoZSBpbWFnZQprbml0cjo6aW5jbHVkZV9ncmFwaGljcygiaHR0cHM6Ly9yYXcuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3dlc3NsZW4vZHNiYTYwMTAtc3ByaW5nMjAyMi9tYXN0ZXIvc3RhdGljL2ltZy9leGFtcGxlLzA2LWNsYXNzLTAyLnBuZyIpCmBgYAoKYGBge3J9CmYgPC0gZnVuY3Rpb24obj0xMDAsYlhaPTEsYlpZPTEpewogIFggPC0gcm5vcm0obikKICB1IDwtIHJub3JtKG4pCiAgWiA8LSBybm9ybShuLCBiWFoqWCArIHUpCiAgWSA8LSBybm9ybShuLCBiWlkqWiArIHUpCiAgYlggPC0gY29lZiggbG0oWSB+IFgpIClbJ1gnXQogIGJYWiA8LSBjb2VmKCBsbShZIH4gWCArIFopIClbJ1gnXQogIHJldHVybiggYyhiWCxiWFopICkKfQoKc2ltIDwtIG1jcmVwbGljYXRlKCAxZTQsIGYoKSwgbWMuY29yZXM9NCkKCmRlbnMoIHNpbVsxLF0gLCBsd2Q9MywgeGxhYj0icG9zdGVyaW9yIG1lYW4iLCB4bGltPWMoLTEsMiksIHlsaW09YygwLDMpICkKZGVucyggc2ltWzIsXSAsIGx3ZD0zLCBjb2w9MiwgYWRkPVRSVUUpCnRleHQoeCA9IDAsIHkgPSAyLjUsICJ3cm9uZyAoYFkgfiBYICsgWmApIiwgY29sPTIsIGNleCA9IDEpIAp0ZXh0KHggPSAxLCB5ID0gMS45LCAiY29ycmVjdCAoYFkgfiBYYCkiLCBjb2w9MSwgY2V4ID0gMSkgCmBgYAoKCiMjIyBFeGFtcGxlIDIKCmBgYHtyIGVjaG89RkFMU0UsIG91dC53aWR0aCA9ICc1MCUnfQojIHJ1biB0aGlzIGNodW5rIHRvIHZpZXcgdGhlIGltYWdlCmtuaXRyOjppbmNsdWRlX2dyYXBoaWNzKCJodHRwczovL3Jhdy5naXRodWJ1c2VyY29udGVudC5jb20vd2Vzc2xlbi9kc2JhNjAxMC1zcHJpbmcyMDIyL21hc3Rlci9zdGF0aWMvaW1nL2V4YW1wbGUvMDYtY2xhc3MtMDMucG5nIikKYGBgCgpgYGB7cn0KZiA8LSBmdW5jdGlvbihuPTEwMCxiWFo9MSxiWlk9MSl7CiAgWCA8LSBybm9ybShuKQogIHUgPC0gcm5vcm0obikKICBaIDwtIHJub3JtKG4sIGJYWipYICsgdSkKICBZIDwtIHJub3JtKG4sIGJaWSpaICsgdSkKICBiWCA8LSBjb2VmKCBsbShZIH4gWCkgKVsnWCddCiAgYlhaIDwtIGNvZWYoIGxtKFkgfiBYICsgWikgKVsnWCddCiAgcmV0dXJuKCBjKGJYLGJYWikgKQp9CgpzaW0gPC0gbWNyZXBsaWNhdGUoIDFlNCwgZihiWlk9MCksIG1jLmNvcmVzPTQpCgpkZW5zKCBzaW1bMSxdICwgbHdkPTMsIHhsYWI9InBvc3RlcmlvciBtZWFuIiwgeGxpbT1jKC0xLDIpLCB5bGltPWMoMCwzKSApCmRlbnMoIHNpbVsyLF0gLCBsd2Q9MywgY29sPTIsIGFkZD1UUlVFKQp0ZXh0KHggPSAtLjYsIHkgPSAyLjgsICJ3cm9uZyAoYFkgfiBYICsgWmApIiwgY29sPTIsIGNleCA9IDEpIAp0ZXh0KHggPSAuNSwgeSA9IDEuOSwgImNvcnJlY3QgKGBZIH4gWGApIiwgY29sPTEsIGNleCA9IDEpIApgYGAKCiMjIyBFeGFtcGxlIDMKCmBgYHtyIGVjaG89RkFMU0UsIG91dC53aWR0aCA9ICc1MCUnfQojIHJ1biB0aGlzIGNodW5rIHRvIHZpZXcgdGhlIGltYWdlCmtuaXRyOjppbmNsdWRlX2dyYXBoaWNzKCJodHRwczovL3Jhdy5naXRodWJ1c2VyY29udGVudC5jb20vd2Vzc2xlbi9kc2JhNjAxMC1zcHJpbmcyMDIyL21hc3Rlci9zdGF0aWMvaW1nL2V4YW1wbGUvMDYtY2xhc3MtMDQucG5nIikKYGBgCgpgYGB7cn0KZiA8LSBmdW5jdGlvbihuPTEwMCxiWFk9MSxiWVo9MSkgewogWCA8LSBybm9ybShuKQogWSA8LSBybm9ybShuLCBiWFkqWCApCiBaIDwtIHJub3JtKG4sIGJZWipZICkKIGJYIDwtIGNvZWYoIGxtKFkgfiBYKSApWydYJ10KIGJYWiA8LSBjb2VmKCBsbShZIH4gWCArIFopIClbJ1gnXQogcmV0dXJuKCBjKGJYLGJYWikgKQp9CgpzaW0gPC0gbWNyZXBsaWNhdGUoIDFlNCAsIGYoKSAsIG1jLmNvcmVzPTQgKQpkZW5zKCBzaW1bMSxdICwgbHdkPTMgLCB4bGFiPSJwb3N0ZXJpb3IgbWVhbiIsIHhsaW09YygwLDEuNSksIHlsaW09YygwLDUpICApCmRlbnMoIHNpbVsyLF0gLCBsd2Q9MyAsIGNvbD0yICwgYWRkPVRSVUUgKQpgYGAKCgojIyMgRXhhbXBsZSA0CgpgYGB7ciBlY2hvPUZBTFNFLCBvdXQud2lkdGggPSAnMzAlJ30KIyBydW4gdGhpcyBjaHVuayB0byB2aWV3IHRoZSBpbWFnZQprbml0cjo6aW5jbHVkZV9ncmFwaGljcygiaHR0cHM6Ly9yYXcuZ2l0aHVidXNlcmNvbnRlbnQuY29tL3dlc3NsZW4vZHNiYTYwMTAtc3ByaW5nMjAyMi9tYXN0ZXIvc3RhdGljL2ltZy9leGFtcGxlLzA2LWNsYXNzLTA1LnBuZyIpCmBgYAoKYGBge3J9CiBmIDwtIGZ1bmN0aW9uKG49MTAwLGJaWD0xLGJYWT0xKSB7CiBaIDwtIHJub3JtKG4pCiBYIDwtIHJub3JtKG4sIGJaWCpaICkKIFkgPC0gcm5vcm0obiwgYlhZKlggKQogYlggPC0gY29lZiggbG0oWSB+IFgpIClbJ1gnXQogYlhaIDwtIGNvZWYoIGxtKFkgfiBYICsgWikgKVsnWCddCiByZXR1cm4oIGMoYlgsYlhaKSApCn0Kc2ltIDwtIG1jcmVwbGljYXRlKCAxZTQgLCBmKG49NTApICwgbWMuY29yZXM9NCApCmRlbnMoIHNpbVsxLF0gLCBsd2Q9MyAsIHhsYWI9InBvc3RlcmlvciBtZWFuIiApCmRlbnMoIHNpbVsyLF0gLCBsd2Q9MyAsIGNvbD0yICwgYWRkPVRSVUUgKQp0ZXh0KHggPSAxLjMsIHkgPSAxLjQsICJ3cm9uZyAoYFkgfiBYICsgWmApIiwgY29sPTIsIGNleCA9IDEpIAp0ZXh0KHggPSAxLjIsIHkgPSAzLCAiY29ycmVjdCAoYFkgfiBYYCkiLCBjb2w9MSwgY2V4ID0gMSkgCmBgYAoKIyMjIEV4YW1wbGUgNQoKYGBge3IgZWNobz1GQUxTRSwgb3V0LndpZHRoID0gJzMwJSd9CiMgcnVuIHRoaXMgY2h1bmsgdG8gdmlldyB0aGUgaW1hZ2UKa25pdHI6OmluY2x1ZGVfZ3JhcGhpY3MoImh0dHBzOi8vcmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbS93ZXNzbGVuL2RzYmE2MDEwLXNwcmluZzIwMjIvbWFzdGVyL3N0YXRpYy9pbWcvZXhhbXBsZS8wNi1jbGFzcy0wNi5wbmciKQpgYGAKCmBgYHtyfQpmIDwtIGZ1bmN0aW9uKG49MTAwLGJaWD0xLGJYWT0xKSB7CiBaIDwtIHJub3JtKG4pCiB1IDwtIHJub3JtKG4pCiBYIDwtIHJub3JtKG4sIGJaWCpaICsgdSApCiBZIDwtIHJub3JtKG4sIGJYWSpYICsgdSApCiBiWCA8LSBjb2VmKCBsbShZIH4gWCkgKVsnWCddCiBiWFogPC0gY29lZiggbG0oWSB+IFggKyBaKSApWydYJ10KIHJldHVybiggYyhiWCxiWFopICkKfQpzaW0gPC0gbWNyZXBsaWNhdGUoIDFlNCAsIGYoYlhZPTApICwgbWMuY29yZXM9OCApCmRlbnMoIHNpbVsxLF0gLCBsd2Q9MyAsIHhsYWI9InBvc3RlcmlvciBtZWFuIiApCmRlbnMoIHNpbVsyLF0gLCBsd2Q9MyAsIGNvbD0yICwgYWRkPVRSVUUgKQpgYGAKCgojIyBQYWNrYWdlIHZlcnNpb25zCgpgYGB7cn0Kc2Vzc2lvbkluZm8oKQpgYGA=" download="06-class.Rmd">
<button class="btn btn-danger"><i class="fa fa-save"></i> Download this code</button>
</a>

<a href="https://gitpod.io/#https://github.com/wesslen/dsba6010_examples" target="_blank"><img src="https://gitpod.io/button/open-in-gitpod.svg" style="display: block; margin: auto auto auto 0;" /></a>

## Introduction

## In-Class Practice

For in-class practice, we’re going to use [dagitty.net](https://dagitty.net). Dagitty.net was introduced in [Lecture 6](https://www.youtube.com/watch?v=NSuTaeW6Orc&t=2398s). We’re going to consider a few different examples of different DAGs and consider the implications of their structure.

Malaria is a life-threatening disease caused by parasites that are transmitted to people through the bites of infected female Anopheles mosquitoes ([WHO](https://www.who.int/news-room/fact-sheets/detail/malaria)). It is preventable and curable. In 2020, there were an estimated 241 million cases of malaria worldwide. The estimated number of malaria deaths stood at **627,000** in 2020. The WHO African Region carries a disproportionately high share of the global malaria burden. In 2020, the region was home to 95% of malaria cases and 96% of malaria deaths. Children under 5 accounted for an estimated 80% of all malaria deaths in the Region.

We’re going to look at understanding the effect of mosquito nets on the spread of Malaria.

### Prep

You will need to have these two additional R packages installed.

``` r
install.packages('dagitty')
install.packages('ggdag')
```

### Question 1

First, use [dagitty.net](https://dagitty.net) to create this initial dag.

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/assignments/04-problem-set/04-problem-set-1.png" width="50%" />

Now, do the two following steps:

1.  set Mosquito Net as the “exposure” (aka treatment or independent variable) in Dagitty.
2.  set Risk of Malaria as the “outcome” (aka response or dependent variable) in Dagitty.

Your DAG should now look like this:

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/assignments/04-problem-set/04-problem-set-2.png" width="50%" />

Answer the following four questions:

**A. Given the DAG, what type of confound is Income?**

**B. Given Income’s confound type, if the goal is to find the total effect of Mosquito Nets on the Risk of Malaria, should you include Income as a predictor (aka independent variable) into a regression with Mosquito Nets to explain the Risk of Malaria? Why or why not?**

**C. Using your Dagitty DAG, find the *minimal adjustment set* for estimating the total effect of Mosquito Net on Risk of Malaria. How does this compare to part B?**

**D. Now change the variables in your *minimal adjustment set* in Part C to “adjusted” on Dagitty. You can do this by selecting the nodes and click the “adjusted” box on the left. After you have completed this, copy and paste your DAG R Code in the chunk below. Be sure to change the chunk parameter from `eval=FALSE` to `eval=TRUE` to ensure the code chunk runs.**

``` r
g <- dagitty::dagitty('
# insert your dagitty copy/paste code here                      
')
ggdag::ggdag_status(g, text = FALSE, use_labels = "name") +
  guides(color = "none") +  # Turn off legend
  theme_dag()
```

This is what your DAG above should look like:

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/assignments/04-problem-set/04-problem-set-3.png" width="50%" />

### Question 2

Let’s now add in more complexity to our DAG.

First, go back and **remove “Income” as an “Adjusted” check box.**

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/assignments/04-problem-set/04-problem-set-4.png" width="75%" />

Now, add in additional nodes in your DAG to get the dag above.

Note that in this DAG on “Mosquito Net” (exposure) and “Risk of Malaria” (outcome) have been assigned (i.e., no other nodes are selected as “adjusted”).

Answer the following four questions:

**A. Given the DAG, what is the minimal sufficient adjustment sets for estimating the total effect of Mosquito Net on Risk of Malaria?**

**B. What does this imply for what variables should be included as predictor (independent variables) when trying to identify the total effect of Mosquito Net on Risk of Malaria?**

**C. Now change the adjustment set nodes to “adjusted” nodes (like in Part D of Q1). After you have completed this, copy and paste your DAG R Code in the chunk below. Be sure to change the chunk parameter from `eval=FALSE` to `eval=TRUE` to ensure the code chunk runs.**

``` r
g <- dagitty::dagitty('
# insert your dagitty copy/paste code here                      
')
ggdag::ggdag_status(g, text = FALSE, use_labels = "name") +
  guides(color = "none") +  # Turn off legend
  theme_dag()
```

**D. With your new DAG, use `dagitty`’s `adjustment_set()` function on your DAG to identify. Confirm that you have the same adjustment set as you found on dagitty.net for Part A.**

**E. Run `dagitty`’s `ggdag_adjustment_set()` function on the `g` object to get a better graph of the DAG with nodes. Add in these three parameters to your function: `shadow = TRUE`, `use_labels = "label"`, `text = FALSE`.**

## Lecture 6 Examples

In this lab example, we’ll review examples from [Lecture 6](https://www.youtube.com/watch?v=NSuTaeW6Orc).

### Grandparent and Parent Effect on Child’s Education

Let’s start first with this DAG example used in Lecture 6.

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/example/06-class.png" width="50%" />

``` r
library(rethinking)

N <- 200 # num grandparent-parent-child triads
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect of G on C
b_PC <- 1 # direct effect of P on C
b_U <- 2 #direct effect of U on P and C

set.seed(1)
# generative model
U <- 2*rbern( N , 0.5 ) - 1
G <- rnorm( N )
P <- rnorm( N , b_GP*G + b_U*U )
C <- rnorm( N , b_PC*P + b_GC*G + b_U*U )
d <- data.frame( C=C , P=P , G=G , U=U )

m6.11 <- quap(
 alist(
 C ~ dnorm( mu , sigma ),
 mu <- a + b_PC*P + b_GC*G,
 a ~ dnorm( 0 , 1 ),
 c(b_PC,b_GC) ~ dnorm( 0 , 1 ),
 sigma ~ dexp( 1 )
 ), data=d )

coeftab_plot(coeftab(m6.11), pars = c("b_PC","b_GC"))
text(x = b_PC, y = 4, "+", col="red", cex = 2)  # b_PC
text(x = b_GC, y = 1, "+", col="red", cex = 2)  # b_GC
```

<img src="/lab/06-class_files/figure-html/unnamed-chunk-12-1.png" width="672" />

### Example

Let’s assume this DAG.

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/example/06-class-01.png" width="50%" />

### Example 1

Let’s assume this DAG weights:

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/example/06-class-02.png" width="50%" />

``` r
f <- function(n=100,bXZ=1,bZY=1){
  X <- rnorm(n)
  u <- rnorm(n)
  Z <- rnorm(n, bXZ*X + u)
  Y <- rnorm(n, bZY*Z + u)
  bX <- coef( lm(Y ~ X) )['X']
  bXZ <- coef( lm(Y ~ X + Z) )['X']
  return( c(bX,bXZ) )
}

sim <- mcreplicate( 1e4, f(), mc.cores=4)
```

``` r
dens( sim[1,] , lwd=3, xlab="posterior mean", xlim=c(-1,2), ylim=c(0,3) )
dens( sim[2,] , lwd=3, col=2, add=TRUE)
text(x = 0, y = 2.5, "wrong (`Y ~ X + Z`)", col=2, cex = 1) 
text(x = 1, y = 1.9, "correct (`Y ~ X`)", col=1, cex = 1) 
```

<img src="/lab/06-class_files/figure-html/unnamed-chunk-15-1.png" width="672" />

### Example 2

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/example/06-class-03.png" width="50%" />

``` r
f <- function(n=100,bXZ=1,bZY=1){
  X <- rnorm(n)
  u <- rnorm(n)
  Z <- rnorm(n, bXZ*X + u)
  Y <- rnorm(n, bZY*Z + u)
  bX <- coef( lm(Y ~ X) )['X']
  bXZ <- coef( lm(Y ~ X + Z) )['X']
  return( c(bX,bXZ) )
}

sim <- mcreplicate( 1e4, f(bZY=0), mc.cores=4)
```

``` r
dens( sim[1,] , lwd=3, xlab="posterior mean", xlim=c(-1,2), ylim=c(0,3) )
dens( sim[2,] , lwd=3, col=2, add=TRUE)
text(x = -.6, y = 2.8, "wrong (`Y ~ X + Z`)", col=2, cex = 1) 
text(x = .5, y = 1.9, "correct (`Y ~ X`)", col=1, cex = 1) 
```

<img src="/lab/06-class_files/figure-html/unnamed-chunk-17-1.png" width="672" />

### Example 3

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/example/06-class-04.png" width="50%" />

``` r
f <- function(n=100,bXY=1,bYZ=1) {
 X <- rnorm(n)
 Y <- rnorm(n, bXY*X )
 Z <- rnorm(n, bYZ*Y )
 bX <- coef( lm(Y ~ X) )['X']
 bXZ <- coef( lm(Y ~ X + Z) )['X']
 return( c(bX,bXZ) )
}

sim <- mcreplicate( 1e4 , f() , mc.cores=4 )
```

``` r
dens( sim[1,] , lwd=3 , xlab="posterior mean", xlim=c(0,1.5), ylim=c(0,5)  )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )
text(x = 0.2, y = 2.8, "wrong (`Y ~ X + Z`)", col=2, cex = 1) 
text(x = 1.32, y = 1.9, "correct (`Y ~ X`)", col=1, cex = 1) 
```

<img src="/lab/06-class_files/figure-html/unnamed-chunk-19-1.png" width="672" />

### Example 4

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/example/06-class-05.png" width="30%" />

``` r
 f <- function(n=100,bZX=1,bXY=1) {
 Z <- rnorm(n)
 X <- rnorm(n, bZX*Z )
 Y <- rnorm(n, bXY*X )
 bX <- coef( lm(Y ~ X) )['X']
 bXZ <- coef( lm(Y ~ X + Z) )['X']
 return( c(bX,bXZ) )
}
sim <- mcreplicate( 1e4 , f(n=50) , mc.cores=4 )
```

``` r
dens( sim[1,] , lwd=3 , xlab="posterior mean" )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )
text(x = 1.3, y = 1.4, "wrong (`Y ~ X + Z`)", col=2, cex = 1) 
text(x = 1.2, y = 3, "correct (`Y ~ X`)", col=1, cex = 1) 
```

<img src="/lab/06-class_files/figure-html/unnamed-chunk-21-1.png" width="672" />

### Example 5

<img src="https://raw.githubusercontent.com/wesslen/dsba6010-spring2022/master/static/img/example/06-class-06.png" width="30%" />

``` r
f <- function(n=100,bZX=1,bXY=1) {
 Z <- rnorm(n)
 u <- rnorm(n)
 X <- rnorm(n, bZX*Z + u )
 Y <- rnorm(n, bXY*X + u )
 bX <- coef( lm(Y ~ X) )['X']
 bXZ <- coef( lm(Y ~ X + Z) )['X']
 return( c(bX,bXZ) )
}
sim <- mcreplicate( 1e4 , f(bXY=0) , mc.cores=8 )
```

``` r
dens( sim[1,] , lwd=3 , xlab="posterior mean" )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )
text(x = 0.5, y = 4.9, "Y ~ X + Z More Biased", col=2, cex = 1) 
text(x = 0.13, y = 1.5, "Y ~ X Biased", col=1, cex = 1) 
```

<img src="/lab/06-class_files/figure-html/unnamed-chunk-23-1.png" width="672" />

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
## [1] rethinking_2.21      cmdstanr_0.4.0.9001  rstan_2.21.3        
## [4] ggplot2_3.3.5        StanHeaders_2.21.0-7
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
## [49] stringr_1.4.0        munsell_0.5.0        callr_3.7.0         
## [52] compiler_4.1.1       jquerylib_0.1.4      rlang_0.4.12        
## [55] grid_4.1.1           rstudioapi_0.13      base64enc_0.1-3     
## [58] rmarkdown_2.11       xaringanExtra_0.5.5  gtable_0.3.0        
## [61] codetools_0.2-18     inline_0.3.19        abind_1.4-5         
## [64] DBI_1.1.1            R6_2.5.1             gridExtra_2.3       
## [67] lubridate_1.8.0      knitr_1.36           dplyr_1.0.7         
## [70] fastmap_1.1.0        utf8_1.2.2           downloadthis_0.2.1  
## [73] bsplus_0.1.3         shape_1.4.6          stringi_1.7.6       
## [76] Rcpp_1.0.7           vctrs_0.3.8          tidyselect_1.1.1    
## [79] xfun_0.28            coda_0.19-4
```
