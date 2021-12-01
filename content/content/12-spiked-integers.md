---
date: "2021-12-01"
class_date: "2022-02-14"
menu:
  content:
    parent: Course content
    weight: 13
highlight: true
tags:
- Computation
title: Maximium entropy & GLMs
type: docs
weight: 13
---


{{< icon name="book" pack="fas" >}} Chapter 11: God Spiked the Integers

<!--more-->

## Lecture

{{< youtube hRJtKCIDTwc >}}

## Slides

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/8204715ef3c445daa83dd2653c4b2559" title="L12 Statistical Rethinking Winter 2019" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 420px;" data-ratio="1.3333333333333333"></iframe>

## Quiz

{{< spoiler text="When is a heatmap useful?" >}}
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
{{< /spoiler >}}

{{< spoiler text="Write Plotly code to render a bar chart" >}}
```python
import plotly.express as px
data_canada = px.data.gapminder().query("country == 'Canada'")
fig = px.bar(data_canada, x='year', y='pop')
fig.show()
```
{{< /spoiler >}}
