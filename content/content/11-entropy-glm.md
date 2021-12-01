---
date: "2021-12-01"
class_date: "2022-02-14"
menu:
  content:
    parent: Course content
    weight: 12
highlight: true
tags:
- Computation
title: Maximium entropy & GLMs
type: docs
weight: 12
---


{{< icon name="book" pack="fas" >}} Chapter 10: Big Entropy and the Generalized Linear Model

<!--more-->

## Lecture

{{< youtube -4y4X8ELcEM >}}

## Slides

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/49d056ed5f2449459654d8b3b1e273fe" title="L11 Statistical Rethinking Winter 2019" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 420px;" data-ratio="1.3333333333333333"></iframe>

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
