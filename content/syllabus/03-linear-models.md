---
date: "2022-02-06"
highlight: true
tags:
- Bayesian methods
title: Linear Models
type: book
weight: 3
---

{{< icon name="book" pack="fas" >}} Chapter 4: Geocentric models

<!--more-->

## Lecture

{{< youtube h5aPo5wXN8E >}}

## Slides

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/c81fda5d72c54127935b83201d31c2c0" title="L03 Statistical Rethinking Winter 2019" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 420px;" data-ratio="1.3333333333333333"></iframe>

## Comprehension questions

{{% spoiler text="When is a heatmap useful?" >}}

{{% /spoiler %}}

{{% spoiler text="Write R code to " %}}
```r
import plotly.express as px
data_canada = px.data.gapminder().query("country == 'Canada'")
fig = px.bar(data_canada, x='year', y='pop')
fig.show()
```
{{% /spoiler %}}
