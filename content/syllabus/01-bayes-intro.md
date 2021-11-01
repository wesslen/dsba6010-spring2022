---
date: "2022-01-10"
title: Intro to Bayesian Methods
type: book
tags:
- Bayesian methods
weight: 1
---

{{< icon name="book" pack="fas" >}} Chapter 1: The Golem of Prague

<!--more-->

## Lecture

{{< youtube 4WVelCswXo4 >}}

## Slides

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/c63892105b90479bb8827c9afcf5ad76" title="L01 Statistical Rethinking Winter 2019" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 420px;" data-ratio="1.3333333333333333"></iframe>

## Comprehension questions

{{< spoiler text="What is Bayesian data analysis? How does it differ in its definition of uncertainty from Frequentist interpretations?" >}}
An approach to count all the ways data can happen according to assumptions.

Bayesian data analysis uses probabilities to describe uncertainty. Importantly, in Bayesian data analysis probabilities describe degrees of belief. In contrast, frequentist interpretat of probabilities as the frequencies of events in very large samples.

This leads to frequentist uncertainty being premised on imaginary resampling of data—if we were to repeat the measurement many many times, we would end up collecting a list of values that will have some pattern to it. It means also that parameters and models cannot have probability distributions, only measurements can. 
{{< /spoiler >}}

{{< spoiler text="What are three ways in which cross-validation and information theory aid in model evaluation?" >}}

1. They provide useful expectations of predictive accuracy, rather than merely fit to sample. So they compare models where it matters. 

2. They give us an estimate of the tendency of a model to overfit. This will help us to understand how models and data interact, which in turn helps us to design better models.

3. They help us to spot highly influential observations.

{{< /spoiler >}}

{{< spoiler text="What are the benefits of using multilevel (aka hierarchical) models?" >}}

They acknowledge that, though each individual group might have its own model, one group can provide valuable information about another. That is, "let’s learn from one another while celebrating our individuality."

{{< /spoiler >}}
