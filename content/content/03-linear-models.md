---
date: "2021-12-01"
class_date: "2022-01-31"
menu:
  content:
    parent: Course content
    weight: 3
highlight: true
tags:
- Bayesian methods
title: Class 3 - Basic Regression
type: docs
weight: 3
---

{{< tweet 1454033843095224324>}}

## Zoom

<a href="https://uncc.zoom.us/j/93339403054"><i class="fas fa-video fa-lg"></i></a>: 12:00pm - 2:45pm, January 31, 2022

## Required Readings

{{< icon name="book" pack="fas" >}} Chapter 4: Geocentric models (Sections 4.1-4.4)

<!--more-->

## Lecture

### Lecture 3

{{< youtube zYYBtxHWE0A >}}

## Slides

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/f3ab6dba78c24f74a843e0b2701a457d" title="Statistical Rethinking 2022 Lecture 03" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 314px;" data-ratio="1.78343949044586"></iframe>

## Comprehension questions

{{% spoiler text="How is a linear regression similar to a Geocentric modeling approach?" %}}

- Linear regression are essentially Geocentric models. They are descriptively accurate, mechanistically wrong, and a general method of approximation.

{{% /spoiler %}}

{{% spoiler text="Why are Normal (Gaussian) distributions common?" %}}

- The generative argument implies that summed fluctuations tend towards normal distributions.

- The statistical argument implies that estimate mean and variance (which what linear regressions are doing), the normal distribution is the least informative distribution. This is the maximum entropy argument that we'll cover later in the course when we consider Information Theory.

- The main takeaway is **the variable does not have to be normally distributed for the normal model to be useful**.

{{% /spoiler %}}

{{% spoiler text="Why is rescaling (standardization) important for modeling?" %}}

- Rescaling has multiple benefits. First, it enables comparison across different variables that have different scales (e.g., weight in pounds and height in centimeters).

- Second, it enables simpler priors based on standardized distributions (e.g., Normal(0, 1) or Normal(0,5)).

- Third, later in the course we'll see where scaling makes computationally intensive (MCMC) easier; or said differently,  MCMC algorithms may run slower on non-scaled data.

- Rescaling also makes the intercept (alpha) means the expected dependent variable values, (e.g., in weight/height it means the expected adult weight (the DV)).

{{% /spoiler %}}

## Deliverables

Due before class: Monday, January 31 at 11:59am 

<a href="https://forms.gle/zMipNzav3BCL3Rwy9"><i class="fas fa-comment fa-lg"></i>  Class Feedback</a>

<a href="https://uncc.instructure.com/courses/171000/quizzes/331402"><i class="fas fa-question fa-lg"></i>  Lesson Quiz</a>

## Example 3

[Example 3 code](../../example/03-class)

## Problem Set 2

Due by next class: Monday, February 7 at 11:59am 

<a href="{{ .Site.baseurl }}/assignment/02-problem-set"><i class="fas fa-pencil-ruler fa-lg"></i>  Problem Set</a> / [Canvas Link](https://uncc.instructure.com/courses/171000/assignments/1415433)
