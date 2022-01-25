---
date: "2021-12-01"
class_date: "2022-01-24"
menu:
  content:
    parent: Course content
    weight: 2
highlight: true
tags:
- Bayesian methods
title: Class 2 - Bayesian Inference
type: docs
weight: 2
---

![](https://pbs.twimg.com/media/E7luuH_VIAM8yzn?format=png&name=small)<center>[@ChelseaParlett](https://twitter.com/ChelseaParlett/status/1421291716229746689)</center>

## Zoom

<a href="https://uncc.zoom.us/j/93339403054"><i class="fas fa-video fa-lg"></i> Zoom</a>: 12:00pm - 2:45pm, January 24, 2022

## Required Readings

{{< icon name="book" pack="fas" >}} [Chapter 1: The Golem of Prague Data](http://xcelab.net/rmpubs/sr2/statisticalrethinking2_chapters1and2.pdf)

{{< icon name="book" pack="fas" >}} [Chapter 2: The Garden of Forking Data](http://xcelab.net/rmpubs/sr2/statisticalrethinking2_chapters1and2.pdf)

{{< icon name="book" pack="fas" >}} Chapter 3: Sampling the Imaginary

<!--more-->

## Lectures

### Lecture 1

{{< youtube cclUd_HoRlo >}}

<br>

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/075b46c0223d495d89692fc22077b394" title="Statistical Rethinking 2022 Lecture 01" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 314px;" data-ratio="1.78343949044586"></iframe>

<br>

### Lecture 2

{{< youtube guTdrfycW2Q >}}

<br>

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/ecbbe6bc6df1496aa5b71e30c66b6bb7" title="Statistical Rethinking 2022 Lecture 02" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 314px;" data-ratio="1.78343949044586"></iframe>

## Comprehension questions

{{% spoiler text="What is Bayesian data analysis? How does it differ in its definition of uncertainty from Frequentist interpretations?" %}}
- An approach to **count all the ways data can happen** according to assumptions.

- Bayesian data analysis uses **probabilities** to describe uncertainty. Importantly, in Bayesian data analysis probabilities describe **degrees of belief**. In contrast, a frequentist interpretation of probabilities would be as the **frequencies of events in very large samples**.

- This leads to frequentist uncertainty being premised on **imaginary resampling of data**—if we were to repeat the measurement many many times, we would end up collecting a list of values that will have some pattern to it. It means also that parameters and models **cannot** have probability distributions, only measurements can. 
{{% /spoiler %}}

{{% spoiler text="Classical (Frequentist) statistical tests were originally developed for what purposes?" %}}

- They were originally developed (largely by Ronald Fisher) in the early 20th century for agricultural applications. They typically were for randomized experiments with large effects, in which **measurement issues had been solved**.

- Such statistical tests produce inferences, **not decisions**.

{{% /spoiler %}}


{{% spoiler text="What is the core problem with null hypothesis testing?" %}}

- Null hypotheses are **not** unique. Hypotheses do not imply unique models, and models do not imply unique hypotheses.

- **Models are not hypotheses**; they are neither true or false. Models are "golems" that do as they are told. Ideally should **compare performance across models** (model comparison).

- Popper: **test (attempt to falsify) research hypothesis**. Use theory, make a falsifiable prediction, and test that; not that nothing happened (aka null hypothesis).

{{% /spoiler %}}


## Deliverables

Due before class: Monday, January 24 at 11:59am 

<a href="https://forms.gle/zMipNzav3BCL3Rwy9"><i class="fas fa-comment fa-lg"></i>  Class Feedback</a>

<a href="https://uncc.instructure.com/courses/171000/quizzes/331407"><i class="fas fa-question fa-lg"></i>  Lesson Quiz</a>

## Project Check-in 1

Presentations on February 14, [Project Check In 1](../../assignment/01-project)

Due by next class (Jan 31): Email or DM Ryan the article you will be presenting on.

## Example 2

[Example 2 code](../../example/02-class)

## Problem Set 1

Due by next class: Monday, January 31 at 11:59am

<a href="{{ .Site.baseurl }}/assignment/01-problem-set"><i class="fas fa-pencil-ruler fa-lg"></i>  Problem Set</a> / [Canvas Link](https://uncc.instructure.com/courses/171000/assignments/1415432)