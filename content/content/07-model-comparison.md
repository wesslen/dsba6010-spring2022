---
date: "2021-12-01"
class_date: "2022-02-28"
menu:
  content:
    parent: Course content
    weight: 7
highlight: true
tags:
- Causal inference
title: Class 7 - Model Comparison
type: docs
weight: 7
---

## Zoom

<a href="https://uncc.zoom.us/j/93339403054"><i class="fas fa-video fa-lg"></i></a>: 12:00pm - 2:45pm, February 28, 2022

## Required Readings

{{< icon name="book" pack="fas" >}} Chapter 7: Ulysses' Compass

{{< icon name="book" pack="fas" >}} Chapter 8: Conditional Manatees


<!--more-->

## Lecture

### Lecture 7

{{< youtube odGAAJDlgp8 >}}

<br>

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/9f57deff04ac470d95ff9c8b34abb8f6" title="Statistical Rethinking 2022 Lecture 07" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 314px;" data-ratio="1.78343949044586"></iframe>
<br>

### Lecture 8

{{< youtube Qqz5AJjyugM >}}

<br>

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/cc59c28f5c974674a0745eb48d54d693" title="Statistical Rethinking 2022 Lecture 08" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 314px;" data-ratio="1.78343949044586"></iframe>


## Comprehension questions

{{% spoiler text="What are three ways in which cross-validation and information theory aid in model evaluation?" %}}

1. They provide **useful expectations of predictive accuracy**, rather than merely fit to sample. So they **compare models** where it matters. 

2. They give us an **estimate of the tendency of a model** to overfit. This will help us to understand how models and data interact, which in turn helps us to design better models.

3. They help us to spot **highly influential observations**.

{{% /spoiler %}}

{{% spoiler text="Compare and contrast the four ways to calculate posteriors covered in this class." %}}

1. Analytical approach: mathematical approach that relies closed form (aka pure math) solutions that are accurate but cover only limiting circumstances (e.g., memorizing [conjugate distributions](https://en.wikipedia.org/wiki/Conjugate_prior#Table_of_conjugate_distributions)).

2. Grid approximation: very limited approach that relies on brute force counting. This approach is helpful for simple models (e.g., single variable) but becomes too computationally complex with multiple variables.

3. Quadratic approximation: Laplace's approximation that relies on a Gaussian (normal) distribution assumption. This is fast and works well with simple to moderate models. This approach begins having issues with more complex models (e.g., multilevel)

4. Markov Chain Monte Carlo: A family of approaches (e.g., Metropolis, Hamiltonian) that relies on drawing samples from posterior distribution. Depending on the version, it scales well to many dimensions and has beneficial mathematical guarantees (e.g., with many draws long run estimate in proportion to population size). It is used in Stan and other modern PPL's.

{{% /spoiler %}}

## Deliverables

Due before class: Monday, February 28 at 11:59am 

<a href="https://forms.gle/zMipNzav3BCL3Rwy9"><i class="fas fa-comment fa-lg"></i>  Class Feedback</a>

<a href="https://uncc.instructure.com/courses/171000/quizzes/331405"><i class="fas fa-question fa-lg"></i>  Lesson Quiz</a>

## Lab for Class 7

[Lab class 7 code](../../example/07-class)

## Problem Set 5

Due by next class: Monday, March 14 at 11:59am

<a href="https://dsba6010-spring2022.netlify.app/assignment/05-problem-set"><i class="fas fa-pencil-ruler fa-lg"></i>  Problem Set</a> / [Canvas Link](https://uncc.instructure.com/courses/171000/assignments/1415462)
