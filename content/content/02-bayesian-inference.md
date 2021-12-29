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

{{< icon name="book" pack="fas" >}} [Chapter 1: The Golem of Prague Data](http://xcelab.net/rmpubs/sr2/statisticalrethinking2_chapters1and2.pdf)

{{< icon name="book" pack="fas" >}} [Chapter 2: The Garden of Forking Data](http://xcelab.net/rmpubs/sr2/statisticalrethinking2_chapters1and2.pdf)

{{< icon name="book" pack="fas" >}} Chapter 3: Sampling the Imaginary

<!--more-->

## Lectures

### Lecture 1

{{< youtube 4WVelCswXo4 >}}

<br>

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/c63892105b90479bb8827c9afcf5ad76" title="L01 Statistical Rethinking Winter 2019" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 420px;" data-ratio="1.3333333333333333"></iframe>

<br>

### Lecture 2

{{< youtube XoVtOAN0htU >}}

<br>

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/3bfa335d71ac499ebb10e11073efe777" title="L02 Statistical Rethinking Winter 2019" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 560px; height: 210px;" data-ratio="1.3333333333333333"></iframe>


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

- **Models are not hypotheses**; they are neither true or false. Models are "golems" that do as they are told.

- Ideally should **compare performance across models** (model comparison).

- Popper: **test (attempt to falsify) research hypothesis**. Use theory, make a falsifiable prediction, and test that; not that nothing happened (aka null hypothesis).

{{% /spoiler %}}


{{% spoiler text="What are three ways in which cross-validation and information theory aid in model evaluation?" %}}

1. They provide **useful expectations of predictive accuracy**, rather than merely fit to sample. So they **compare models** where it matters. 

2. They give us an **estimate of the tendency of a model** to overfit. This will help us to understand how models and data interact, which in turn helps us to design better models.

3. They help us to spot **highly influential observations**.

- We will cover these topics in Lesson 8.

{{% /spoiler %}}

{{% spoiler text="What are the benefits of using multilevel (aka hierarchical) models?" %}}

- They acknowledge that, though each individual group might have its own model, one group can provide valuable information about another. That is, **"let’s learn from one another while celebrating our individuality."**

- We will cover these later in the course.

{{% /spoiler %}}

## Feedback

{{< feedback >}}
