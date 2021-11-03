---
date: "2022-01-10"
linkTitle: Syllabus
summary: Bayesian statistics and causal inference
title: "\U0001F4CA Syllabus"
type: book
---

{{< figure src="featured.png" >}}

{{< toc hide_on="xl" >}}

## What you will learn

- Fundamental {{<hl>}}R programming skills{{</hl>}}
- {{<hl>}}Statistical concepts{{</hl>}} and how to apply them in practice
- Gain experience with {{<hl>}}`stan`{{</hl>}}, including data visualization with {{<hl>}}`tidybayes`{{</hl>}} and data wrangling with {{<hl>}}`tidyverse`{{</hl>}}

## Course overview

This course builds your knowledge of and confidence in making inferences from data. Reflecting the need for scripting in today's model-based statistics, students will perform step-by-step calculations that are usually automated. This unique computational approach ensures that sufficient understanding to make reasonable choices and interpretations in your own modeling work.

We'll cover causal inference and generalized linear multilevel models from a simple Bayesian perspective that builds on information theory and maximum entropy. The core material ranges from the basics of regression to advanced multilevel models. If time, we'll discuss measurement error, missing data, and Gaussian process models for spatial and phylogenetic confounding.

The course also includes directed acyclic graph (DAG) approach to causal inference. Additional topics may include the design of prior distributions, splines, ordered categorical predictors, social relations models, cross-validation, importance sampling, instrumental variables, and Hamiltonian Monte Carlo. Our goal is to go beyond generalized linear modeling, showing how domain-specific scientific models can be built into statistical analyses.

Application of the models will focus on research in cognitive science, human-computer interaction, computational social science, and information visualization. 

## Topics

{{< list_children >}}

## FAQs

{{% spoiler text="What are the course prerequisites?" %}}

**Programming**: Experience with R, ideally tidyverse, are necessary. Students without should immediately consider DataCamp courses and hands-on practice problems. 

**Probability**: Fundamental probability theory is highly recommended. This includes exposure to common distributions like Normal (Gaussian), Binomial, and Beta distributions. 

We'll also assume core understanding of statistical models like linear regression.
{{% /spoiler %}}

{{% spoiler text="What programming languages will be used?" %}}
- Lectures and coursework will require R. Students must complete assignments in R and mid term will cover basics of R. 

- Final projects can be done in Python (e.g., pyMC3) or Julia (turing.jl); however, it is the student's responsibility to be fluent in setting up related environments/packages (e.g., Jupyter, virtual environments, etc.) if they want to use Python or Julia.
{{% /spoiler %}}

{{< cta cta_text="Begin the course" cta_link="bayes-intro" >}}
