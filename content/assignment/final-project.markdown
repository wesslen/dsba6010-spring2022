---
title: Final Project
date: "2022-03-14"
menu:
  assignment:
    parent: Project
    weight: 2
type: docs
toc: true
---

## Overview

There are two options for your final project.

### Option 1

Use an existing notebook and customize it for your project. **This is the recommended option for most students.**

If you choose this option, you will be expected in your proposal to make significant changes in the notebook including (but not limited to):

* modification of a different (but ideally related) data set

* additional analysis tasks

* modification to fit Bayesian workflow as outlined below

### Option 2

Create a custom (from scratch) Bayesian data analysis.

- This is a more challenging option that provides more flexibility for more creative project ideas.

- Students choosing this can combine ideas from other notebooks/sources. 
  
## Submission Documents

Students will be expected to [submit onto canvas](https://uncc.instructure.com/courses/171000/assignments/1415491):

1. A reproducible notebook (e.g., RMarkdown, Jupyter, etc.) of their data analysis. Students are able to break the project into multiple notebooks for organization purposes. Ideally, this could also be a zipped folder that includes the entire contents of the project (e.g., data + code).

2. Present on the final day of class a 7 minute presentation + 2-3 min question and answers. Presentation can either be your notebook or a new presentation.

Students may use additional communication packages like `shiny`, `streamlit`, `quarto`, and `Rmarkdown`. Students that use those tools successfully will receive very high grades although they are not required. 

I **highly recommend** students that this opportunity to showcase their skills by using their notebook as a blog post that would be instrumental for developing their portfolio as a data scientist/engineer.

## Grading

I've revised the syllabus to now have two additional check-ins as well as dedicated time in class towards working to projects. Final projects makes up 330 points towards your final grade.

|Dimensions          | Points|
|:-------------------|------:|
|Check-ins (4 at 20 points)           |     80|
|Final project + Presentation     |    250|
|Total Points for grade    |    330|

## Project Check-ins

### Project Check in 1

This was already completed and graded.

### Project Check in 2

Due Date: 11:59AM on Monday, March 24

Due next class, you will need to complete this [Google Form](https://forms.gle/9REg9gK38QDu8k738) outlining details of your project. This is worth 20 points.

In addition, next class you will work in small groups (2-3 students) and share your current version of your notebook. For teammates on Zoom, you'll be broken into small groups.

### Project Check in 3

Due Date: 11:59AM on Monday, March 24

[Project Check In 3 Canvas Link](https://uncc.instructure.com/courses/171000/assignments/1503271)

You will be expected to provide your first draft of your project notebook. 

You will need to submit this to Canvas. Instead of submitting to canvas, github repos and Colab notebooks are also acceptable.

Some feedback may be provided but overall this is more of a check for students to ensure they are holding to deadlines and working on their project.

### Project Check in 4

Due Date: 11:59AM on Monday, April 25

[Project Check In 4 Canvas Link](https://uncc.instructure.com/courses/171000/assignments/1503272)

You will be expected to provide a second draft of your project notebook. 

Instructor feedback will be minimal as this submission will occur late in the semester. However, we'll spend the rest of the final class to work in class in the same groups and classmate feedback is anticipated and welcomed.

## Grading Guidelines

You will be graded on six dimensions. Final projects are worth 250 points.

|Dimensions          | Points|
|:-------------------|------:|
|Creativity          |     40|
|Number of Tasks     |     50|
|Depth / Correctness |     40|
|Reproducibility     |     40|
|Communication       |     40|
|Presentation        |     40|
|--------------------|-------|
|Total Points        |    250|

### 1. Creativity: 40 points 

* **Excellent (21-25 points):** Project is a creative and important issue that may have many applications beyond what was explored. Student provided great background on the motivation of the problem.

* **Good (16-20 points):** Project was an acceptable objective but questions still remain. Likely student tried to analyze too large of a dataset or too broad of a topic.

* **Unsatisfactory (<= 15 points):** The project's objective was not clear or completely identical to a past notebook (e.g., exact same data was changed).

### 2. Number of analysis tasks: 50 points

For this project, we'll follow the workflow outlined by [Bayesian Workflow](https://arxiv.org/abs/2011.01808) paper. 

![](/img/assignments/bayesian-workflow.JPG)

* Students **must** complete at least the five starred analysis tasks.

* **Students receive up to 4 points for every data analysis task** they complete. Examples of these include fake data (generative) simulation, cross-validation, etc.

* Each task should include at least 1 paragraph (3-5 sentences) with appropriate figures to support the task. Incomplete or insufficient tasks will not receive the full 4 points credit.

* I expect that multiple models may exist -- that is, you're allowed to repeat cycle for 2nd or even 3rd iteration of a different model depending on the performance of your model.

### 3. Depth/correctness of analysis tasks: 40 points

* **Excellent (34-40 points):** Student has concise and correct code. It is well defined, runs in sufficient time. Models that may require long time are saved and can be rerun quickly.

* **Good (26-33 points):** Student code is sufficient but may have some problems. Code may also work but not be the most concise way of running (e.g., runs a bit longer than could be if specified differently)

* **Unsatisfactory (< 25 points):** Code has multiple bugs or structural issues (e.g., fails to correctly specify the model, incorrect specification or setup)

### 4. Reproducibility: 40 points

* **Excellent (34-40 points):** Submission includes full reproducibility with ease. This could include GitHub, virtual environments (e.g., package versions), Colab, etc.

* **Good (26-33 points):** Student provided code + data but code does not fully reproduce (e.g., absolute reference links).

* **Unsatisfactory (< 25 points):** Student provided notebook code but cannot be run (e.g., don't call in libraries/functions).  Notebook is hard to follow and many questions remain open on how to run correctly.

A general rule of thumb will be how quickly/easy it is for someone completely new to Bayesian statistics to run your analysis on a different computer to run your code.

### 5. Communication clarity: 40 points

* **Excellent (34-40 points):** Project is well written, organized, appropriate figures, and reads like a polished article/blog post. Excellent projects consider communication tools/packages we haven't used in class like `rmarkdown`, `quarto`,`shiny` or `streamlit`.

* **Good (26-33 points):** Notebook may provide some descriptions (e.g., as comments) but not written in full sentences. Figures/tables are acceptable but may have room for improvement. Discussions may hit on key points but may have some misinterpretations. 

* **Unsatisfactory (< 25 points):** Notebook provides little to no description. The organizational workflow is unclear. Figures/tables are unnecessary or poorly designed. 

### 6. Final presentation: 40 points

* **Excellent (34-40 points):** Presentation is well organized, organized, well rehearsed, clear and concise. Excellent presentations cover the project in an adequate amount of time with the right coverage of details.

* **Good (26-33 points):** Presentation covers main points of project may have some shortcomings. Some figures/tables may not be well designed and additional presentation preparation could have helped the presentation.  

* **Unsatisfactory (< 25 points):** Presentation misses several project details or lacks clarity on the steps/procedures used. The presentation's organization is unclear and includes material that are unnecessary or poorly designed. 
