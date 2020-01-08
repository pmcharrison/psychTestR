---
title: 'psychTestR: An R package for designing and conducting psychological experiments'
tags:
  - R
  - psychology
  - experiment design
authors:
  - name: Peter M. C. Harrison
  orcid: 0000-0002-9851-9462
  affiliation: "1, 2" # (Multiple affiliations must be quoted)
affiliations:
  - name: Max Planck Institute for Empirical Aesthetics, Frankfurt, Germany
  index: 1
  - name: Queen Mary University of London, UK
  index: 2
date: 31 January 2020
bibliography: paper.bib
---

# Overview 

The contemporary psychologist has access to many different software packages
for developing and administering psychological experiments.
Each such package is best-suited to particular applications.
For example,
if the experiment is to take the form of a simple questionnaire,
straightforward tools such as [Survey Monkey](https://www.surveymonkey.co.uk/),
[Google Forms](https://www.google.co.uk/forms/about/),
or [Qualtrics](https://www.qualtrics.com/)
could be most appropriate.
If the experiment is to depend on specialist hardware
and highly accurate response time measurement,
then one might instead adopt the Python package
[PsychoPy](https://www.psychopy.org/index.html) [@Peirce2019].
If the experiment is to be administered over the Internet,
one might alternatively consider the Javascript package
[jsPsych](https://www.jspsych.org/) [@de2015jspsych].
If the experiment is to incorporate modern psychometric techniques,
such as item response theory and adaptive testing,
the R framework [Concerto](https://concertoplatform.com/about) [@scalise2015use]
might be preferable.
If the experiment is to involve many online participants interacting
in complex networks, the 
[Dallinger framework](http://docs.dallinger.io/en/latest/index.html#)
would arguably be preferable.
Many more such tools exist, each specialised for a particular aspect
of psychological research.

[psychTestR](http://psychtestr.com/) constitutes another tool for developing
and administering psychological experiments.
It occupies a special niche within this software landscape,
one that may be summarised in terms of the following features:

1. **The R programming language.** 
R is a free and open-source language specialised for scientific computing,
well-known for its powerful array of data analysis and visualisation packages.
R has become increasingly popular in psychological research communities,
with many researchers adopting R as their primary programming languages.
psychTestR is ideal for such users,
enabling them to take their existing knowledge of R and apply it to experiment design,
and thereby creating a coherent pipeline for psychological data collection and analysis within R.
2. **Modularisation.**
psychTestR encourages a modular approach to test design,
where code is parcelled up into functions, modules, and packages.
This modularity makes code easier to test, distribute, and reuse.
As a result, psychTestR is particularly well-suited to
large test batteries combining multiple psychological measures,
including for example IQ tests, memory tests, and personality questionnaires.
3. **Internationalisation.**
psychTestR provides a sophisticated internationalisation paradigm
whereby experiments are implemented with reference to a multilingual dictionary.
This makes psychTestR particularly well-suited for developing psychological measures
to be used in different countries.
4. **Local and online deployment.**
psychTestR experiments may be deployed either to a local computer for lab testing
or an online server for large-scale data collection.
The psychTestR server model is designed with scalability in mind, 
using an single-process model that shares memory between sessions
and thereby efficiently scales to hundreds of simultaneous participants.
5. **Automated testing.**
psychTestR exposes powerful tools for automated testing.
Using these tools, the researcher can quickly simulate 
a participant's progression through a given experiment
and thereby validate the robustness of the experiment implementation.
6. **Exposing the power of Shiny.**
psychTestR is built on [Shiny](https://shiny.rstudio.com/), 
a powerful web framework for developing interactive web apps
that is heavily supported by [RStudio](https://rstudio.com/) 
and used by many thousands of developers.
psychTestR users can therefore take advantage of Shiny's many tools
for programmatically generating HTML and developing interactive widgets.
7. **Open source**.
The open-source license gives researchers freedom
to use psychTestR without cost or restriction.
Researchers have full access to the source code,
and are free to customise the software as they see fit.

psychTestR has already been used in various published studies.
Several adaptive musical ability tests have been developed using the software,
including
a [melody discrimination test](https://github.com/pmcharrison/mdt) [@Harrison2017], 
a [beat perception test](https://github.com/pmcharrison/cabat) [@Harrison2018],
a [mistuning perception test](https://github.com/pmcharrison/mpt) [@LarrouyMaestri2019],
and a [pitch imagery test](https://github.com/pmcharrison/piat) [@gelding2019developing].
psychTestR also provides the underlying framework for data collection in 
the [LongGold study](https://longgold.org/),
a longitudinal investigation of intelligence, musical ability, and personal
development throughout adolescence.
We are eager to see how psychTestR will be used in future research.
For more information,
please visit the psychTestR website: http://psychtestr.com.

# Acknowledgments

The author gratefully acknowledges useful feedback from
Daniel Müllensiefen,
Klaus Frieler, Marcus Pearce, and Nicolas Ruth.
He was supported by the EPSRC and AHRC Centre for Doctoral Training
in Media and Arts Technology (EP/L01632X/1)
and by the Humboldt’s foundation Anneliese Maier research prize 
as awarded to Daniel Müllensiefen.

# References
