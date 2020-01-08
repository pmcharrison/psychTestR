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

# Summary 

The contemporary psychologist has access to many different software packages
for developing and administering psychological experiments,
each optimised to particular use cases.
For example, if the test is just a simple one-off questionnaire,
straightforward tools such as [Survey Monkey](https://www.surveymonkey.co.uk/)
and [Google Forms](https://www.google.co.uk/forms/about/)
may well be sufficient.
If the test depends on specialist lab hardware
and requires highly accurate response time measurement,
then one might use [PsychoPy](https://www.psychopy.org/index.html),
a powerful cross-platform package written in Python.
The Javascript package [jsPsych](https://www.jspsych.org/) is 
a strong alternative to PsychoPy that is targeted instead at 
web data collection.
If the test depends heavily on modern psychometric techniques,
such as item response theory and adaptive testing,
the R-based framework [Concerto](https://concertoplatform.com/about)
may be preferable.
If the test requires many online participants to interact
in complex networks, the 
[Dallinger framework](http://docs.dallinger.io/en/latest/index.html#)
is arguably the best option.
These are just a few of the many packages available for psychological testing.

*psychTestR* occupies a special niche within this landscape,
which may be summarised in terms of the following key features:

1. **The R programming language.**
2. **Modularisation, combination, dissemination.**
3. **Internationalisation.**
4. **Online deployment.**
5. **Automated testing.**
6. **Exposing the power of Shiny.**
7. **Open source**.

1. *psychTestR enables users to design entire psychological experiments
within the R programming language*.
2. *psychTestR is well-suited for creating and distributing modular psychological tests
(e.g. IQ tests, memory tests, personality tests).*

# Acknowledgments

The author gratefully acknowledges useful feedback from
Daniel Müllensiefen,
Klaus Frieler, Marcus Pearce, and Nicolas Ruth.
He was supported by the EPSRC and AHRC Centre for Doctoral Training
in Media and Arts Technology (EP/L01632X/1)
and by the Humboldt’s foundation Anneliese Maier research prize 
as awarded to Daniel Müllensiefen.

# References
