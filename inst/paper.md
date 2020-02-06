---
title: 'psychTestR: An R package for designing and conducting behavioural psychological experiments'
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


Today's psychologists can choose from many different software packages
for developing and administering psychological experiments,
with the most appropriate package typically varying from task to task.
For simple experiments based on short questionnaires,
straightforward tools such as [Survey Monkey](https://www.surveymonkey.co.uk/)
or [Google Forms](https://www.google.co.uk/forms/about/)
can be most appropriate.
For experiments depending on specialist hardware
and accurate response time measurement,
one might instead adopt the Python package
[PsychoPy](https://www.psychopy.org/index.html) [@Peirce2019].
For experiments administered over the Internet,
one could also consider the Javascript package
[jsPsych](https://www.jspsych.org/) [@de2015jspsych].
For experiments involving many online participants interacting
in complex networks, the 
[Dallinger](http://docs.dallinger.io/en/latest/index.html#) framework
will typically be preferable.
Many more such tools exist, each specialised for a particular aspect
of psychological research.

[psychTestR](http://psychtestr.com/) constitutes a new tool for developing
and administering psychological experiments.
It occupies a special niche within this software landscape,
one that may be summarised in terms of the following features:

1. **The R programming language.** 
psychTestR extends R,
a free and open-source programming language specialised for scientific computing,
well-known for its powerful array of data analysis and visualisation packages.
R has become increasingly popular in psychological research communities,
with many researchers adopting R as their primary programming language.
psychTestR is ideal for such users,
enabling them to take their existing knowledge of R and apply it to experiment design,
and thereby creating a coherent pipeline that integrates collection and analysis
of psychological data within the R ecosystem.
2. **Complex experiment design.**
Integrating data collection and data analysis allows psychTestR to 
support complex experiments where stimulus selection is guided by
online analysis of previous response data.
Such experiment implementations can leverage the many statistical packages in the
[Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/),
especially the 
[Psychometric Models & Methods](https://cran.r-project.org/web/views/Psychometrics.html)
and the
[Design of Experiments & Analysis of Experimental Data](https://cran.r-project.org/web/views/ExperimentalDesign.html)
package collections.
This implementation process can be facilitated by psychTestR extensions
such as [psychTestRCAT](http://psychtestrcat.pmcharrison.com/),
a package for constructing computerised adaptive tests using
item response theory [@magis2012random].
3. **Modularisation.**
psychTestR encourages a modular approach to test design,
where code is parcelled up into functions, modules, and packages.
This modularity makes code easier to test, distribute, and reuse.
As a result, psychTestR is particularly well-suited to
large test batteries combining multiple psychological measures,
including for example IQ tests, memory tests, and personality questionnaires.
4. **Internationalisation.**
psychTestR provides a sophisticated internationalisation paradigm
whereby experiments are implemented with reference to a multilingual dictionary.
This makes psychTestR particularly well-suited for developing psychological measures
to be used in different countries.
5. **Local and online deployment.**
psychTestR experiments may be deployed either to a local computer for lab testing
or to a remote server for large-scale online data collection.
The psychTestR server model is designed with scalability in mind, 
using an efficient single-process model that shares memory between sessions
and hence easily scales to hundreds of simultaneous participants.
6. **Automated software testing.**
psychTestR exposes powerful tools for automated software testing.
Using these tools, the researcher can quickly simulate 
a participant's progression through a given experiment
and thereby validate the robustness of the experiment implementation.
7. **Exposing the power of Shiny.**
psychTestR is built on a powerful web development framework called
[Shiny](https://shiny.rstudio.com/).
psychTestR users can easily integrate Shiny web elements into their psychTestR experiments,
thereby taking advantage of the rich array of open-source Shiny utilities
available online.
8. **Open source**.
The open-source license gives researchers freedom
to use psychTestR without cost or restriction.
Researchers have full access to the source code,
and are free to customise the software as they see fit.

psychTestR has already been used in various academic studies.
Several adaptive ability tests have been developed using the software,
including
a [melody discrimination test](https://github.com/pmcharrison/mdt) [@Harrison2017], 
a [beat perception test](https://github.com/pmcharrison/cabat) [@Harrison2018],
a [mistuning perception test](https://github.com/pmcharrison/mpt) [@LarrouyMaestri2019],
a [pitch imagery test](https://github.com/pmcharrison/piat) [@gelding2019developing],
a [visuospatial working memory test](https://github.com/klausfrieler/JAJ) [@frielerJAJ],
and a [rhythmic ability test](https://github.com/klausfrieler/RAT) [@frielerRAT].
psychTestR also provides the underlying framework for data collection in 
the [LongGold study](https://longgold.org/),
a longitudinal investigation of intelligence, musical ability, and personal
development throughout adolescence.
We are eager to see how psychTestR will be used in future research.
For more information,
please visit the psychTestR website, http://psychtestr.com,
which provides 
software download links, introductory articles, tutorials, 
and function-level documentation.

# Acknowledgments

The author gratefully acknowledges useful feedback from
Daniel Müllensiefen,
Klaus Frieler, Marcus Pearce, and Nicolas Ruth.
He was supported by the EPSRC and AHRC Centre for Doctoral Training
in Media and Arts Technology (EP/L01632X/1)
and by the Humboldt foundation's Anneliese Maier research prize 
as awarded to Daniel Müllensiefen.

# References
