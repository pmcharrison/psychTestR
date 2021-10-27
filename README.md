# psychTestR

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.02088/status.svg)](https://doi.org/10.21105/joss.02088)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1415355.svg)](https://doi.org/10.5281/zenodo.1415355)
[![R-CMD-check](https://github.com/pmcharrison/psychTestR/workflows/R-CMD-check/badge.svg)](https://github.com/pmcharrison/psychTestR/actions)

[psychTestR](https://pmcharrison.github.io/psychTestR) 
is an R package for creating behavioural experiment interfaces.
For an overview of psychTestR's features, 
see its [overview paper](https://doi.org/10.21105/joss.02088)
in the Journal of Open Source Software.
For a more detailed introduction, see the
[introductory article](https://pmcharrison.github.io/psychTestR/articles/b-introduction)
on the psychTestR website.
For more documentation on designing and deploying psychTestR tests,
see the other vignettes and the
[function-level documentation](https://pmcharrison.github.io/psychTestR/reference/index.html).
For documentation on setting up a self-hosted psychTestR server, see 
[Guide to creating a server for online *R* experiments using *psychTestR*](https://s3-eu-west-1.amazonaws.com/research.pmcharrison.com/psychTestR/psychTestR-server-docs-latest.pdf)
by [Anthony Chmiel](https://www.westernsydney.edu.au/marcs/our_team/researchers/dr_anthony_chmiel).

## Installation

If you don't have R installed, [download](https://cloud.r-project.org/) and install it first. 

We recommend also installing [RStudio](https://www.rstudio.com/),
a popular integrated development environment for R.

Then open R and install psychTestR:

```r
install.packages("devtools")
devtools::install_github("pmcharrison/psychTestR")
```

For a quick test of psychTestR functionality, visit 
[Simple demo](https://pmcharrison.github.io/psychTestR/articles/a1-simple-demo.html).

## Citation

You can cite psychTestR as follows:

> Harrison, Peter M. C. (2020).
> psychTestR: An R package for designing and
> conducting behavioural psychological experiments.
> *Journal of Open Source Software*. https://doi.org/10.21105/joss.02088

The psychTestR source code is also permanently archived
on Zenodo. You can find this permanent archive at the following DOI:
http://doi.org/10.5281/zenodo.1415355.
This DOI will always point to the latest release of 
the `psychTestR` package,
but you can also find version-specific DOIs on the Zenodo page.

You might also want to mention the particular version of the package you used.
To check the installed version, you can run the following code in your R console:

``` r
asNamespace("psychTestR")$`.__NAMESPACE__.`$spec[["version"]]
```

## Community guidelines

The [psychTestR documentation website](https://pmcharrison.github.io/psychTestR/)
answers many common questions about psychTestR usage.
Further questions are welcomed via the 
[GitHub issue tracker](https://github.com/pmcharrison/psychTestR/issues).
This issue tracker is also the place to report issues or software problems.
We welcome software contributions in the form of 
[GitHub pull requests](https://github.com/pmcharrison/psychTestR/pulls);
if you are thinking of making a contribution, consider first beginning
a discussion on the issue tracker so that we can discuss implementation details.


