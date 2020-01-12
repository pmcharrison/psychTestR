# psychTestR

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1415355.svg)](https://doi.org/10.5281/zenodo.1415355)
[![Travis build status](https://travis-ci.org/pmcharrison/psychTestR.svg?branch=master)](https://travis-ci.org/pmcharrison/psychTestR)

[psychTestR](https://pmcharrison.github.io/psychTestR) 
is an R package for creating behavioural experiment interfaces.
Key benefits of psychTestR include:

- Easy installation
- Supports testing on your local computer (laboratory conditions) or on a web server (online recruitment)
- Fully within the R ecosystem, no knowledge of other programming languages required
- Powerful back-end processing that allows you to use all the flexibility of R during test administration
- Scales well to hundreds of concurrent users
- Compatible with all major operating systems (Windows, Mac, Linux)
- Easy to modularise and share your designs with other researchers
- Provides automated testing utilities to ensure that your implementations
are production-ready


## Documentation

We recommend you begin by reading 
[Is psychTestR right for me?](https://pmcharrison.github.io/psychTestR/articles/a-right-for-me),
then psychTestR's 
[introductory article](https://pmcharrison.github.io/psychTestR/articles/b-introduction),
and then exploring the other articles on the 
[documentation website](https://pmcharrison.github.io/psychTestR) 
as and when they become relevant to your queries.
Once you start writing psychTestR code, we 
recommend referring to the 
[function-level documentation](https://pmcharrison.github.io/psychTestR/reference/index.html),
which can also be accessed by running e.g. `?one_button_page` at the R terminal.
It may also be helpful to refer to the source code of pre-existing test implementations
(see [Example tests](https://pmcharrison.github.io/psychTestR/#examples)).

## Examples

- Computerised Adaptive Beat Alignment Test (CA-BAT):
[demo](http://shiny.pmcharrison.com/cabat-demo), 
[source](https://github.com/pmcharrison/cabat),
[paper](https://doi.org/10.1038/s41598-018-30318-8)
- Melodic Discrimination Test (MDT):
[demo](http://shiny.pmcharrison.com/mdt-demo),
[source](https://github.com/pmcharrison/mdt),
[paper](https://doi.org/10.1038/s41598-017-03586-z)
- Mistuning Perception Test (MPT):
[demo](http://shiny.pmcharrison.com/mpt-demo),
[source](https://github.com/pmcharrison/mpt),
[paper](https://doi.org/10.3758/s13428-019-01225-1)
- Pitch imagery arrow task (PIAT):
[demo](http://shiny.pmcharrison.com/piat-demo),
[source](https://github.com/pmcharrison/piat)

## Citation

You can cite psychTestR as follows:

> Harrison, Peter M. C. (2020).
> psychTestR: An R package for designing and
> conducting behavioural psychological experiments.
> *PsyArXiv*. http://doi.org/10.31234/osf.io/dyar7

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

## Local installation

If you don't have R installed, [download](https://cloud.r-project.org/) and install it first. 

We recommend also installing [RStudio](https://www.rstudio.com/),
a popular integrated development environment for R.

Then open R and install psychTestR:

```r
install.packages("devtools")
devtools::install_github("pmcharrison/psychTestR")
```

## Local demo

Once psychTestR is installed on your local computer, 
open RStudio and enter the following to run an example psychTestR test:

```r
library(psychTestR)
make_test(list(
  text_input_page(
    label = "name", 
    prompt = "What's your name?", 
    validate = function(answer, ...) {
      if (answer == "")
        "Name cannot be left blank."
      else TRUE
    },
    on_complete = function(answer, state, ...) {
      set_global(key = "name", value = answer,
                 state = state)
    }),
  NAFC_page(
    label = "colour",
    prompt = "What's your favourite colour?",
    choices = c("Red", "Green", "Blue")),
  elt_save_results_to_disk(complete = TRUE),
  reactive_page(function(state, ...) {
    final_page(paste0("Thank you for participating, ", 
                      get_global("name", state),
                      "."))
  })))
```

Once you've recorded some data, try logging into the admin panel with the password 'demo'
(as specified under the `opt` argument to `make_test()`).
Here you can download your response data.

## Acknowledgements

psychTestR was created by Peter M. C. Harrison
with useful feedback from 
Daniel Müllensiefen, Klaus Frieler, Marcus Pearce, and Nicolas Ruth.
The work was was supported by Peter's PhD studentship from 
the EPSRC and AHRC Centre for Doctoral Training
in Media and Arts Technology (EP/L01632X/1)
and by the Humboldt foundation's Anneliese Maier research prize as awarded to Daniel Müllensiefen.

## License

psychTestR will always be completely free to use for both non-commercial or commercial purposes.
The package is available under the [MIT license](https://opensource.org/licenses/MIT).
