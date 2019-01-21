# psychTestR

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1415355.svg)](https://doi.org/10.5281/zenodo.1415355)

psychTestR is an R package for creating behavioural experiment interfaces.
Key benefits of psychTestR include:

- Easy installation
- Supports testing on your local computer (laboratory conditions) or on a web server (online recruitment)
- Fully within the R ecosystem, no knowledge of other programming languages required
- Powerful back-end processing that allows you to use all the flexibility of R during test administration
- Scales well to hundreds of concurrent users
- Compatible with all major operating systems (Windows, Mac, Linux)
- Easy to modularise and share your designs with other researchers

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
[source](https://github.com/pmcharrison/mpt)
- Pitch imagery arrow task (PIAT):
[demo](http://shiny.pmcharrison.com/piat-demo)

## Status

psychTestR is still under active development.
Our research groups have successfully used psychTestR for several studies,
but we want to address various issues before advertising it for wider use:

- Adding comprehensive documentation and tutorials; 
- Adding more user-friendly type-checking and error catching;
- Improving test coverage.

You are still welcome to use psychTestR before these steps are complete, of course.
Peter (p.m.c.harrison@qmul.ac.uk) is happy to provide advice/technical support,
especially because feedback at this stage can easily be fed into the development process.

## Documentation

The documentation will be improved in the coming months. 
In the meanwhile, there are three main sources:

- Function documentation: available by running e.g. `?one_button_page` at the R terminal
- [Wiki documentation](https://github.com/pmcharrison/psychTestR/wiki)
- [Example tests](https://github.com/pmcharrison/psychTestR#examples)

## Citation

You can cite psychTestR with the permanent DOI link
https://doi.org/10.5281/zenodo.1415355.
We also advise mentioning the particular version you used,
which you can find from R as follows:

``` r
library(psychTestR)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package == "psychTestR", ]
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
Daniel Müllensiefen, Klaus Frieler, and Marcus Pearce.
The work was was supported by Peter's PhD studentship from 
the EPSRC and AHRC Centre for Doctoral Training
in Media and Arts Technology (EP/L01632X/1)
and by the Humboldt’s foundation Anneliese Maier research prize awarded to Daniel Müllensiefen.

## License

psychTestR will always be completely free to use for both non-commercial or commercial purposes.
The package is available under the [MIT license](https://opensource.org/licenses/MIT).
