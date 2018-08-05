# psychTestR

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

Adaptive melodic discrimination test (Harrison, Collins, & Müllensiefen 2017): 
http://shiny.pmcharrison.com/mdt-demo.

Pitch imagery arrow task (Gelding et al. 2018):
http://shiny.pmcharrison.com/piat-demo.

## Local installation

If you don't have R installed, install it from here: https://cloud.r-project.org/

We recommend also installing RStudio, a popular integrated development environment for R:
https://www.rstudio.com/.

Then open R and install psychTestR:

```r
install.packages("devtools")
devtools::install_github("pmcharrison/psychtestr")
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

Once you've recorded some data, try logging into the admin panel with the password 'demo'.
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
The package is available under the MIT license (https: //opensource.org/licenses/MIT).

## References

Gelding, Rebecca W., Harrison, Peter M. C.,
Johnson, Blake W., Thompson, William F.,
& Müllensiefen, D. (2018).
Developing a psychometrically advanced version of the Pitch Imagery Arrow Task.
Paper presented at the 15th International Conference on Music Perception and Cognition and
10th triennial conference of the European Society for the Cognitive Sciences of Music
(ICMPC15/ESCOM10).

Harrison, P. M. C., Collins, T., & Müllensiefen, D. (2017). 
Applying modern psychometric techniques to melodic discrimination testing: 
Item response theory, computerised adaptive testing, and automatic item generation. 
*Scientific Reports*, 7, 1–18. https://doi.org/10.1038/s41598-017-03586-z
