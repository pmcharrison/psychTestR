#' @export
get_basic_demographics <- function() {
  c(
    new_section("demographics"),
    one_button_page("We will now ask you some questions about your personal background."),
    get_basic_demographics.gender(),
    get_basic_demographics.age(),
    get_basic_demographics.occupation(),
    get_basic_demographics.education_highest_achieved()
  )
}

get_basic_demographics.gender <- function() {
  NAFC_page("gender", prompt = "How do you describe your gender?",
            choices = c("Female", "Male", "Other", "Prefer not to answer"))
}

get_basic_demographics.age <- function() {
  text_input_page("age", prompt = "What is your age in years?",
                  width = "100px",
                  validate = function(state, ...) {
                    x <- answer(state)
                    if (is.na(suppressWarnings(as.numeric(x))) ||
                        x < 0) {
                      paste0("Please ensure that age is entered correctly. ",
                             "Your answer should be numeric.")
                    } else TRUE
                  })
}

get_basic_demographics.occupation <- function() {
  NAFC_page("occupation",
            prompt = shiny::div(shiny::p("What is your occupational status?"),
                                shiny::p("(choose the one most appropriate option)")),
            choices = c("Still at school",
                        "At university",
                        "In full- or part-time employment",
                        "Self-employed",
                        "Homemaker/full-time parent",
                        "Unemployed",
                        "Retired",
                        "Rather not say"))
}

get_basic_demographics.education_highest_achieved <- function() {
  NAFC_page(
    "education_highest_achieved",
    prompt = shiny::div(
      shiny::p("What is the highest educational qualification that you have attained?"),
      shiny::p("(choose the one most appropriate option)")),
    choices = c(
      "Postgraduate degree",
      "Undergraduate degree or professional qualification",
      "Completed second qualification (e.g. A levels/High School)",
      "Completed first school qualification at about 16 years (e.g. GCSE/Junior High School)",
      "Did not complete any school qualification",
      "Rather not say"))
}
