#' Get basic demographics
#'
#' Gets basic demographic information from the participant.
#' @export
#' @param intro Introductory page to the demographics section.
#' If \code{NULL}, no introduction is given.
#' @param gender Ask the participant's gender? (Boolean)
#' @param age Ask the participant's age? (Boolean)
#' @param occupation Ask the participant's occupation? (Boolean)
#' @param education Ask the participant's education? (Boolean)
get_basic_demographics <- function(intro = basic_demographics_default_intro(),
                                   gender = TRUE,
                                   age = TRUE,
                                   occupation = TRUE,
                                   education = TRUE) {
  stopifnot(is.null(intro) || is(intro, "page"))
  c(
    begin_module("demographics"),
    intro,
    if (gender) get_basic_demographics.gender(),
    if (age) get_basic_demographics.age(),
    if (occupation) get_basic_demographics.occupation(),
    if (education) get_basic_demographics.education_highest_achieved(),
    end_module()
  )
}

#' Default demographics introduction
#'
#' Default introduction to the demographics section.
#' @export
basic_demographics_default_intro <- function() {
  one_button_page("We will now ask you some questions about your personal background.")
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
    "education_qualification_highest",
    prompt = shiny::div(
      shiny::p("What is the highest educational qualification that you have completed?"),
      shiny::p("(choose the one most appropriate option)")),
    choices = c(
      `Postgraduate degree` = "postgraduate",
      `Undergraduate degree or professional qualification` = "undergraduate",
      `Completed second qualification (e.g. A levels/High School)` = "senior_school",
      `Completed first school qualification at about 16 years (e.g. GCSE/Junior High School)` = "middle_school",
      `Did not complete any school qualification` = "none",
      `Rather not say` = "rather_not_say"))
}
