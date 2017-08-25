getBasicDemographics <- function() {
  list(new("one_btn_page",
           body = tags$p("We will now ask you some questions about your personal background."),
           on_complete = function(rv, input) {
             rv$basic_demographics <- list()
           }),
       new("page_NAFC",
           prompt = tags$p("How do you describe your gender?"),
           response_options = c("Female", "Male", "Other", "Prefer not to answer"),
           on_complete = function(rv, input) {
             rv$basic_demographics$gender <- input$lastBtnPressed
             print(rv$basic_demographics)
           }),
       new("one_btn_page",
           body = tags$div(
             tags$p("What is your age in years?"),
             textInput("age", label = NULL, width = "100px", placeholder = "e.g. 24")),
           check_validity = function(rv, input) {
             if (is.na(as.numeric(input$age)) || input$age < 0) {
               shinyjs::alert("Please ensure that age is entered correctly. Your answer should be numeric.")
               FALSE
             } else TRUE
           },
           on_complete = function(rv, input) {
             rv$basic_demographics$age <- input$age
           }),
       new("page_NAFC",
           prompt = tags$div(tags$p("What is your occupational status?"),
                             tags$p("choose the one most appropriate option)")),
           response_options = c("Still at school",
                                "At university",
                                "In full-time employment",
                                "Self-employed",
                                "Homemaker/full-time parent",
                                "Unemployed",
                                "Retired",
                                "Rather not say"),
           on_complete = function(rv, input) {
             rv$basic_demographics$gender <- input$lastBtnPressed
             print(rv$basic_demographics)
           }))
}