title <- "My title"

pages <- 
  withTags(
    list(new("one_btn_page", p("Info text 1")),
         new("one_btn_page", p("Info text 2")),
         new("one_btn_page", p("Info text 3"))))

# pages$p_id <- new("app_page",
#                   ui = fluidPage(
#                     div(class = "centre_screen", align = "center",
#                         textInput("p_id", "Participant ID", placeholder = "e.g. ph93"),
#                         actionButton("next_page", "Submit"))),
#                   result = "p_id")
# 
# pages$session_type <- new("app_page",
#                           ui = fluidPage(
#                             div(class = "centre_screen", align = "center",
#                                 radioButtons("radio", label = "Session type",
#                                              choices = list("Test" = "test",
#                                                             "Pilot" = "pilot",
#                                                             "Live" = "live"),
#                                              selected = "test"),
#                                 actionButton("next_page", "Submit"))),
#                           result = "session_type")
# 
# pages$session_complete <- 
#   new("app_page",
#       ui = fluidPage(div(class = "centre_screen", align = "center",
#                          "Session complete! You may now close the browser window.")),
#       result = "session_type",
#       final = TRUE)
