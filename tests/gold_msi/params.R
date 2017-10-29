title <- "Gold-MSI"
study_id <- NA
pilot <- TRUE

pages <- c(getGoldMSI(sub_factors = "Musical Training",
                      general_factor = FALSE, ask_best_instrument = TRUE),
           new("code_block",
               fun = function(rv, input) {
                 print(rv$results)
               }),
           new("final_page",
                    body = p("You completed the test! Your responses have been recorded. You may now close the browser window.")))