getMusicalTraining <- function() {
  c(list(new("one_btn_page",
             body = tags$p("We will now ask you some questions about your musical background. You will be presented with some statements: your task is to say how much you agree with them."),
             on_complete = function(rv, input) {
               rv$results$musical_training <- list(detail = data.frame(),
                                           total_score = 0)
             })),
    lapply(list(
      list(q = "I have never been complimented for my talents as a musical performer.",
           opt = c(`Completely Disagree` = "1",
                   `Strongly Disagree` = "2",
                   `Disagree` = "3",
                   `Neither Agree nor Disagree` = "4",
                   `Agree` = "5",
                   `Strongly Agree` = "6",
                   `Completely Agree` = "7"),
           positive = FALSE),
      list(q = "I would not consider myself a musician.",
           opt = c(`Completely Disagree` = "1",
                   `Strongly Disagree` = "2",
                   `Disagree` = "3",
                   `Neither Agree nor Disagree` = "4",
                   `Agree` = "5",
                   `Strongly Agree` = "6",
                   `Completely Agree` = "7"),
           positive = FALSE),
      list(q = "I engaged in regular, daily practice of a musical instrument (including voice) for ___ years.",
           opt = c(`0` = "1",
                   `1` = "2",
                   `2` = "3",
                   `3` = "4",
                   `4 to 5` = "5",
                   `6 to 9` = "6",
                   `10 or more` = "7"),
           positive = TRUE),
      list(q = "At the peak of my interest, I practiced ___ hours per day on my primary instrument.",
           opt = c(`0` = "1",
                   `0.5` = "2",
                   `1` = "3",
                   `1.5` = "4",
                   `2` = "5",
                   `3 to 4` = "6",
                   `5 or more` = "7"),
           positive = TRUE),
      list(q = "I have had formal training in music theory for __ years",
           opt = c(`0` = "1",
                   `0.5` = "2",
                   `1` = "3",
                   `2` = "4",
                   `3` = "5",
                   `4 to 6` = "6",
                   `7 or more` = "7"),
           positive = TRUE),
      list(q = "I have had __ years of formal training on a musical instrument (including voice) during my lifetime.",
           opt = c(`0` = "1",
                   `0.5` = "2",
                   `1` = "3",
                   `2` = "4",
                   `3 to 5` = "5",
                   `6 to 9` = "6",
                   `10 or more` = "7"),
           positive = TRUE),
      list(q = "I can play ___ musical instruments (including voice).",
           opt = c(`0` = "1",
                   `1` = "2",
                   `2` = "3",
                   `3` = "4",
                   `4` = "5",
                   `5` = "6",
                   `6 or more` = "7"),
           positive = TRUE)),
      function(x) {
        new("page_NAFC",
            prompt = tags$p(x$q),
            response_options = x$opt,
            on_complete = function(rv, input) {
              answer <- which(vapply(x$opt, function(y) input[[y]] == 1,
                                     logical(1)))
              answer_num <- as.numeric(x$opt[answer])
              answer_num_weighted <-
                if (x$positive) answer_num else 8 - answer_num
              answer_char <- names(x$opt)[answer]
              rv$results$musical_training$detail <-
                rbind(rv$results$musical_training$detail,
                      data.frame(question = x$q,
                                 positive = x$positive,
                                 answer_char = answer_char,
                                 answer_num = answer_num,
                                 answer_num_weighted = answer_num_weighted))
              rv$results$musical_training$total_score <- 
                rv$results$musical_training$total_score + answer_num_weighted
            })
      }))
}