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
      list(q = "At the peak of my interest, I practised ___ hours per day on my primary instrument.",
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

getGoldMSI <- function(sub_factors = "All", general_factor = FALSE,
                       ask_best_instrument = FALSE) {
  possible_sub_factors <- c("Active Engagement", "Emotions", "Musical Training",
                            "Perceptual Abilities", "Singing Abilities")
  csv <- read.csv("tests/gold_msi/items.csv", stringsAsFactors = FALSE)
  
  assertthat::assert_that(
    is.character(sub_factors),
    all(sub_factors %in% c(possible_sub_factors, "All")),
    is.logical(general_factor),
    assertthat::is.scalar(general_factor),
    is.logical(ask_best_instrument), assertthat::is.scalar(ask_best_instrument),
    all(possible_sub_factors %in% csv$factor_label),
    all(csv$factor_label %in% possible_sub_factors)
  )
  
  if ("All" %in% sub_factors) sub_factors <- possible_sub_factors
  
  items <- csv[csv$factor_label %in% sub_factors |
                 (general_factor & csv$general_factor), ] %>%
    cbind(., response = NA)
  
  assertthat::assert_that(
    nrow(items) > 0
  )
  
  prep_pages <- new("one_btn_page",
                    body = tags$p("We will now ask you some questions about your musical background. You will be presented with some statements: your task is to say how much you agree with them."),
                    on_complete = function(rv, input) {
                      rv$results$gold_msi <- list(detail = items)
                    })
  
  item_pages <- mapply(
    function(position, question, 
             btn1_text, btn2_text, btn3_text, btn4_text,
             btn5_text, btn6_text, btn7_text) {
      assertthat::assert_that(
        is.numeric(position),
        is.character(question),
        assertthat::is.scalar(question),
        is.character(btn1_text), assertthat::is.scalar(btn1_text),
        is.character(btn2_text), assertthat::is.scalar(btn2_text),
        is.character(btn3_text), assertthat::is.scalar(btn3_text),
        is.character(btn4_text), assertthat::is.scalar(btn4_text),
        is.character(btn5_text), assertthat::is.scalar(btn5_text),
        is.character(btn6_text), assertthat::is.scalar(btn6_text),
        is.character(btn7_text), assertthat::is.scalar(btn7_text)
      )
      new("page_NAFC",
          prompt = tags$p(question),
          response_options = as.character(1:7) %>% (function(x) {
            names(x) <- c(btn1_text, btn2_text, btn3_text, btn4_text,
                          btn5_text, btn6_text, btn7_text)
            x
          }),
          on_complete = function(rv, input) {
            rv$results$gold_msi$detail$response[position] <- 
              as.numeric(input$lastBtnPressed)
          })
    },
    seq_len(nrow(items)),
    items$question,
    items$btn1_text, items$btn2_text, items$btn3_text, items$btn4_text,
    items$btn5_text, items$btn6_text, items$btn7_text
  )
  
  best_instrument_page <- 
    if (ask_best_instrument) {
      new(
        "page_dropdown",
        prompt = tags$p("The instrument I play best (including voice) is ____"),
        options = c("NA", "voice", "piano", "guitar", "drums", "xylophone",
                    "flute", "oboe", "clarinet", "bassoon", "trumpet",
                    "trombone", "tuba", "saxophone", "horn", "violin",
                    "cello", "alto", "double bass", "harp") %>%
          sapply(., Hmisc::capitalize, USE.NAMES = FALSE),
        other_please_state = TRUE,
        max_pixel_width = 200,
        on_complete = function(rv, input) {
          rv$results$gold_msi$best_instrument <- 
            if (input$dropdown == "Other (please state)") {
              input$other_please_state
            } else {
              input$dropdown
            }
        }
      )
    } else NULL
  
  final_pages <- new("one_btn_page",
                     body = tags$p("You completed the musical background questionaire."),
                     on_complete = function(rv, input) {
                       rv$results$gold_msi$scores <- 
                         scoreGoldMSI(rv$results$gold_msi$detail,
                                      sub_factors = sub_factors,
                                      possible_sub_factors = possible_sub_factors,
                                      general_factor = general_factor)
                     })
  c(prep_pages, item_pages, best_instrument_page, final_pages)
}

scoreGoldMSI <- function(detail, sub_factors,
                         possible_sub_factors,
                         general_factor) {
  assertthat::assert_that(
    is.data.frame(detail),
    is.character(sub_factors),
    all(sub_factors %in% possible_sub_factors),
    all(detail$factor_label %in% possible_sub_factors),
    is.logical(general_factor), assertthat::is.scalar(general_factor)
  )
  sumScores <- function(df) {
    ifelse(df$positive_scoring,
           df$response,
           8 - df$response) %>%
      sum
  }
  res <- list()
  for (sub_factor in sub_factors) {
    res[[sub_factor]] <- 
      detail[detail$factor_label == sub_factor, ] %>%
      sumScores
  }
  if (general_factor) {
    res$general <- 
      detail[detail$general_factor, ] %>%
      sumScores
  }
  res
}
