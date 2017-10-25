library(catR)
library(shiny)

actionButtonTrigger <- function(inputId, label, icon = NULL, width = NULL, ...) {
  # Version of actionButton that also triggers the next page
  actionButton(
    inputId = inputId, label = label,
    icon = icon, width = width,
    onclick = paste(
      sprintf('Shiny.onInputChange("lastBtnPressed", "%s");',
              inputId),
      "document.getElementById('current_page.ui').style.visibility = 'hidden';",
      'setTimeout(function() {Shiny.onInputChange("nextPage", performance.now());}, 500);'),
    ...)
}

setOldClass("shiny.tag")
setOldClass("shiny.tag.list")

setClass("test_element")

setClass("page",
         slots = list(ui = "shiny.tag", # page UI
                      final = "logical", # whether page is final page or not
                      on_complete = "function", # function(rv, input) to run on completion
                      validate = "function"), # function(rv, input) to check whether input is valid and progress allowed
         prototype = list(ui = div(),
                          final = FALSE,
                          on_complete = function(rv, input) NULL,
                          validate = function(rv, input) TRUE),
         contains = "test_element")

# one_btn_page shows a page with some content and 
# a 'Next' button.
setClass("one_btn_page",
         slots = list(body = "shiny.tag"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "one_btn_page",
  definition = function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@ui <- div(.Object@body,
                      actionButtonTrigger("next", "Next"))
    return(.Object)
  }
)

# final_page shows a page with some content and no action buttons
setClass("final_page",
         slots = list(body = "shiny.tag"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "final_page",
  definition = function(.Object, body) {
    .Object@body <- body
    .Object@ui <- div(body)
    .Object@final <- TRUE
    return(.Object)
  }
)

# video_stimulus shows some content (typically text) with a video
# below it and a variable number of forced-choice response options
setClass("video_stimulus_NAFC",
         slots = list(prompt = "shiny.tag",
                      source = "character",
                      type = "character",
                      response_options = "character",
                      wait = "logical",
                      mobile_enabled = "logical"),
         contains = "page",
         prototype = list(wait = TRUE,
                          mobile_enabled = TRUE))
setMethod(
  f = "initialize",
  signature = "video_stimulus_NAFC",
  definition = function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@final <- FALSE
    
    cmd_play_video <- "document.getElementById('video_stimulus').play();"
    cmd_show_video_btn <- "document.getElementById('btn_play_video').style.visibility='inherit';"
    cmd_hide_video_btn <- "document.getElementById('btn_play_video').style.visibility='hidden';"
    
    video_ui <- tags$div(
      tags$video(
        tags$source(
          src = .Object@source,
          type = paste0("video/", .Object@type)),
        id = "video_stimulus",
        width = "50%",
        preload = "auto",
        oncanplaythrough = cmd_show_video_btn,
        autoplay = "autoplay",
        onplay = cmd_hide_video_btn,
        onended = if (.Object@wait) {
          "document.getElementById('response_UI').style.visibility = 'inherit';"
        } else "null"),
      if (.Object@mobile_enabled) {
        tags$p(tags$strong("Click here to play video"),
               id = "btn_play_video",
               style = "visibility: hidden",
               onclick = cmd_play_video)
      } else NULL
    )
    response_ui <- make_ui_NAFC(.Object@response_options, hidden = .Object@wait)
    
    .Object@ui <- div(.Object@prompt, video_ui, response_ui)
    return(.Object)
  }
)

# audio_stimulus shows some content (typically text) with a audio
# below it and a variable number of forced-choice response options
setClass("audio_stimulus_NAFC",
         slots = list(prompt = "shiny.tag",
                      source = "character",
                      type = "character",
                      response_options = "character",
                      wait = "logical"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "audio_stimulus_NAFC",
  definition = function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@final <- FALSE
    
    audio_ui <- tags$div(
      tags$audio(
        tags$source(
          src = .Object@source,
          type = paste0("audio/", .Object@type)),
        id = "audio_stimulus",
        width = "50%",
        preload = "auto",
        autoplay = "autoplay",
        onended = if (.Object@wait) {
          "document.getElementById('response_UI').style.visibility = 'inherit';"
        } else "null"))
    response_ui <- make_ui_NAFC(.Object@response_options, hidden = .Object@wait)
    
    .Object@ui <- div(.Object@prompt, audio_ui, response_ui)
    return(.Object)
  }
)

# volume_calibration is a subclass of one_btn_page that autoplays looping audio
# in the background.
setClass("volume_calibration",
         slots = list(prompt = "shiny.tag",
                      source = "character",
                      type = "character",
                      mobile_enabled = "logical"),
         contains = "page",
         prototype = list(mobile_enabled = TRUE))
setMethod(
  f = "initialize",
  signature = "volume_calibration",
  definition = function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    cmd_play <- "document.getElementById('volume_calibration_audio').play();"
    cmd_show_play_btn <- 
      "if (!audio_played) {document.getElementById('btn_play').style.visibility='inherit'};"
    cmd_hide_play_btn <- "document.getElementById('btn_play').style.visibility='hidden';"
    audio_ui <- tags$div(
      tags$head(
        tags$script(HTML("var audio_played = false;"))
      ),
      tags$audio(
        tags$source(
          src = list(...)$source,
          type = paste0("audio/", list(...)$type)),
        id = "volume_calibration_audio",
        preload = "auto",
        oncanplaythrough = cmd_show_play_btn,
        onplay = paste0("audio_played = true;", cmd_hide_play_btn),
        autoplay = "autoplay",
        loop = "loop"),
      if (.Object@mobile_enabled) {
        tags$p(tags$strong("Click here to play audio"),
               id = "btn_play",
               style = "visibility: hidden",
               onclick = cmd_play)
      } else NULL
    )
    ui <- tags$div(audio_ui, list(...)$prompt, actionButtonTrigger("next", "Next"))
    .Object@ui <- ui
    return(.Object)
  }
)

setClass("page_NAFC",
         slots = list(prompt = "shiny.tag",
                      response_options = "character"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "page_NAFC",
  definition = function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@final <- FALSE
    
    response_ui <- make_ui_NAFC(.Object@response_options, hidden = FALSE)
    
    .Object@ui <- div(.Object@prompt, response_ui)
    return(.Object)
  }
)


setClass("code_block",
         slots = list(fun = "function"),
         contains = "test_element",
         prototype = list(fun = function(rv, input) NULL))

make_ui_NAFC <- function(response_options, hidden = FALSE) {
  assertthat::assert_that(is.character(response_options), is.logical(hidden))
  labels <- if (is.null(names(response_options))) {
    response_options
  } else {
    names(response_options)
  }
  withTags(
    div(id = "response_UI",
        style = if (hidden) "visibility: hidden" else "visibility: inherit",
        mapply(function(id, label) {
          tags$p(actionButtonTrigger(inputId = id, label = label))
        }, response_options, labels, SIMPLIFY = F, USE.NAMES = F)))
}

setClass("AudioCATParams",
         # Defines the mutable part of an adaptive test; is stored in the rv$params object.
         slots = list(
           audio_root = "character",
           test_length = "numeric",
           next_item.criterion = "character",
           next_item.estimator = "character",
           results.by_item = "data.frame",
           results.final = "list"
         ),
         prototype = list(
           audio_root = NULL,
           test_length = NULL,
           next_item.criterion = "bOpt",
           next_item.estimator = "BM",
           results.by_item = data.frame(),
           results.final = list()
         ))

setClass("AudioCAT",
         # Defines the immutable part of an adaptive test; is a test element.
         slots = list(
           itemPar = "matrix",
           audio_paths = "character",
           audio_type = "character",
           response_options = "character",
           answers = "character",
           cbControl = "list",
           cbGroup = "character",
           intro = "list",
           params_id = "character",
           item.prompt = "shiny.tag"),
         contains = "code_block")

setMethod(
  f = "initialize",
  signature = "AudioCAT",
  definition = function(.Object, itemPar, audio_paths, audio_type, response_options, answers,
                        cbControl, cbGroup, intro, params_id, item.prompt) {
    .Object <- callNextMethod(.Object, itemPar = itemPar, audio_paths = audio_paths,
                              audio_type = audio_type, response_options = response_options,
                              answers = answers,
                              cbControl = cbControl, cbGroup = cbGroup, intro = intro,
                              params_id = params_id, item.prompt = item.prompt)
    .Object@fun <- function(rv, input) {
      params_id <- .Object@params_id
      item_logic <- function() {
        new("code_block", fun = function(rv, input) {
          results.by_item <- rv$params[[params_id]]@results.by_item
          test_length <- rv$params[[params_id]]@test_length
          if (nrow(results.by_item) < test_length) {
            next_item <- nextItem(
              itemBank = .Object@itemPar,
              theta = if (nrow(results.by_item) == 0) 0 else {
                results.by_item[nrow(results.by_item),
                                paste0("theta_", rv$params[[params_id]]@next_item.estimator)]
              },
              out = if (nrow(results.by_item) == 0) NULL else results.by_item[, "item_id"],
              x = if (nrow(results.by_item) == 0) NULL else results.by_item[, "score"],
              criterion = rv$params[[params_id]]@next_item.criterion,
              method = rv$params[[params_id]]@next_item.estimator,
              cbControl = .Object@cbControl,
              cbGroup = .Object@cbGroup
            )
            new_row <- data.frame(
              num = nrow(results.by_item) + 1,
              item_id = next_item$item,
              discrimination = next_item$par[["discrimination"]],
              difficulty = next_item$par[["difficulty"]],
              guessing = next_item$par[["guessing"]],
              inattention = next_item$par[["inattention"]],
              information = next_item$info,
              criterion = next_item$criterion,
              response = NA, correct_answer = NA, score = NA,
              theta_ML = NA, theta_ML_sem = NA,
              theta_BM = NA, theta_BM_sem = NA,
              theta_EAP = NA, theta_EAP_sem = NA,
              theta_WL = NA, theta_WL_sem = NA
            )
            rv$params[[params_id]]@results.by_item <- plyr::rbind.fill(results.by_item, new_row)
            pushToTestStack(
              new("audio_stimulus_NAFC",
                  prompt = .Object@item.prompt,
                  source = file.path(rv$params[[params_id]]@audio_root,
                                     .Object@audio_paths[next_item$item]),
                  type = .Object@audio_type,
                  response_options = .Object@response_options,
                  wait = TRUE,
                  on_complete = function(rv, input) {
                    response <- input$lastBtnPressed
                    correct_answer <- .Object@answers[next_item$item]
                    score <- response == correct_answer
                    scores <- c(rv$params[[params_id]]@results.by_item$score, score)
                    item_ids <- c(rv$params[[params_id]]@results.by_item$item_id, next_item$item)
                    item_params <- .Object@itemPar[item_ids, , drop = FALSE]
                    n <- nrow(rv$params[[params_id]]@results.by_item)
                    rv$params[[params_id]]@results.by_item$response[n] <-
                      response
                    rv$params[[params_id]]@results.by_item$correct_answer[n] <-
                      correct_answer
                    rv$params[[params_id]]@results.by_item$score[n] <-
                      score
                    for (method in c("ML", "BM", "EAP", "WL")) {
                      tmp_theta <- thetaEst(item_params, scores, method = method)
                      tmp_sem_theta <- semTheta(thEst = tmp_theta,
                                                it = item_params, method = method)
                      
                      rv$params[[params_id]]@results.by_item[n, paste0("theta_", method)] <- 
                        tmp_theta
                      rv$params[[params_id]]@results.by_item[n, paste0("theta_", method, "_sem")] <- 
                        tmp_sem_theta
                    }
                    pushToTestStack(item_logic(), rv)
                  }),
              rv
            )}})}
      pushToTestStack(item_logic(), rv)}
    return(.Object)
  })
