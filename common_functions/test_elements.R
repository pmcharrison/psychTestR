actionButtonTrigger <- function(inputId, label, icon = NULL, width = NULL, ...) {
  # Version of actionButton that also triggers the next page
  actionButton(inputId = inputId, label = label,
               icon = icon, width = width,
               onclick = 'Shiny.onInputChange("nextPage", performance.now());',
               ...)
}

setOldClass("shiny.tag")
setOldClass("shiny.tag.list")

setClass("test_element")

setClass("page",
         slots = list(ui = "shiny.tag", # page UI
                      final = "logical", # whether page is final page or not
                      on_complete = "function"), # function(rv, input) to run on completion
         prototype = list(ui = div(),
                          final = FALSE,
                          on_complete = function(rv, input) NULL),
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
                      wait = "logical"),
         contains = "page")
setMethod(
  f = "initialize",
  signature = "video_stimulus_NAFC",
  definition = function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@final <- FALSE
    
    video_ui <- tags$video(tags$source(src = .Object@source,
                                       type = paste0("video/", .Object@type)),
                           id = "video_stimulus",
                           width = "50%",
                           autoplay = "autoplay",
                           onended = if (.Object@wait) {
                             "document.getElementById('response_UI').style.visibility = 'visible';"
                             } else "null")
    response_ui <- make_ui_NAFC(.Object@response_options, hidden = .Object@wait)
    
    .Object@ui <- div(.Object@prompt, video_ui, response_ui)
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
        style = if (hidden) "visibility: hidden" else "visibility: visible",
        mapply(function(id, label) {
          tags$p(actionButtonTrigger(inputId = id, label = label))
        }, response_options, labels, SIMPLIFY = F, USE.NAMES = F)))
}
